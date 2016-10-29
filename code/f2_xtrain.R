# based of min_obs_wide_study.R
# old: minimal observation with wide dataset
# old: Try a wide dataset with all the failure observations and N:1 random pass:fail observations
# new: repeat using unused pass data
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(xgboost)
library(ROCR)
library(recommenderlab)

source('bosch_plp_util.R')

if(! exists("pass_fail_ratio")) pass_fail_ratio <- 50
if(! exists("ichunk")) ichunk <- 1
if(! exists("input_csv")) input_csv <- '../input/train_numeric.csv'
if(! exists("seed"))seed <- 1912

#source("f2_family_data_prep.R")
trnw.f2 <- readRDS(file='../data/tmp_trnw_f2_wcat.rds')
family_pf_ratio <- nrow(trnw.f2) / sum(trnw.f2$Response)

n_oos <- floor( family_pf_ratio / pass_fail_ratio)  
if(! exists("start_oos")) start_oos <- 1    #assume first model is build and stored as .rds somewhere?

trnw <- trnw.f2

# number of na's os a feature (might be the only feature for some categorical observations)
trnw$na_count <- apply(trnw, 1, function(x) sum(is.na(x))) 
setkey(trnw, Id)

trnl_date <- data.table()
for (i in 1:10) {
    trnl_date <- rbind(
        trnl_date,
        readRDS(file = sprintf("../data/train_date_long_chunk_%d.rds", i)) ) # see 'load_date_long.R'
    setkey(trnl_date, Id)
    trnl_date <- trnl_date[ Id %in% trnw$Id ] ; gc()
}
tcheck( desc = 'loaded date dataset (long)')

## stolen from date eda
id_cnt <- trnl_date[
    , station := str_extract(metric, "S\\d+")][               #extract station ID
        , .(station_metric_count=.N,
            time_in = min(value), time_out = max(value)), by=c("Id", "station")][     #rollup by station
                , .(station_count = .N, metric_count = sum(station_metric_count),
                    min_time = min(time_in), max_time = max(time_out)), by=Id ][  #rollup by Id
                        , proc_time := max_time - min_time]
rm(trnl_date); gc()
setkey(id_cnt, Id)

trnw <- trnw[ id_cnt, nomatch=FALSE]
rm(id_cnt); gc()

# Add Faron's magic
trnw <- add_magic(trnw)

# reduce_cols <- imp_avg %>% arrange(desc(avg_gain)) %>% head(25) %>% .[[1]]
# reduce cols from _nmc50 -> _nmc50r1
reduce_cols <- c("magic4", "L3_S32_F3854", "magic3", "L3_S33_F3865", "proc_time",
"L0_S12_F350", "L3_S30_F3704", "min_time", "max_time", "L3_S30_F3759", "L3_S33_F3857",
"L3_S33_F3855", "L0_S12_F346", "L0_S13_F354", "L0_S13_F356",  "L3_S30_F3744", 
"L3_S36_F3920", "L3_S33_F3859", "L0_S21_F497", "L0_S12_F330", "L3_S30_F3494",
"L2_S27_F3166", "L0_S18_F439", "L0_S15_F397", "L3_S29_F3339")
trnw <- trnw[, c('Id', 'Response', reduce_cols), with=F ]
trnw <- trnw[ trnw.f2[, c('Id', 'L3_S34_F3882'), with=F]] #setkey(...,Id)

# remove duplicates
chk_cols <- setdiff( names(trnw), c("Id", names(trnw)[grepl("magic", names(trnw))]))
dup_rows <- duplicated(data.frame(trnw)[ ,chk_cols])
trnw <- trnw[! dup_rows]

nfolds <- 5

#create folds from sequential data
ord_time <- order(trnw$min_time, trnw$Id)
setkey(trnw, min_time)
fold_size <- round((nrow(trnw) +  nfolds - 1) / nfolds)
#trnw$kfold <- rep(1:nfolds, each=fold_size)[1:nrow(trnw)]
trnw$kfold <- rep(1:nfolds, fold_size)[1:nrow(trnw)]

# build a hold out data set -- consistent for each round
pass_fail_train <- nrow(trnw) / sum(trnw$Response)

oos_chunk_results <- list()
family_results <- data.table()
trnw.keep <- trnw
for(k in 1:nfolds) {
    trnw <- trnw.keep
    trnw <- add_cv_feature(trnw, exclude_fold = k)
    trn_cols <- setdiff( names(trnw), c("Id", "Response", "kfold"))
    k_val <- trnw[ kfold == k ]
    kval_xgb <- xgb.DMatrix( dropNA(as.matrix(k_val[, .SD, .SDcols = trn_cols])), label = k_val$Response, missing = 9999999 )
    
    #build pool for training data
    ix_trn_fail <- which(trnw[ k != kfold, Response] == '1')
    ix_pass_pool <- which(trnw[ k != kfold, Response] == '0')
    n_trn_pass <- length(ix_trn_fail) * pass_fail_ratio  # we'll sample this many passes each round
    
    fold_results <- k_val[, .(Id, Response)]
    
    for (ioos in start_oos:n_oos) {
        
        #shrink the number of passes to choose from
        ix_trn_pass <- sample( ix_pass_pool, min(n_trn_pass, length(ix_pass_pool)) )
        ix_pass_pool <- setdiff(ix_pass_pool, ix_trn_pass)
        
        k_trn <- trnw[ k != kfold ][c(ix_trn_fail, ix_trn_pass) ] %>% sample_frac()
        
        xgb_params <- list( 
            eta = 0.1,      #
            #     max_depth = 6,   # 
            #     gamma = 0.5,     # 
            #     min_child_weight = 5, #
            #     subsample = 0.5,
            #     colsample_bytree = 0.5, 
            eval_metric = "logloss", #mlogloss",  #map@3",
            objective = "binary:logistic",
            nthreads = 4
        )
        xgb_nrounds = 500
        
        xgb_trn <- xgb.DMatrix( dropNA(as.matrix(k_trn[, .SD, .SDcols = trn_cols])), label = k_trn$Response, missing = 99 )
        model <- xgb.train( params = xgb_params, 
                            data=xgb_trn,
                            nrounds = xgb_nrounds,
                            watchlist = list(eval = kval_xgb),
                            print.every.n = 5L,
                            early.stop.round = 10L,
                            verbose = 1 )
        probs <- predict( model, dropNA(as.matrix(k_val[, .SD, .SDcols = trn_cols]) ) )
        preds <- prediction( probs, k_val$Response )
        auc_val <- performance(preds, "auc")@y.values[[1]]
        perf <- performance(preds, "tpr", "fpr")
        plot(perf)
        mcc <- performance( preds, "mat")
        mcc_vals <- unlist( attr(mcc, "y.values"))
        mcc_cuts <- unlist( attr(mcc, "x.values"))
        cutoff_mcc <- mcc_cuts[ which.max(mcc_vals)]
        table( ifelse(probs > cutoff_mcc, 1, 0), k_val$Response)
        plot(mcc_cuts, mcc_vals, type='l')
        abline(v=cutoff_mcc)
        title(sprintf("MMC versus cutoff for fold %d train set %d", k, ioos))
        mcc_best <- max(mcc_vals, na.rm=TRUE )
        cat( sprintf( "max MCC @ %4.2f = %f, AUC = %f\n", cutoff_mcc, mcc_best, auc_val ))
        cutoff_ratio <- find_cutoff_by_ratio( probs, 1/pass_fail_train)
        table( ifelse(probs > cutoff_ratio, 1, 0), k_val$Response)
        abline(v=cutoff_ratio, lty=2 )
        
        pred_mcc <- ifelse( probs >= cutoff_mcc, 1, 0)
        pred_ratio <- ifelse( probs >= cutoff_ratio, 1, 0)
        mcc_mcc <- calc_mcc( with(fold_results, table(Response, pred_mcc)) ) # same as: max(mcc_vals, na.rm = T)
        mcc_ratio <- calc_mcc( with(fold_results, table(Response, pred_ratio)) )
        
        fold_results[, (paste0('prob_', ioos)) := probs ]
        fold_results[, (paste0('mcc_best_', ioos)) := pred_mcc ]
        fold_results[, (paste0('mcc_ratio_', ioos)) := pred_ratio ]
        
        xgb_imp <- xgb.importance( trn_cols, model=model )
        xgb_plot <- xgb_imp %>% arrange(desc(Gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,Gain), Gain)) + geom_bar(stat="identity", position='identity') + coord_flip()
        
        chunk_results <- list( model=ioos, MCC=mcc_best, cutoff=cutoff_mcc, AUC=auc_val, 
                               xgb_imp=xgb_imp, plot_imp=xgb_plot, cols_used=trn_cols, xgb=model)
        oos_chunk_results[[(k - 1) * n_oos + ioos]] <- chunk_results  # 
        tcheck(desc= sprintf('trained k %d/5 oos_model %d', k, ioos))
    }
    family_results <- rbind(family_results, fold_results)
}

saveRDS(family_results, file='../data/f2_xtrain_results.rds')  
saveRDS(oos_chunk_results, file='../data/f2_xtrain_models.rds')

oos_mcc <- numeric()
oos_cut <- numeric()
oos_auc <- numeric()
for (i in 1:length(oos_chunk_results)) {
    ioos <- (i-1) %% n_oos + 1
    k <- floor((i-1) / n_oos) + 1
    oos_mcc[i] <- oos_chunk_results[[i]]$MCC
    oos_cut[i] <- oos_chunk_results[[i]]$cutoff
    oos_auc[i] <- oos_chunk_results[[i]]$AUC
    
    print(oos_chunk_results[[i]]$cutoff )
    imp <- oos_chunk_results[[i]]$xgb_imp
    print(head(imp, 5))
    imp_oos <- imp[, .(Feature, Gain)]
    names(imp_oos)[2] <- sprintf("Gain.k%d.ts%d", k, ioos)
    setkey(imp_oos, Feature)
    if( i == 1) {
        imp_avg <- imp_oos
    } else {
        imp_avg <- imp_avg[imp_oos]
    }
}
imp_avg$avg_gain <- rowMeans(as.matrix( imp_avg[ , -1, with=F ]), na.rm=TRUE  )
imp_avg %>% arrange(desc(avg_gain)) %>% head(25)
imp_avg %>% arrange(desc(avg_gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,avg_gain), avg_gain)) + geom_bar(stat="identity", position='identity') + coord_flip()

print(oos_mcc)
print(oos_cut)
print(oos_auc)
cv_results <- data.frame( MCC=oos_mcc, AUC=oos_auc, cutoff=oos_cut, run=1:(n_oos*nfolds),
                          kfold = as.factor(rep(1:nfolds, each=n_oos)),
                          ts =  rep(1:n_oos, nfolds) )
cv_results %>% gather( result, value, MCC, AUC, cutoff) %>%
    ggplot(aes(run, value)) + 
    geom_line(aes(col=result)) +
    geom_point(aes(shape=kfold), size=4) +
    ggtitle('Results by fold and training set')

family_results$mean_probs <- rowMeans( as.matrix( family_results %>% select(starts_with('prob'))))
ens_preds <- prediction( family_results$mean_probs, family_results$Response )
ens_auc <- performance(ens_preds, "auc")@y.values[[1]]
ens_perf <- performance(ens_preds, "tpr", "fpr")
plot(ens_perf)
mcc <- performance( ens_preds, "mat")
mcc_vals <- mcc@y.values[[1]]
mcc_cuts <- mcc@x.values[[1]]
cutoff <- mcc_cuts[ which.max(mcc_vals)]
mcc_best <- max(mcc_vals, na.rm=TRUE )
plot(mcc_cuts, mcc_vals, type='n')
rect( min(oos_cut), -1, max(oos_cut), 1, col='cyan', border="transparent")
lines(mcc_cuts, mcc_vals, type='l')
abline(v=cutoff)
cutoff_ratio <- find_cutoff_by_ratio( family_results$mean_probs, 1/171)
abline(v=cutoff_ratio, lty=2)
#abline(v=cutoff_wmean, lty=3)  # this is better, but I'm not using a weighted mean for the results yet
abline(v=mean(oos_cut), lty=3)
legend("bottomright", c("Best MCC", "by_ratio", "mean", "MCC range"), 
       lty=c(1,2,3,NA), pch=c(NA,NA,NA, 15), col=c(1,1,1, "cyan"))
title(sprintf("MCC for mean probs AUC=%f", ens_auc))

mcc_ratio <- calc_mcc( table( family_results$Response, family_results$mean_probs >= cutoff_ratio))
mcc_mean <- calc_mcc( table( family_results$Response, family_results$mean_probs >= mean(oos_cut)))
mcc_best
mcc_ratio
mcc_mean
ens_auc

#rm(trnw, trnw_fresh, trnw.f2, trnw.f2.info)
gc()
