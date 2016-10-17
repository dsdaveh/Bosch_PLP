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

source("f2_family_data_prep.R")

n_oos <- floor( 171 / pass_fail_ratio)  
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

# build a hold out data set -- consistent for each round
ix_fail <- which(trnw$Response == '1')
ix_pass_all <- which(trnw$Response == '0')
pass_fail_train <- length(ix_pass_all) / length(ix_fail)
nFails <- length(ix_fail)
set.seed(seed)
ix_hold_fail <- sample( ix_fail, floor( nFails * .20 ))  # 20% holdout for testing
ix_hold_pass <- sample( ix_pass_all, length(ix_hold_fail) * pass_fail_train)
trn_hold <- trnw[ c(ix_hold_fail, ix_hold_pass) ] %>% sample_frac()
trn_cols <- setdiff( names(trnw), c("Id", "Response"))
xgb_oos <- xgb.DMatrix( dropNA(as.matrix(trn_hold[, .SD, .SDcols = trn_cols])), label = trn_hold$Response, missing = 99 )

#build pool for training data
ix_trn_fail <- setdiff( ix_fail, ix_hold_fail)
n_trn_pass <- length(ix_trn_fail) * pass_fail_ratio  # we'll sample this many passes each round
ix_pass_oos <- setdiff( ix_pass_all, ix_hold_pass )

trnw_fresh <- trnw   #saves time of refresh_trnw if we have memory
oos_chunk_results <- list()
family_results <- trn_hold[, .(Id, Response)]

for (ioos in start_oos:n_oos) {
    trnw <- trnw_fresh
    
    #shrink the number of passes to choose from
    ix_trn_pass <- sample( ix_pass_oos, min(n_trn_pass, length(ix_pass_oos)) )
    ix_pass_oos <- setdiff(ix_pass_oos, ix_trn_pass)

    trnw <- trnw[ c(ix_trn_fail, ix_trn_pass) ] %>% sample_frac()
    
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
    
    na_cols = which( lapply(trnw, function(x) all(is.na(x))) == TRUE )
    xgb_trn <- xgb.DMatrix( dropNA(as.matrix(trnw[, .SD, .SDcols = trn_cols])), label = trnw$Response, missing = 99 )
    model <- xgb.train( params = xgb_params, 
                        data=xgb_trn,
                        nrounds = xgb_nrounds,
                        watchlist = list(eval = xgb_oos),
                        print.every.n = 5L,
                        early.stop.round = 10L,
                        verbose = 1 )
    probs <- predict( model, dropNA(as.matrix(trn_hold[, .SD, .SDcols = trn_cols]) ) )
    preds <- prediction( probs, trn_hold$Response )
    auc_val <- performance(preds, "auc")@y.values[[1]]
    perf <- performance(preds, "tpr", "fpr")
    plot(perf)
    mcc <- performance( preds, "mat")
    mcc_vals <- unlist( attr(mcc, "y.values"))
    mcc_cuts <- unlist( attr(mcc, "x.values"))
    cutoff_mcc <- mcc_cuts[ which.max(mcc_vals)]
    table( ifelse(probs > cutoff_mcc, 1, 0), trn_hold$Response)
    plot(mcc_cuts, mcc_vals, type='l')
    abline(v=cutoff_mcc)
    title(paste("MMC versus cutoff for train set", ioos))
    mcc_best <- max(mcc_vals, na.rm=TRUE )
    cat( sprintf( "max MCC @ %4.2f = %f, AUC = %f\n", cutoff_mcc, mcc_best, auc_val ))
    cutoff_ratio <- find_cutoff_by_ratio( probs, 1/pass_fail_train)
    table( ifelse(probs > cutoff_ratio, 1, 0), trn_hold$Response)
    abline(v=cutoff_ratio, lty=2 )
    
    pred_mcc <- ifelse( probs >= cutoff_mcc, 1, 0)
    pred_ratio <- ifelse( probs >= cutoff_ratio, 1, 0)
    mcc_mcc <- calc_mcc( with(family_results, table(Response, pred_mcc)) )
    mcc_ratio <- calc_mcc( with(family_results, table(Response, pred_ratio)) )
    
    family_results[, (paste0('prob_', ioos)) := probs ]
    family_results[, (paste0('mcc_best_', ioos)) := pred_mcc ]
    family_results[, (paste0('mcc_ratio_', ioos)) := pred_ratio ]
    
    xgb_imp <- xgb.importance( trn_cols, model=model )
    xgb_plot <- xgb_imp %>% arrange(desc(Gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,Gain), Gain)) + geom_bar(stat="identity", position='identity') + coord_flip()
    
    chunk_results <- list( model=ioos, MCC=mcc_best, cutoff=cutoff_mcc, AUC=auc_val, 
                           xgb_imp=xgb_imp, plot_imp=xgb_plot, cols_used=trn_cols, xgb=model)
    oos_chunk_results[[ioos]] <- chunk_results  # 
    tcheck(desc= sprintf('trained chunk %d oos_model %d', ichunk, ioos))
}

saveRDS(family_results, file='../data/f2_xtrain_results.rds')  
saveRDS(oos_chunk_results, file='../data/f2_xtrain_models.rds')

oos_mcc <- numeric()
oos_cut <- numeric()
oos_auc <- numeric()
for (i in 1:length(oos_chunk_results)) {
    oos_mcc[i] <- oos_chunk_results[[i]]$MCC
    oos_cut[i] <- oos_chunk_results[[i]]$cutoff
    oos_auc[i] <- oos_chunk_results[[i]]$AUC
    
    print(oos_chunk_results[[i]]$cutoff )
    imp <- oos_chunk_results[[i]]$xgb_imp
    print(head(imp, 5))
    imp_oos <- imp[, .(Feature, Gain)]
    setkey(imp_oos, Feature)
    if( i == 1) {
        imp_avg <- imp_oos
    } else {
        imp_avg <- imp_avg[imp_oos]
    }
}
imp_avg <- imp_avg %>% mutate( avg_gain = (Gain + i.Gain + i.Gain.1) / 3 ) %>%
    arrange( desc( avg_gain))
print(head(imp_avg, 5))
print(oos_mcc)
print(oos_cut)
print(oos_auc)

family_results[, mean_probs := (prob_1 + prob_2 + prob_3) / 3] 
ens_preds <- prediction( family_results$mean_probs, trn_hold$Response )
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
legend("topright", c("Best MCC", "by_ratio", "mean", "MCC range"), 
       lty=c(1,2,3,NA), pch=c(NA,NA,NA, 15), col=c(1,1,1, "cyan"))

title(sprintf("MCC for mean probs AUC=%f", ens_auc))
mcc_ratio <- calc_mcc( table( family_results$Response, family_results$mean_probs >= cutoff_ratio))
mcc_mean <- calc_mcc( table( family_results$Response, family_results$mean_probs >= mean(oos_cut)))
mcc_best
mcc_ratio
mcc_mean
ens_auc

rm(trnw, trnw_fresh, trnw.f2, trnw.f2.info)
gc()
