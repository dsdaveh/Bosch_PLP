# minimal observation with wide dataset
# Try a wide dataset with all the failure observations and 5:1 random pass:fail observations
#
# 
# _study builds the models
# _study_xrun variation uses models from a previous run.  All 10 models are run again the holdout for each chunk
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

stopifnot(exists("pass_fail_ratio"))  #this should be set from training step

## parameters
if(! exists("pass_fail_ratio_score")) pass_fail_ratio_score <- 200
if(! exists("ichunk")) ichunk <- 1
if(! exists("input_csv")) input_csv <- '../input/train_numeric.csv'
if(! exists("seed"))seed <- 1912
if(! exists("Faron_magic")) Faron_magic <- FALSE
## 
trnw <- read_raw_chunk(ichunk, input=input_csv )

# number of na's os a feature (might be the only feature for some categorical observations)
trnw$na_count <- apply(trnw, 1, function(x) sum(is.na(x))) 
setkey(trnw, Id)

trnl_date <- readRDS(file = sprintf("../data/train_date_long_chunk_%d.rds", ichunk)) # see 'load_date_long.R'
tcheck( desc = 'initial data load')

if( ! grepl("numeric", input_csv)) {
    truth <- readRDS(file='../data/train_response.rds')
    trnw <- trnw[ truth, nomatch=FALSE ]
}

## stolen from date eda
id_cnt <- trnl_date[
    , station := str_extract(metric, "S\\d+")][               #extract station ID
        , .(station_metric_count=.N,
            time_in = min(value), time_out = max(value)), by=c("Id", "station")][     #rollup by station
                , .(station_count = .N, metric_count = sum(station_metric_count),
                    min_time = min(time_in), max_time = max(time_out)), by=Id ][  #rollup by Id
                        , proc_time := max_time - min_time]
rm(trnl_date); gc(); tcheck(desc='extract data features')

setkey(id_cnt, Id)

trnw <- trnw[ id_cnt, nomatch=FALSE]
rm(id_cnt)

if (Faron_magic) {
    magic <- readRDS(file = '../data/faron_magic4.rds')
    setkey(magic, Id)
    trnw <- trnw[ magic, nomatch=FALSE]
    rm(magic)
}

ix_fail <- which(trnw$Response == '1')
nFails <- length(ix_fail)
set.seed(seed)

#shrink the number of passes to choose from
n_pass_size <- nFails * pass_fail_ratio_score
ix_pass <- setdiff(1:nrow(trnw), ix_fail)
if (n_pass_size > length(ix_pass)) {
    cat( sprintf(
        "Request pass/fail ratio (%d) exeeds the data (%d) using full set\n",
        floor(pass_fail_ratio_score), floor(length(ix_pass) / nFails) ))
} else {
    ix_pass <- sample( ix_pass, n_pass_size )
}

# create datasets
trnw <- trnw[ c(ix_fail, ix_pass) ] %>% sample_frac()

xresults <- data.frame()  # summarized results
obs_results <- data.table()
obs_stack <- trnw[, .(Id, Response)]
reslen <- 7  # this should match the length of chunk_results in min_obs_wide_study.R
for (imodel in 1:10) {
    if (imodel == ichunk) {
        na_tmp <- rep(NA, nrow(trnw))
        answers <- data.frame( probs= na_tmp, yhat= na_tmp  )
        auc_val <- mcc_best <- cutoff <- NA
    } else {
        model_cols <- results[[ (imodel - 1) * reslen + 6 ]]
        par.orig <- par(mfrow=c(1,2))
        model <- results[[imodel * reslen]]
        probs <- predict( model, dropNA(as.matrix(trnw[, .SD, .SDcols = model_cols ]) ))
        obs_results <- rbind( 
            obs_results, trnw[, .(
                imodel, Id, probs, Response
            )])
        
        preds <- prediction( probs, trnw$Response ) #ROCR
        auc_val <- performance(preds, "auc")@y.values[[1]]
        perf <- performance(preds, "tpr", "fpr")
        plot(perf)
        mcc <- performance( preds, "mat")
        mcc_vals <- unlist( attr(mcc, "y.values"))
        mcc_cuts <- unlist( attr(mcc, "x.values"))
        cutoff <- mcc_cuts[ which.max(mcc_vals)]
        mcc_best <- max(mcc_vals, na.rm=TRUE )
            
        xresults <- rbind(
            xresults, data.frame( imodel=imodel, ichunk=ichunk,
                                  MCC=mcc_best, AUC=auc_val, cutoff=cutoff))

        plot(mcc_cuts, mcc_vals, type='l')
        abline(v=cutoff)
        par(par.orig)
        title(sprintf("ROC & MCC for model %d using chunk %d AUC=%f", imodel, ichunk, auc_val))
        
        yhat <- as.integer( probs >= cutoff)
        answers <- data.frame( probs, yhat)
    }
    # add results as columns
    names(answers) <- c( paste0('p',imodel), paste0('yhat',imodel))
    obs_stack <- cbind( obs_stack, answers)
    rm(answers)
    
    #print( table( trn_hold$Response, ifelse(probs > cutoff, 1, 0) ))
    tcheck( desc = sprintf( "max MCC @ %4.2f = %f, AUC=%f (chunk,model = %d,%d)",
                  cutoff, mcc_best, auc_val, ichunk, imodel ))
    
}
# xgb_imp <- xgb.importance( trn_cols, model=model )
# xgb_plot <- xgb_imp %>% arrange(desc(Gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,Gain), Gain)) + geom_bar(stat="identity", position='identity') + coord_flip()

#ensemble results
#################
# method 1 - mean probabilities
# method 2 - stacking (happens one level up)

# mean probabilty of all the (nchunk - 1) models by Id
ens_results <- obs_results[, .(Response = min(Response),
                               mean_prob = mean(probs)), by=Id]

#method1 cutoff finding ratio
cutoff_m1 <- find_cutoff_by_ratio( ens_results$mean_prob, 1/171)
ens_results$y_m1 <- as.integer( ens_results$mean_prob >= cutoff_m1 )

#plots and evaluation
par(mfrow=c(1,1))
par.orig <- par(mfrow=c(1,2))

ens_preds <- with(ens_results, prediction( mean_prob, Response )) #ROCR
ens_auc <- performance(ens_preds, "auc")@y.values[[1]]
ens_perf <- performance(ens_preds, "tpr", "fpr")
plot(ens_perf)
mcc <- performance( ens_preds, "mat")
mcc_vals <- mcc@y.values[[1]]
mcc_cuts <- mcc@x.values[[1]]
cutoff <- mcc_cuts[ which.max(mcc_vals)]
mcc_best <- max(mcc_vals, na.rm=TRUE )
cutoff_wmean <- with(xresults, weighted.mean(cutoff, AUC))
plot(mcc_cuts, mcc_vals, type='n')
rect( min(xresults$cutoff), -1, max(xresults$cutoff), 1, col='cyan', border="transparent")
lines(mcc_cuts, mcc_vals, type='l')
abline(v=cutoff)
abline(v=cutoff_m1, lty=2)
#abline(v=cutoff_wmean, lty=3)  # this is better, but I'm not using a weighted mean for the results yet
abline(v=mean(xresults$cutoff), lty=3)
legend("bottomright", c("Best MCC", "by_ratio", "mean", "MCC range"), 
       lty=c(1,2,3,NA), pch=c(NA,NA,NA, 15), col=c(1,1,1, "cyan"))

par(par.orig)
title(sprintf("ROC & MCC for ensembled chunk %d results AUC=%f", ichunk, ens_auc))

# barplot(xresults$AUC, ylim=c(0, ens_auc+.05))
# abline(h=ens_auc, lty=2)
# text(1,ens_auc, "m1 ensemble", pos=3)
# title(sprintf('AUC for models on chunk %d', ichunk))

tcheck(desc= sprintf('completed chunk %d', ichunk))