# minimal observation with wide dataset
# Try a wide dataset with all the failure observations and 5:1 random pass:fail observations
#
# 
# _study builds the models
# _study_xrun variation uses models from a previous run.  All 10 models are run again the holdout for each chunk
# _submit creates a submission TODO: (should combine with _xrun, so it doesn't diverge)
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

## parameters
if(! exists("ichunk")) ichunk <- 1
## 
trnw_num <- read_raw_chunk(ichunk, input='../input/test_numeric.csv')
trnl_date <- readRDS(file = sprintf("../data/test_date_long_chunk_%d.rds", ichunk)) # see 'load_date_long.R'
tcheck( desc = 'initial data load')

## stolen from date eda
id_cnt <- trnl_date[
    , station := str_extract(metric, "S\\d+")][               #extract station ID
        , .(station_metric_count=.N,
            time_in = min(value), time_out = max(value)), by=c("Id", "station")][     #rollup by station
                , .(station_count = .N, metric_count = sum(station_metric_count),
                    min_time = min(time_in), max_time = max(time_out)), by=Id ][  #rollup by Id
                        , proc_time := max_time - min_time]
rm(trnl_date); gc(); tcheck(desc='extract data features')

setkey(trnw_num, Id)
setkey(id_cnt, Id)

trnw_num <- trnw_num[ id_cnt, nomatch=FALSE]
rm(id_cnt)

## move this to eda
# trnw_num %>% ggplot( aes(min_time, fill=as.factor(Response) ))+ geom_density( alpha=.5)
# trnw_num %>% ggplot( aes(min_time ))+ geom_density( alpha=.5)

# ix_fail <- which(trnw_num$Response == '1')
# nFails <- length(ix_fail)
# set.seed(1912)
# ix_hold_fail <- sample( ix_fail, floor( nFails * .20 ))  # 20% holdout for testing
### ix_trn_fail <- setdiff( ix_fail, ix_hold_fail)

#shrink the number of passes to choose from
# n_pass_size <- nFails * pass_fail_ratio
# ix_pass <- which(trnw_num$Response == '0')
# if (n_pass_size > length(ix_pass)) {
#     warning( sprintf(
#         "Request pass/fail ratio (%d) exeeds the data (%d) using full set\n",
#         floor(pass_fail_ratio), floor(length(ix_pass) / nFails) ))
# } else {
#     ix_pass <- sample( ix_pass, n_pass_size )
# }
# ix_hold_pass <- sample( ix_pass, floor(length(ix_pass) * .20 ))  # 20% holdout for testing
# ### ix_trn_pass <- setdiff( ix_pass, ix_hold_pass)
# 
# # create datasets
# trn_hold <- trnw_num[ c(ix_hold_fail, ix_hold_pass) ] %>% sample_frac()
# ### trnw_num <- trnw_num[ c(ix_trn_fail, ix_trn_pass) ] %>% sample_frac()

trn_cols <- setdiff( names(trnw_num), c("Id", "Response"))
# 
# xgb_params <- list( 
#     eta = 0.3,      #
#     #     max_depth = 6,   # 
#     #     gamma = 0.5,     # 
#     #     min_child_weight = 5, #
#     #     subsample = 0.5,
#     #     colsample_bytree = 0.5, 
#     eval_metric = "logloss", #mlogloss",  #map@3",
#     objective = "binary:logistic",
#     # num_class = 12,
#     nthreads = 4,
#     # maximize = TRUE
#     verbose = 1
# )
# xgb_nrounds = 500
# 
# xgb.train <- xgb.DMatrix( dropNA(as.matrix(trnw_num)[, trn_cols]), label = trnw_num$Response, missing = 99 )
# model <- xgboost( xgb.train,
#                   nrounds = xgb_nrounds,
#                   params = xgb_params, verbose = 1 )

xresults <- data.frame()  # summarized results
obs_results <- data.table()
for (imodel in 1:10) {
    
#     par.orig <- par(mfrow=c(1,2))
    model <- results[[imodel * 6]]
    probs <- predict( model, dropNA(as.matrix(trn_hold)[, trn_cols]) )
#     preds <- prediction( probs, trn_hold$Response ) #ROCR
#     perf <- performance(preds, "tpr", "fpr")
#     plot(perf)
#     mcc <- performance( preds, "mat")
#     mcc_vals <- unlist( attr(mcc, "y.values"))
#     mcc_cuts <- unlist( attr(mcc, "x.values"))
#     cutoff <- mcc_cuts[ which.max(mcc_vals)]
#     plot(mcc_cuts, mcc_vals, type='l')
#     abline(v=cutoff)
#     par(par.orig)
#     title(sprintf("ROC & MCC for model %d using chunk %d", imodel, ichunk))
#     
#     mcc_best <- max(mcc_vals, na.rm=TRUE )
#     
#     #print( table( trn_hold$Response, ifelse(probs > cutoff, 1, 0) ))
#     cat( sprintf( "max MCC @ %4.2f = %f\n", cutoff, mcc_best ))
    
    obs_results <- rbind( 
        obs_results, trn_hold[, .(
            imodel, Id, probs
        )])
}
# xgb_imp <- xgb.importance( trn_cols, model=model )
# xgb_plot <- xgb_imp %>% arrange(desc(Gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,Gain), Gain)) + geom_bar(stat="identity", position='identity') + coord_flip()

#ensemble results
#################
# method 1 - mean predictions
# method 2 - mean probabilities

ens_results <- obs_results[, .(mean_prob = mean(probs)), by=Id]

#method1 cutoff finding ratio
cutoff_m1 <- find_cutoff_by_ratio( ens_results$mean_prob, 1/171)
ens_results$prob_pred <- as.integer( ens_results$mean_prob >= cutoff_m1 )

# #method2 cutoff finding ratio
# cutoff_m2 <- find_cutoff_by_ratio( ens_results$mean_pred, 1/171)
# ens_results$pred_pred <- as.integer( ens_results$mean_prob >= cutoff_m2 )

tcheck(desc= sprintf('completed chunk %d', ichunk))