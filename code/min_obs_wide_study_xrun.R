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

## parameters
if(! exists("pass_fail_ratio_score")) pass_fail_ratio_score <- 200
if(! exists("ichunk")) ichunk <- 1
if(! exists("input_csv")) input_csv <- '../input/train_numeric.csv'
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

ix_fail <- which(trnw$Response == '1')
nFails <- length(ix_fail)
set.seed(1912)
ix_hold_fail <- sample( ix_fail, floor( nFails * .20 ))  # 20% holdout for testing
### ix_trn_fail <- setdiff( ix_fail, ix_hold_fail)

#shrink the number of passes to choose from
n_pass_size <- nFails * pass_fail_ratio_score
ix_pass <- which(trnw$Response == '0')
if (n_pass_size > length(ix_pass)) {
    warning( sprintf(
        "Request pass/fail ratio (%d) exeeds the data (%d) using full set\n",
        floor(pass_fail_ratio_score), floor(length(ix_pass) / nFails) ))
} else {
    ix_pass <- sample( ix_pass, n_pass_size )
}
ix_hold_pass <- sample( ix_pass, floor(length(ix_pass) * .20 ))  # 20% holdout for testing
### ix_trn_pass <- setdiff( ix_pass, ix_hold_pass)

# create datasets
trn_hold <- trnw[ c(ix_hold_fail, ix_hold_pass) ] %>% sample_frac()

xresults <- data.frame()  # summarized results
obs_results <- data.table()
obs_stack <- trn_hold[, .(Id, Response)]
reslen <- 7  # this should match the length of chunk_results in min_obs_wide_study.R
for (imodel in 1:10) {
    
    model_cols <- results[[ (imodel - 1) * reslen + 6 ]]
    par.orig <- par(mfrow=c(1,2))
    model <- results[[imodel * reslen]]
    probs <- predict( model, dropNA(as.matrix(trn_hold[, .SD, .SDcols = model_cols ]) ))
    preds <- prediction( probs, trn_hold$Response ) #ROCR
    perf <- performance(preds, "tpr", "fpr")
    plot(perf)
    mcc <- performance( preds, "mat")
    mcc_vals <- unlist( attr(mcc, "y.values"))
    mcc_cuts <- unlist( attr(mcc, "x.values"))
    cutoff <- mcc_cuts[ which.max(mcc_vals)]
    yhat <- as.integer( probs >= cutoff)
    
    # add results as columns
    answers <- data.frame( probs, yhat)
    names(answers) <- c( paste0('p',imodel), paste0('yhat',imodel))
    obs_stack <- cbind( obs_stack, answers)
    rm(answers)
    
    plot(mcc_cuts, mcc_vals, type='l')
    abline(v=cutoff)
    par(par.orig)
    title(sprintf("ROC & MCC for model %d using chunk %d", imodel, ichunk))
    
    mcc_best <- max(mcc_vals, na.rm=TRUE )
    
    #print( table( trn_hold$Response, ifelse(probs > cutoff, 1, 0) ))
    cat( sprintf( "max MCC @ %4.2f = %f\n", cutoff, mcc_best ))
    
    obs_results <- rbind( 
        obs_results, trn_hold[, .(
            imodel, Id, probs, Response
        )])
    
    xresults <- rbind(
        xresults, data.frame( imodel=imodel, ichunk=ichunk, MCC=mcc_best, cutoff=cutoff))
}
# xgb_imp <- xgb.importance( trn_cols, model=model )
# xgb_plot <- xgb_imp %>% arrange(desc(Gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,Gain), Gain)) + geom_bar(stat="identity", position='identity') + coord_flip()

#ensemble results
#################
# method 1 - mean probabilities
# method 2 - stacking (happens one level up)

ens_results <- obs_results[, .(Response = min(Response),
                               mean_prob = mean(probs)), by=Id]

#method1 cutoff finding ratio
cutoff_m1 <- find_cutoff_by_ratio( ens_results$mean_prob, 1/171)
ens_results$y_m1 <- as.integer( ens_results$mean_prob >= cutoff_m1 )

tcheck(desc= sprintf('completed chunk %d', ichunk))