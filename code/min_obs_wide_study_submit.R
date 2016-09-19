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
if(! exists("input_csv")) test_csv <- '../input/test_numeric.csv'
## 
tstw <- read_raw_chunk(ichunk, input=test_csv)

# number of na's os a feature (might be the only feature for some categorical observations)
tstw$na_count <- apply(tstw, 1, function(x) sum(is.na(x))) 
setkey(tstw, Id)

tstl_date <- readRDS(file = sprintf("../data/test_date_long_chunk_%d.rds", ichunk)) # see 'load_date_long.R'
tcheck( desc = 'initial data load')

## stolen from date eda
id_cnt <- tstl_date[
    , station := str_extract(metric, "S\\d+")][               #extract station ID
        , .(station_metric_count=.N,
            time_in = min(value), time_out = max(value)), by=c("Id", "station")][     #rollup by station
                , .(station_count = .N, metric_count = sum(station_metric_count),
                    min_time = min(time_in), max_time = max(time_out)), by=Id ][  #rollup by Id
                        , proc_time := max_time - min_time]
rm(tstl_date); gc(); tcheck(desc='extract date features')

setkey(id_cnt, Id)

all_ids <- tstw$Id
tstw <- tstw[ id_cnt, nomatch=FALSE]
rm(id_cnt)

missing_date_ids <- setdiff(all_ids, tstw$Id) 

obs_results <- data.table()
obs_stack <- tstw[, .(Id)]
reslen <- 7  # this should match the length of chunk_results in min_obs_wide_study.R

for (imodel in 1:10) {

    model_cols <- results[[ (imodel - 1) * reslen + 6 ]]
    model <- results[[imodel * reslen]]
    probs <- predict( model, dropNA(as.matrix(tstw[, .SD, .SDcols = model_cols ]) )); tcheck(desc=sprintf("run model%d", imodel))
    cutoff <- find_cutoff_by_ratio( probs, 1/171)  #TODO Optimize this by chunk
    yhat <- as.integer( probs >= cutoff)
    
    # add results as columns
    answers <- data.frame( probs, yhat)
    names(answers) <- c( paste0('p',imodel), paste0('yhat',imodel))
    obs_stack <- cbind( obs_stack, answers)
    rm(answers)
    
    obs_results <- rbind( 
        obs_results, tstw[, .(
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

tcheck(desc= sprintf('completed chunk %d', ichunk))
