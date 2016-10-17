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

source('f2_family_data_prep_test.R')
tstw <- dt.f2
rm(dt.f2)

# number of na's os a feature (might be the only feature for some categorical observations)
tstw$na_count <- apply(tstw, 1, function(x) sum(is.na(x))) 
setkey(tstw, Id)

xxxxxxxxxxxxx
dt_date <- data.table()
for (i in 1:10) {
    dt_date <- rbind(
        dt_date,
        readRDS(file = sprintf("../data/test_date_long_chunk_%d.rds", i)) ) # see 'load_date_long.R'
    setkey(dt_date, Id)
    dt_date <- dt_date[ Id %in% tstw$Id ] ; gc()
}
tcheck( desc = 'loaded date dataset (long)')

## stolen from date eda
id_cnt <- dt_date[
    , station := str_extract(metric, "S\\d+")][               #extract station ID
        , .(station_metric_count=.N,
            time_in = min(value), time_out = max(value)), by=c("Id", "station")][     #rollup by station
                , .(station_count = .N, metric_count = sum(station_metric_count),
                    min_time = min(time_in), max_time = max(time_out)), by=Id ][  #rollup by Id
                        , proc_time := max_time - min_time]
rm(dt_date); gc()
setkey(id_cnt, Id)

tstw <- tstw[ id_cnt, nomatch=FALSE]
rm(id_cnt); gc()

mcc_cutoff <- rep(0., length(oos_chunk_results))
obs_results <- data.table()
obs_stack <- tstw[, .(Id)]
reslen <- 7  # this should match the length of chunk_results in min_obs_wide_study.R


for (i in 1:length(oos_chunk_results)) {

    model_cols <- oos_chunk_results[[i]]$cols_used  #NOTE: identical for all models
    model <- oos_chunk_results[[i]]$xgb
    mcc_cutoff[i] <-  oos_chunk_results[[i]]$cutoff
    probs <- predict( model, dropNA(as.matrix(tstw[, .SD, .SDcols = model_cols ]) )); tcheck(desc=sprintf("run model%d", imodel))
    cutoff <- find_cutoff_by_ratio( probs, 1/171)  #TODO Optimize this by chunk
    yhat <- as.integer( probs >= cutoff)
    
    # add results as columns
    answers <- data.frame( probs, yhat)
    names(answers) <- c( paste0('p',i), paste0('yhat',i))
    obs_stack <- cbind( obs_stack, answers)
    rm(answers)
    gc()
}
# xgb_imp <- xgb.importance( trn_cols, model=model )
# xgb_plot <- xgb_imp %>% arrange(desc(Gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,Gain), Gain)) + geom_bar(stat="identity", position='identity') + coord_flip()

#ensemble results
#################
# method 1 - mean predictions
# method 2 - mean probabilities

obs_stack$p_mean <- rowMeans( as.matrix( obs_stack %>% select(starts_with("p"))))

#method1 cutoff finding ratio
cutoff_ratio <- find_cutoff_by_ratio( obs_stack$p_mean, 1/171)
obs_stack$yhat_ratio <- as.integer( obs_stack$p_mean >= cutoff_ratio )
obs_stack$yhat_best_mccs <- as.integer( obs_stack$p_mean >= mean(mcc_cutoff) )
nrow(obs_stack) / sum(obs_stack$yhat_ratio) #171.7
nrow(obs_stack) / sum(obs_stack$yhat_best_mccs) # 479

tcheck(desc= sprintf('completed predictions for %d Ids', nrow(obs_stack)))

# num50 baseline min_obs_thin_2016_09_18_163805.zip (LB = 0.23815)
# f2 baseline f2_baseline.zip (LB = 0.09300)

#start with the f2_baseline
f2_baseline <- fread('../submissions/f2_baseline.csv')
setkey(obs_stack, Id)
setkey(f2_baseline, Id)
f2_ix <- which(f2_baseline$Id %in% obs_stack$Id)
sum(f2_baseline$Response) #1351
sum(obs_stack$yhat_ratio) #1400
table(obs_stack$yhat_ratio, f2_baseline[f2_ix, Response])

f2_baseline[f2_ix, Response := obs_stack$yhat_ratio ]
write.csv(f2_baseline, '../submissions/f2_num50_a.csv', row.names = FALSE)
