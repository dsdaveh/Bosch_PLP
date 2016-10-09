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

n_oos <- floor( 171 / pass_fail_ratio)  
if(! exists("start_oos")) start_oos <- 1    #assume first model is build and stored as .rds somewhere?

refresh_trnw <- function(ichunk, input_csv) {
    trnw <- read_raw_chunk(ichunk, input= input_csv)
    
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
    rm(trnl_date); gc()
    
    setkey(id_cnt, Id)
    
    trnw <- trnw[ id_cnt, nomatch=FALSE]
    rm(id_cnt)
    return(trnw)
}
trnw <- refresh_trnw(ichunk, input_csv); tcheck(desc= sprintf('reconstruct train chunk %d', ichunk))

## move this to eda
# trnw_num %>% ggplot( aes(min_time, fill=as.factor(Response) ))+ geom_density( alpha=.5)
# trnw_num %>% ggplot( aes(min_time ))+ geom_density( alpha=.5)

ix_fail <- which(trnw$Response == '1')
ix_pass_all <- which(trnw$Response == '0')
nFails <- length(ix_fail)
set.seed(seed)
ix_hold_fail <- sample( ix_fail, floor( nFails * .20 ))  # 20% holdout for testing
ix_trn_fail <- setdiff( ix_fail, ix_hold_fail)

trnw_fresh <- trnw   #saves time of refresh_trnw if we have memory
oos_chunk_results <- list()
for (ioos in start_oos:n_oos) {
    trnw <- trnw_fresh
    
    #shrink the number of passes to choose from
    i <- 0
    ix_pass_oos <- ix_pass_all
    while( i < ioos) {  
        i <- i + 1
        ix_pass <- sample( ix_pass_oos, min(nFails * pass_fail_ratio, length(ix_pass_all)) )
        ix_pass_oos <- setdiff(ix_pass_oos, ix_pass)
    }
    
    ix_hold_pass <- sample( ix_pass, floor(length(ix_pass) * .20 ))  # 20% holdout for testing
    ix_trn_pass <- setdiff( ix_pass, ix_hold_pass)
    
    # create datasets
    trn_hold <- trnw[ c(ix_hold_fail, ix_hold_pass) ] %>% sample_frac()
    trnw <- trnw[ c(ix_trn_fail, ix_trn_pass) ] %>% sample_frac()
    
    trn_cols <- setdiff( names(trnw), c("Id", "Response"))
    
    xgb_params <- list( 
        eta = 0.3,      #
        #     max_depth = 6,   # 
        #     gamma = 0.5,     # 
        #     min_child_weight = 5, #
        #     subsample = 0.5,
        #     colsample_bytree = 0.5, 
        eval_metric = "logloss", #mlogloss",  #map@3",
        objective = "binary:logistic",
        # num_class = 12,
        nthreads = 4
        # maximize = TRUE
    )
    xgb_nrounds = 500
    
    na_cols = which( lapply(trnw, function(x) all(is.na(x))) == TRUE )
    trn_cols2 <- setdiff(trn_cols, names(na_cols))
    xgb_trn <- xgb.DMatrix( dropNA(as.matrix(trnw[, .SD, .SDcols = trn_cols2])), label = trnw$Response, missing = 99 )
    xgb_tst  <- xgb.DMatrix( dropNA(as.matrix(trn_hold[, .SD, .SDcols = trn_cols2])), label = trn_hold$Response, missing = 99 )
    
    model <- xgb.train( params = xgb_params, 
                        data=xgb_trn,
                      nrounds = xgb_nrounds,
                      watchlist = list(eval = xgb_tst),
                      print.every.n = 5L,
                      early.stop.round = 5L,
                      verbose = 1 )
    probs <- predict( model, dropNA(as.matrix(trn_hold[, .SD, .SDcols = trn_cols2]) ) )
    preds <- prediction( probs, trn_hold$Response )
    perf <- performance(preds, "tpr", "fpr")
    plot(perf)
    table( ifelse(probs > .5, 1, 0), trn_hold$Response)
    mcc <- performance( preds, "mat")
    mcc_vals <- unlist( attr(mcc, "y.values"))
    mcc_cuts <- unlist( attr(mcc, "x.values"))
    cutoff <- mcc_cuts[ which.max(mcc_vals)]
    plot(mcc_cuts, mcc_vals, type='l')
    abline(v=cutoff)
    title(paste("MMC versus cuttof for chunk", ichunk))
    mcc_best <- max(mcc_vals, na.rm=TRUE )
    cat( sprintf( "max MCC @ %4.2f = %f\n", cutoff, mcc_best ))
    
    xgb_imp <- xgb.importance( trn_cols, model=model )
    xgb_plot <- xgb_imp %>% arrange(desc(Gain)) %>% dplyr::slice(1:30) %>% ggplot( aes(reorder(Feature,Gain), Gain)) + geom_bar(stat="identity", position='identity') + coord_flip()
    
    chunk_results <- list( chunk=ichunk, MCC=mcc_best, cutoff=cutoff, xgb_imp=xgb_imp,
                           plot_imp=xgb_plot, cols_used=trn_cols2, xgb=model)
    oos_chunk_results[[ioos]] <- chunk_results  # TODO should write ioos in here as well but need to inform downstream code first
    tcheck(desc= sprintf('trained chunk %d oos_model %d', ichunk, ioos))
}