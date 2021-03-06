# _run_thin runs  _study multiple times over different chunks

if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(tidyr)
library(gridExtra)

source('bosch_plp_util.R')
tcheck.print = TRUE
tcheck(0)


##########################
## parameters
# ichunk contolled in for loops
if(! exists("pass_fail_ratio")) pass_fail_ratio <- 50
if(! exists("input_csv")) input_csv <- '../input/train_numeric.csv'
if(! exists("seed"))seed <- 1912
Faron_magic <- TRUE
##########################
cat("pass_fail_ratio for train =", pass_fail_ratio, "\n")
cat("training input =", input_csv, "\n")

# This step creates the models for each chunk and runs a test on the same chunk (results)
##
results <- list()
for (ichunk in 1:10) {
    source('min_obs_wide_study.R')
    results <- c(results, chunk_results)
}
save(results, file='../data/min_obs_thin.RData')

fea_diffs <- list()
ix <- 4
fea_rank <- results[[ix]] %>% select( Feature, Gain.1 = Gain )

for(i in 1:10) {
    imp2 <- results[[(i-1)*7 + ix]] %>% select( Feature, Gain)
    names(imp2) <- c("Feature", sprintf("Gain.%d", i))
    fea_diffs[[length(fea_diffs)+1]] <- setdiff( imp2$Feature, fea_rank$Feature)
    fea_rank <- merge( fea_rank, imp2, by="Feature", all=TRUE)
}
fea_rank$Gain.sum <- fea_rank %>% select(-Feature) %>% as.matrix() %>% rowSums(na.rm=TRUE)
fea_top30 <- fea_rank %>% arrange(desc(Gain.sum)) %>% dplyr::slice(1:30)
plot_top30 <- fea_top30 %>% ggplot( aes(reorder(Feature,Gain.sum), Gain.sum)) +
    geom_bar(stat="identity", position='identity') + coord_flip()

plot_top30_range <- fea_top30 %>% select( -Feature, -Gain.sum) %>% gather(model, Gain) %>%
    ggplot( aes(model, Gain, group=model)) + geom_bar(stat="sum")

# This step runs the models for each chunk (xresults_all)
##########################
## parameters
# ichunk contolled in for loops
pass_fail_ratio_score <- 200  # don't change this ... 200 basically tests on all the test data
##

xresults_all <- data.frame()
ens_results_all <- data.table()
obs_stack_all <- data.table()
ratios <- numeric()  # keep track of the pass/fail ratio used (if param setting exceeds data)
for (ichunk in 1:10) {
    source('min_obs_wide_study_xrun.R')
    xresults_all <-rbind(xresults_all, xresults)
    ratios <- c( ratios, floor(length(ix_pass) / nFails) )
    ens_results_all <- rbind(ens_results_all, ens_results[, chunk := ichunk])
    obs_stack[, chunk := ichunk]
    obs_stack_all <- rbind(obs_stack_all, obs_stack)
}
saveRDS(xresults_all, file='../data/min_obs_thin_xrun_meta.rds')
saveRDS(ens_results_all, file='../data/min_obs_thin_xrun_ensemble_m1.rds')
saveRDS(obs_stack_all, file='../data/min_obs_thin_xrun_ensemble_stacked.rds')

summary(xresults_all$MCC)
summary(xresults_all$AUC)
summary(xresults_all$cutoff)

xresults_all <- xresults_all %>% mutate (mregion = ifelse( ichunk <= 3, "low", ifelse( ichunk >= 8, "high", "mid")))
xresults_all %>% mutate(ichunk=as.factor(ichunk)) %>% 
    ggplot( aes(imodel, MCC, group=ichunk, color=ichunk)) + geom_line() +
    geom_smooth(method="lm") +
    facet_wrap( ~mregion) + ggtitle("MCC for all models by chunk (region) model was trained on")

#score m1 ensemble
mcc_m1 <- calc_mcc( with(ens_results_all, table(Response, y_m1)) )
m1_preds <- with(ens_results_all, prediction( mean_prob, Response )) #ROCR
auc_m1 <- performance(m1_preds, "auc")@y.values[[1]]

#stacked ensemble (m2)
ens_cols <- setdiff( names(obs_stack_all), c("Id", "Response"))

nobs <- nrow(obs_stack_all)
ix_hold <- sample(1:nobs, round(nobs * .2))  #so we can score the ensemble
xgb_ens_params <- list( 
    eta = 0.1,      #
    #     max_depth = 6,   # 
    #     gamma = 0.5,     # 
    #     min_child_weight = 5, #
    #     subsample = 0.5,
    #     colsample_bytree = 0.5, 
    eval_metric = "logloss", #mlogloss",  #map@3",
    objective = "binary:logistic"
    #nthreads = 4,
)
xgb_nrounds = 250

obs_stack_all$Response <- as.integer(obs_stack_all$Response) #precautionary
xgb_trn <- xgb.DMatrix( dropNA(as.matrix(obs_stack_all)[-ix_hold, ens_cols]), label = obs_stack_all[-ix_hold, Response], missing = 99 )
xgb_oos <- xgb.DMatrix( dropNA(as.matrix(obs_stack_all)[ix_hold, ens_cols]), label = obs_stack_all[ix_hold, Response], missing = 99 )
model_m2 <- xgb.train( params = xgb_ens_params, 
                     data = xgb_trn,
                     nrounds = xgb_nrounds,
                     watchlist = list( eval=xgb_oos),
                     early.stop.round = 5L,
                     print.every.n = 5L,
                     verbose = 1 )
probs <- predict( model_m2, dropNA(as.matrix(obs_stack_all)[ix_hold, ens_cols]) )
cutoff_m2 <- find_cutoff_by_ratio( probs, 1/171)
mcc_m2 <- calc_mcc( table( obs_stack_all[ix_hold, Response], as.integer(probs >= cutoff_m2)) )
m2_preds <- prediction( probs, obs_stack_all[ix_hold, Response] ) #ROCR
auc_m2 <- performance(m2_preds, "auc")@y.values[[1]]

# This step runs the models for each test chunk
##########################
## parameters
# ichunk contolled in for loops
##

ens_results_tst <- data.table()
obs_stack_tst <- data.table()
guess0 <- integer()
test_csv <- gsub("train", "test", input_csv)
for (ichunk in 1:10) {
    source('min_obs_wide_study_submit.R')
    ens_results_tst <- rbind(ens_results_tst, ens_results[, chunk := ichunk]); tcheck(desc=paste0('predict on test chunk', ichunk))
    guess0 <- c( guess0, missing_date_ids)
    obs_stack[, chunk := ichunk]
    obs_stack_tst <- rbind(obs_stack_tst, obs_stack)
}
ens_results_tst <- rbind(ens_results_tst, data.table( Id=guess0, mean_prob=0, prob_pred=0, chunk=0))

date_stamp <- format(Sys.time(), "%Y_%m_%d_%H%M%S")
saveRDS(ens_results_tst, file= sprintf('../data/min_obs_thin_submit_m1_%s.rds', date_stamp))
saveRDS(obs_stack_tst,  file= sprintf('../data/min_obs_thin_submit_m2_%s.rds', date_stamp))
sfile <- sprintf("../submissions/min_obs_thin_%s.csv", date_stamp)
write.csv( ens_results_tst %>% select(Id, Response=prob_pred), file=sfile, row.names = FALSE)

probs <- predict( model_m2, dropNA(as.matrix(obs_stack_tst)[, ens_cols]) )
cutoff_m2_tst <- find_cutoff_by_ratio( probs, 1/171)
sfile_m2 <- sprintf("../submissions/min_obs_thin_m2_%s.csv", date_stamp)
m2_submit <- obs_stack_tst[, .(Id, Response = probs >= cutoff_m2_tst)]
m2_submit <- rbind(m2_submit, data.table( Id=guess0, Response=0))
write.csv( m2_submit, file=sfile_m2, row.names = FALSE)

#summary for notes
plot_MCC <- xresults_all %>% ggplot(aes(MCC)) + geom_density() + ggtitle ('MCC Density')
grid.arrange( plot_MCC, plot_top30, ncol=2)
mcc_m1
mcc_m2
summary(xresults_all$MCC)
sd(xresults_all$MCC)
auc_m1
auc_m2
summary(xresults_all$AUC)
sd(xresults_all$AUC)

tcheck(desc='Completed run')
