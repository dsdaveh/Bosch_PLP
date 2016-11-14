library(ROCR)
source('bosch_plp_util.R')  # defines performance_double

#family_results <- readRDS(file = '../data/f2_xtrain_results_nmcAllff345.rds') 
shen <- fread('../data/cv_preds_mf.csv')
setkey(family_results, Id)
setkey(shen, Id)
shen <- shen[, .(Id, shen_prob=Response)]

family_results$dave_prob <- rowMeans( as.matrix( family_results %>% select(starts_with('prob'))))
family_results <- family_results[shen, nomatch=FALSE]

shen_preds <- prediction( family_results$shen_prob, family_results$Response )
dave_preds <- prediction( family_results$dave_prob, family_results$Response )
auc_shen <- performance(shen_preds, "auc")@y.values[[1]]
auc_dave <- performance(dave_preds, "auc")@y.values[[1]]
shen_perf <- performance(shen_preds, "tpr", "fpr")
dave_perf <- performance(dave_preds, "tpr", "fpr")
plot(shen_perf)
plot(dave_perf, col='red', add=TRUE)
legend("bottomright", c('Shen', 'Dave'), col=c('black', 'red'), lty=1)
shen_mcc <- performance( shen_preds, "mat")
dave_mcc <- performance( dave_preds, "mat")
mcc_vals.shen <- shen_mcc@y.values[[1]]
mcc_cuts.shen <- shen_mcc@x.values[[1]]
mcc_vals.dave <- dave_mcc@y.values[[1]]
mcc_cuts.dave <- dave_mcc@x.values[[1]]
mcc_shen <- max(mcc_vals.shen, na.rm=TRUE )
mcc_dave <- max(mcc_vals.dave, na.rm=TRUE )
plot(shen_mcc)
lines(mcc_cuts.dave, mcc_vals.dave, col='red')
legend("bottomright", c('Shen', 'Dave'), col=c('black', 'red'), lty=1)

blend_prob <- (family_results$shen_prob * auc_shen + 
               family_results$dave_prob * auc_dave) / 2
blend_preds <- prediction( blend_prob, family_results$Response )
auc_blend <- performance(blend_preds, "auc")@y.values[[1]]

par(mfrow=c(1,2))
blend_mcc <- performance(blend_preds, "mat")
mcc_vals.blend <- blend_mcc@y.values[[1]]
mcc_cuts.blend <- blend_mcc@x.values[[1]]
mcc_blend <- max(mcc_vals.blend, na.rm=TRUE )
plot(blend_mcc)
lines(mcc_cuts.shen, mcc_vals.shen, col='blue')
lines(mcc_cuts.dave, mcc_vals.dave, col='red')

blend_perf <- performance(blend_preds, "tpr", "fpr")
plot(blend_perf)
plot(shen_perf, col='blue', add=TRUE)
plot(dave_perf, col='red', add=TRUE)
legend("bottomright", c('blend', 'Shen', 'Dave'), col=c('black', 'blue', 'red'), lty=1, lwd=2 )
par(mfrow=c(1,1))

cutoff_blend <- mcc_cuts.blend[ which.max(mcc_vals.blend)]
cutoff_shen <- mcc_cuts.shen[ which.max(mcc_vals.shen)]
cutoff_dave <- mcc_cuts.dave[ which.max(mcc_vals.dave)]

calc_mcc( table(  family_results$Response , as.integer(blend_prob > cutoff_blend)))

family_results$blend_prob <- blend_prob

family_results[, c('prob_1', 'mcc_best_1', 'mcc_ratio_1') := NULL]
family_results[, dave_pred := as.integer(dave_prob > cutoff_dave)]
family_results[, shen_pred := as.integer(shen_prob > cutoff_shen)]
family_results[, blend_pred := as.integer(blend_prob > cutoff_blend)]
family_results[, either := as.integer( dave_pred + shen_pred > 0)]

with(family_results, calc_mcc( table( Response, dave_pred)))
with(family_results, calc_mcc( table( Response, shen_pred)))
with(family_results, calc_mcc( table( Response, blend_pred)))
with(family_results, calc_mcc( table( Response, either)))
cutoff_blend

family_results[, sum(dave_pred)]
family_results[, sum(shen_pred)]
family_results[, sum(blend_pred)]
family_results[, sum(either)]
