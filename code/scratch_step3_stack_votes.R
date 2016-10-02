obs_stack_all$Response <- as.integer(obs_stack_all$Response) #precautionary
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
    objective = "binary:logistic",
    #nthreads = 4,
    verbose = 1
)
xgb_nrounds = 200

set.seed(seed)
ens_trn <- obs_stack_all[-ix_hold]
ix_trn_fail <- which( ens_trn$Response == 1 )
ix_trn_pass <- which( ens_trn$Response == 0 ) 
ix_trn <- list()
ix_trn[[1]] <- ix_trn_pass %>% sample( length(ix_trn_fail) * pass_fail_ratio)
ix_remain <- setdiff(ix_trn_pass, ix_trn[[1]])
ix_trn[[2]] <- ix_remain %>% sample( length(ix_trn_fail) * pass_fail_ratio)
ix_remain <- setdiff(ix_remain, ix_trn[[2]])
ix_trn[[3]] <- ix_remain %>% sample( length(ix_trn_fail) * pass_fail_ratio)

res_mcc <- list()
res_probs <- list()
res_model <- list()
for (i in 1:3) {
    ens_trn <- obs_stack_all[-ix_hold]
    ens_trn <- ens_trn[ c(ix_trn[[i]], ix_trn_fail)] %>% sample_frac()
    
    xgb.train <- xgb.DMatrix( dropNA(as.matrix(ens_trn)[, ens_cols]), label = ens_trn[ , Response], missing = 99 )
    xgb.oos <- xgb.DMatrix( dropNA(as.matrix(obs_stack_all)[ix_hold, ens_cols]), label = obs_stack_all[ix_hold, Response], missing = 99 )
    res_model[[i]] <- xgb.train( data=xgb.train,
                           nrounds = xgb_nrounds,
                           params = xgb_ens_params, 
                           watchlist = list(eval = xgb.oos),
                           print.every.n = 5L,
                           early.stop.round = 5L,
                           verbose = 1 )
    probs <- res_probs[[i]] <- predict( res_model[[i]], dropNA(as.matrix(obs_stack_all)[ix_hold, ens_cols]) )
    cutoff_m2 <- find_cutoff_by_ratio( probs, 1/171)
    res_mcc[[i]] <- calc_mcc( table( obs_stack_all[ix_hold, Response], as.integer(probs >= cutoff_m2)) )
}

comb_pred <- data.table( truth=obs_stack_all[ix_hold, Response],
                         model1 = as.integer( res_probs[[1]] >= find_cutoff_by_ratio( res_probs[[1]], 1/171 )),
                         model2 = as.integer( res_probs[[2]] >= find_cutoff_by_ratio( res_probs[[2]], 1/171 )),
                         model3 = as.integer( res_probs[[3]] >= find_cutoff_by_ratio( res_probs[[3]], 1/171 ))
)
comb_pred[, vote := ifelse( model1+model2+model3 >= 2, 1, 0)]
with(comb_pred, calc_mcc(table(truth, vote))) # 0.2016076

comb_prob <- res_probs[[1]] + res_probs[[2]] + res_probs[[2]] / 3
calc_mcc(table( obs_stack_all[ix_hold, Response], comb_prob >= find_cutoff_by_ratio( comb_prob, 1/171))) #0.2014084

#go with voting

res_pred <- list()
votes <- rep(0, nrow(obs_stack_tst))
for (i in 1:3) {
#     probs <- predict( res_model[[i]], dropNA(as.matrix(obs_stack_tst)[, ens_cols]) )
#     res_pred[[i]] <- as.integer( probs >= find_cutoff_by_ratio( probs, 1/171))
    votes <- votes + res_pred[[i]]
}
date_stamp <- format(Sys.time(), "%Y_%m_%d_%H%M%S")
sfile_m2 <- sprintf("../submissions/min_obs_thin_m2_stack_votes_%s.csv", date_stamp)
m2_submit <- obs_stack_tst[, .(Id, Response = ifelse(votes >= 2, 1, 0))]
m2_submit <- rbind(m2_submit, data.table( Id=guess0, Response=0))
write.csv( m2_submit, file=sfile_m2, row.names = FALSE)

# 0.2016 ... no improvement


