xgb_params <- list( 
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

xgb.train <- xgb.DMatrix( dropNA(as.matrix(obs_stack_all)[-ix_hold, ens_cols]), label = obs_stack_all[-ix_hold, Response], missing = 99 )
xgb.cv( xgb.train, nrounds = 500, params = xgb_params, nfold=5, verbose = 1, early.stop.round = 10 )
