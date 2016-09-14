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
df <- trnw
cv_cols <- setdiff(names(df), c('Id', 'Response'))
cv_rows <- 1:nrow(df)
xgb.train <- xgb.DMatrix( dropNA(as.matrix(df)[cv_rows, cv_cols]), label = df[cv_rows, Response], missing = 99 )
xgb.cv( xgb.train, nrounds = 500, params = xgb_params, nfold=5, verbose = 1, early.stop.round = 10 )
