if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(data.table)
library(dplyr)
library(tidyr)
source('bosch_plp_util.R')

submission <- fread('../submissions/min_obs_thin_2016_09_18_163805.csv')
setkey(submission, Id)
col_names <- fread('../input/test_numeric.csv', nrows=0L) %>% names()
f1_col <- col_names[ grepl("L0_S0", col_names)][1]
f2_col <- col_names[ grepl("L0_S12", col_names)][1]
f3_cola <- col_names[ grepl("L1_S24", col_names)][1]
f3_colb <- col_names[ grepl("L1_S25", col_names)][1]
fcols <- c( f1_col, f2_col, f3_cola, f3_colb)

tst_fcols <- fread('../input/test_numeric.csv', select=c("Id", fcols) )
# f1_ix <- which( ! is.na( tst_fcols[ , f1_col, with=FALSE]) )
# f2_ix <- which( ! is.na( tst_fcols[ , f2_col, with=FALSE]) )
# f3_ix <- which( ! is.na( tst_fcols[ , f3_cola, with=FALSE]) | ! is.na( tst_fcols[ , f3_cola, with=FALSE]))
# f12_ix <- intersect( f1_ix, f2_ix)
# f23_ix <- intersect( f2_ix, f3_ix)
# f13_ix <- intersect( f1_ix, f3_ix)
# f4_ix <- setdiff( 1:nrow(tst_fcols), c(f1_ix, f2_ix, f3_ix))
# 
# f1_ix.pure <- setdiff( f1_ix, c(f13_ix, f12_ix))
# f2_ix.pure <- setdiff( f2_ix, c(f23_ix, f12_ix))
# f3_ix.pure <- setdiff( f3_ix, c(f23_ix, f13_ix))
#check
# sum( length(f1_ix.pure), length(f2_ix.pure), length(f3_ix.pure),
#      length(f12_ix), length(f23_ix), length(f13_ix), length(f4_ix)    ) ==
#     nrow(tst_fcols)


tst_fcols$F1 <- ! is.na( tst_fcols[ , f1_col, with=FALSE])
tst_fcols$F2 <- ! is.na( tst_fcols[ , f2_col, with=FALSE])
tst_fcols$F3 <- ! is.na( tst_fcols[ , f3_cola, with=FALSE]) | ! is.na( tst_fcols[ , f3_colb, with=FALSE])
tst_fcols[, F12 := F1 & F2]
tst_fcols[, F13 := F1 & F3]
tst_fcols[, F23 := F2 & F3]
tst_fcols[, pure := F1 + F2 + F3 == 1]
tst_fcols[, F4 := F1 + F2 + F3 == 0 ]

tst_fcols %>% select( starts_with("F")) %>% 
    gather( family, val ) %>% filter( val == TRUE ) %>%
    ggplot( aes( family)) + geom_bar() + ggtitle ('Families in test_ data')

tst_fcols %>% select( starts_with("F")) %>% 
    gather( family, val ) %>% filter( val == TRUE ) %>%
    count( family, sort = TRUE )

setkey( tst_fcols, Id)
ix_pure_f2 <- which( tst_fcols$F2 & tst_fcols$pure )

sum(submission[ ix_pure_f2, Response]) #1351
sum(submission[ -ix_pure_f2, Response]) #5540

submission[ -ix_pure_f2, Response := 0]
write.csv(submission, file='../submissions/f2_baseline.csv', row.names = FALSE) # 0.0930
rm(submission)
rm(tst_fcols)

ens_results_all <- readRDS(file='../data/min_obs_thin_xrun_ensemble_m1_num50.1.rds')
trn_fcols <- fread('../input/train_numeric.csv', select=c("Id", "Response", fcols) )

trn_fcols$F1 <- ! is.na( trn_fcols[ , f1_col, with=FALSE])
trn_fcols$F2 <- ! is.na( trn_fcols[ , f2_col, with=FALSE])
trn_fcols$F3 <- ! is.na( trn_fcols[ , f3_cola, with=FALSE]) | ! is.na( trn_fcols[ , f3_colb, with=FALSE])
trn_fcols[, F12 := F1 & F2]
trn_fcols[, F13 := F1 & F3]
trn_fcols[, F23 := F2 & F3]
trn_fcols[, pure := F1 + F2 + F3 == 1]
trn_fcols[, F4 := F1 + F2 + F3 == 0 ]

trn_fcols %>% select( starts_with("F")) %>% 
    gather( family, val ) %>% filter( val == TRUE ) %>%
    ggplot( aes( family)) + geom_bar() + ggtitle ('Families in training_ data')

trn_fcols %>% select( starts_with("F")) %>% 
    gather( family, val ) %>% filter( val == TRUE ) %>%
    count( family, sort = TRUE )

setkey( trn_fcols, Id)
trn_fcols <- trn_fcols[ Id %in% ens_results_all$Id ]
ix_pure_f2 <- which( trn_fcols$F2 & trn_fcols$pure )

setkey( ens_results_all, Id)
ens_results_all[ -ix_pure_f2, y_m1 := 0]

# baseline local validation (not sure what this is worth)
mcc_m1 <- calc_mcc( with(ens_results_all, table(Response, y_m1)) ) # 0.0536

# baseline perfect score
truth <- ens_results_all$Response
ens_results_all[ , truef2 := Response][-ix_pure_f2, truef2 := 0] 
mcc_best_possible <- calc_mcc( with(ens_results_all, table(Response, truef2)) ) # 0.4358



