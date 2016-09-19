# combine numerical and categorical runs for 50:1 ratios

library(data.table)
library(dplyr)

source('bosch_plp_util.R')

#simple means
#read the numerical 
num50m1 <- readRDS(file='../data/min_obs_thin_submit_m1_2016_09_18_163805.rds')
cat50m1 <- readRDS(file='../data/min_obs_thin_submit_m1_2016_09_17_211127_recon.rds') 
all( num50m1$Id == cat50m1$Id ) # TRUE (check)
cor( num50m1$mean_prob, cat50m1$mean_prob) # 0.6324277
comb50m1 <- data.table( Id = num50m1$Id,
                        y1_mean_prob = (num50m1$mean_prob + cat50m1$mean_prob) / 2 )
cutoff_m1 <- find_cutoff_by_ratio( comb50m1$y1_mean_prob, 1/171)
comb50m1$prob_pred <- as.integer( comb50m1$y1_mean_prob >= cutoff_m1 )

date_stamp <- format(Sys.time(), "%Y_%m_%d_%H%M%S") #  2016_09_18_193444
sfile <- sprintf("../submissions/min_obs_comb_r50_m1_%s.csv", date_stamp)
write.csv( comb50m1 %>% select(Id, Response=prob_pred), file=sfile, row.names = FALSE)

