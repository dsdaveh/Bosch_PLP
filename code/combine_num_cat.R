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
w1 <- 0.23815 # num50m1 LB score
w2 <- 0.14841 # cat50m1 LB score
comb50m1 <- data.table( Id = num50m1$Id,
                        y1_avg_prob = (    num50m1$mean_prob +      cat50m1$mean_prob) / 2,
                        y1_wt_prob = ( w1 * num50m1$mean_prob + w2 * cat50m1$mean_prob) / (w1 + w2))
cutoff_wt <- find_cutoff_by_ratio( comb50m1$y1_wt_prob, 1/171)
comb50m1$wt_pred <- as.integer( comb50m1$y1_wt_prob >= cutoff_wt)
cutoff_avg <- find_cutoff_by_ratio( comb50m1$y1_avg_prob, 1/171)
comb50m1$avg_pred <- as.integer( comb50m1$y1_avg_prob >= cutoff_avg)

sum(comb50m1$wt_pred != comb50m1$avg_pred) # 882 predictions changed

date_stamp <- format(Sys.time(), "%Y_%m_%d_%H%M%S") #  
#  date_stamp = 2016_09_18_193444  ... without weighting
#  date_stamp = 2016_09_19_120108  ... w/ weighting
sfile <- sprintf("../submissions/min_obs_comb_r50_m1_%s.csv", date_stamp)
write.csv( comb50m1 %>% select(Id, Response=wt_pred), file=sfile, row.names = FALSE)

