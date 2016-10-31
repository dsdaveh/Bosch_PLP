#HD Outliers
#script hangs with more than 10K observations
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(dplyr)
library(tidyr)
library(ggplot2)
library(HDoutliers)

source('bosch_plp_util.R')

#run f2_xtrain 30:58 to get trnw  TODO: this should be a function add_features
trnw <- readRDS('../data/tmp_trnw_f2_wcat.rds')
trnw <- add_time_station(trnw, 'train')
setkey(trnw, min_time)

set_na_mean <- function(x) {
    if (! class(x) %in% c('numeric', 'integer')) return(x)
    ifelse( is.na(x), mean(x, na.rm=TRUE), x)
}


i <- icnt <- 0
i_step <- 10000
outliers <- integer()
HDcols <- names(trnw)[grepl('_S1[23]', names(trnw))]  # stations 12 & 13 because they have no NA's
tcheck(0)
HDplots <- list()
while (i < nrow(trnw)) {
    icnt <- icnt + 1
    i_step <- min( i_step, nrow(trnw) - i )
    HD.small <- trnw[(i+1):(i+i_step), HDcols, with=FALSE]
    HD.small[ , ix := 1:nrow(HD.small)]
#    HD.small <- lapply(HD.small, set_na_mean) %>% data.frame()
#     for(icol in 1:ncol(HD.small)) if(grepl('_cF', names(HD.small)[icol]) ) {
#         vals <- HD.small[, icol]
#         HD.small[, icol] <- as.factor( ifelse(is.na(vals), -9999999, vals))
#     }
    
    #this allows for processing other stations as long as na records are consisitent
    nulls <- is.na( data.frame(HD.small)[,1] )  
    HD.small <- HD.small[ ! nulls ]    
    
    out.HD <- HDoutliers(HD.small %>% select(-ix), alpha = 0.20) 
    isOut <- rep(FALSE, nrow(HD.small))
    isOut[out.HD] <- TRUE
    HD.small$isOut <- isOut
    HD.small[ , IdOut := as.factor(ifelse( isOut, ix, 'Inlier')) ]
    HD.long <- gather(HD.small, fea, value, -c(ix, isOut, IdOut)) 
    HDplots[[icnt]] <- HD.long %>% 
        mutate(size = (isOut + 1)^2) %>%
        ggplot(aes(fea, value, shape=isOut, size=size, color=IdOut)) + 
        geom_point(alpha = 0.3) +
        ggtitle(sprintf('HD Outliers for %d:%d', i+1, i+i_step)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        geom_line(data = HD.long %>% filter(isOut),  aes(fea, value, size=1, group=IdOut)) +
        scale_size_continuous(guide=FALSE) +
        scale_shape_discrete(guide=FALSE)
    tcheck(desc=sprintf('HD[%d:%d] : %d outliers', i+1, i+i_step, length(out.HD)))
    outliers <- c(outliers, out.HD + i)
    i <- i + i_step
}

length(outliers) #76
trnw[outliers, sum(Response)] #0  -- meh!


