if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

source('bosch_plp_util.R')
tcheck(0)

train_long <- readRDS(file = "../data/train_numeric_long.rds"); tcheck(desc='read_rds')
metric_cnt <- train_long %>% count(metric); tcheck(desc='count_metrics')
fnames <- metric_cnt$metri[1:(nrow(metric_cnt)-1)]  #drop the Response column

#how many lines, stations, features
coverage <- str_split(fnames, "_") %>% as.data.table() %>% t() %>% as.data.table()
names(coverage) <- c("Line", "Station", "Feature")
coverage %>% group_by(Line) %>% summarize( nStations = length(unique(Station)), nFeatures = n()); tcheck(desc='line_counts')

#how many times does an Id occur (is it constant)?
id_cnt <- train_long %>% group_by(Id) %>% summarize( count = n()); tcheck(desc='Id counts')
summary(id_cnt$count)    # range 1-328 

#could Id metric count be a feature? (number of metrics for an Id)
setkey(train_long, Id)
setkey(id_cnt, Id)
id_cnt <- id_cnt[train_long[metric == 'Response']][, .(Id, count, Response=as.factor(value))]  
summary(id_cnt$Response)  #tiny failure rate ... 0.58 %

id_cnt %>% 
    ggplot( aes( Response, count)) + geom_boxplot()  
tcheck(desc='id metric count plot')


