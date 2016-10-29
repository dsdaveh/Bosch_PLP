library(dplyr)
library(tidyr)
library(ggvis)

#lets get the models prior to using just top 25
oos_chunk_results <- readRDS(file='../data/f2_xtrain_models_nmc50.rds')

#run f2_xtrain.R: 178-202 to get vars by importance
#L0_S16 is one of the parallel stations (w/ S17) and was just out of the top 25
#S12 is in top 25 and all parts pass through that
# lets focus on these 3 stations

#data refresh
trnw <- trnw.f2
# remove duplicates (copied from f2_xtrain)
chk_cols <- setdiff( names(trnw), c("Id", names(trnw)[grepl("magic", names(trnw))]))
dup_rows <- duplicated(data.frame(trnw)[ ,chk_cols])
trnw <- trnw[! dup_rows]
# end

#S12 features
s12_fea <- names(trnw)[grepl("L0_S12", names(trnw))]
# [1] "L0_S12_F330" "L0_S12_F332" "L0_S12_F334" "L0_S12_F336" "L0_S12_F338" "L0_S12_F340" "L0_S12_F342"
# [8] "L0_S12_F344" "L0_S12_F346" "L0_S12_F348" "L0_S12_F350" "L0_S12_F352"
# xgb selected 

#s16 features
s16_fea <- names(trnw)[grepl("L0_S16", names(trnw))]
#[1] "L0_S16_F421" "L0_S16_F426" "L0_S16_F422" "L0_S16_F424" "L0_S16_F425"

#s17 features
s17_fea <- names(trnw)[grepl("L0_S17", names(trnw))]
# [1] "L0_S17_F431" "L0_S17_F433"

## list features that were used in the last run
intersect(s12_fea, reduce_cols)
# [1] "L0_S12_F330" "L0_S12_F346" "L0_S12_F350"

wip <- trnw[, c('Id', 'Response', s16_fea, s17_fea), with=F]
summary(wip)

wip %>% ggplot(aes(L0_S16_F421)) + geom_boxplot()
boxplot(wip$L0_S16_F421 ~ wip$Response )
plot(density(wip$L0_S16_F421, na.rm=T))

wip.long <- wip %>% tbl_df() %>% gather( station, value, -c(Id, Response)) %>% filter(! is.na(value)) 
wip.long %>% mutate(Response = as.factor(Response)) %>% 
    filter(station == 'L0_S16_F421') %>% group_by(Response) %>%
    ggvis( ~value, fill := ~Response) %>% layer_densities()

wip.long %>% mutate(Response = as.factor(Response)) %>%
    filter(station == 'L0_S16_F421') %>%
    ggplot( aes(value, group=Response, fill=Response)) + geom_density(alpha=0.5) 

wip.long %>% mutate(Response = as.factor(Response)) %>%
    filter(station == 'L0_S16_F426') %>%
    ggplot( aes(value, group=Response, fill=Response)) + geom_density(alpha=0.5) 

wip.long %>% mutate(Response = as.factor(Response)) %>%
    filter(station != 'L0_S16_F426') %>%
    ggplot( aes(value, group=Response, fill=Response)) + geom_density(alpha=0.5) +
    facet_wrap( ~ station)

i=1
i=i+1; fea=s17_fea[i]
wip.long %>% mutate(Response = as.factor(Response)) %>%
    filter(station == fea) %>%
    ggplot( aes(value, group=Response, fill=Response)) + geom_density(alpha=0.5) 

