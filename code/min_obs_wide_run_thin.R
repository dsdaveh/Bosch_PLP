library(tidyr)

tcheck.print = TRUE
tcheck(0)

## parameters
# ichunk contolled in for loops
pass_fail_ratio <- 200
##
results <- list()
for (ichunk in 1:10) {
    source('min_obs_wide_study.R')
    results <- c(results, chunk_results)
}
save(results, file='../data/min_obs_thin.RData')
tcheck.print = FALSE

fea_diffs <- list()
ix <- 5
fea_rank <- results[[ix]] %>% select( Feature, Gain.1 = Gain )

for(i in 2:10) {
    imp2 <- results[[(i-1)*6 + ix]] %>% select( Feature, Gain)
    names(imp2) <- c("Feature", sprintf("Gain.%d", i))
    fea_diffs[[length(fea_diffs)+1]] <- setdiff( imp2$Feature, fea_rank$Feature)
    fea_rank <- merge( fea_rank, imp2, by="Feature", all=TRUE)
}
fea_rank$Gain.sum <- fea_rank %>% select(-Feature) %>% as.matrix() %>% rowSums(na.rm=TRUE)
fea_top30 <- fea_rank %>% arrange(desc(Gain.sum)) %>% dplyr::slice(1:30)
fea_top30 %>% ggplot( aes(reorder(Feature,Gain.sum), Gain.sum)) +
    geom_bar(stat="identity", position='identity') + coord_flip()

fea_top30 %>% select( -Feature, -Gain.sum) %>% gather(model, Gain) %>%
    ggplot( aes(model, Gain, group=model)) + geom_bar(stat="sum")


xresults_all <- data.frame()
for (ichunk in 1:10) {
    source('min_obs_wide_study_xrun.R')
    xresults_all <-rbind(xresults_all, xresults)
}
save(xresults_all, file='../data/min_obs_thin_xrun.RData')

summary(xresults_all$MCC)
summary(xresults_all$cutoff)

xresults_all <- xresults_all %>% mutate (mregion = ifelse( ichunk <= 3, "low", ifelse( ichunk >= 8, "high", "mid")))
xresults_all %>% mutate(ichunk=as.factor(ichunk)) %>% 
    ggplot( aes(imodel, MCC, group=ichunk, color=ichunk)) + geom_line() +
    geom_smooth(method="lm") +
    facet_wrap( ~mregion) + ggtitle("MCC for all models by chunk (region) model was trained on")

