if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))


library(data.table)
library(dplyr)
library(stringr)

trnl_date <- data.table()
for (i in 1:10) {
    trnl_date <- rbind(
        trnl_date,
        readRDS(file = sprintf("../data/train_date_long_chunk_%d.rds", i)) ) # see 'load_date_long.R'
    setkey(trnl_date, Id)
    trnl_date <- trnl_date[ Id %in% trnw$Id ] ; gc()
}
tcheck( desc = 'loaded date dataset (long)')

## stolen from date eda
id_cnt <- trnl_date[
    , station := str_extract(metric, "S\\d+")][               #extract station ID
        , .(station_metric_count=.N,
            time_in = min(value), time_out = max(value)), by=c("Id", "station")][     #rollup by station
                , .(station_count = .N, metric_count = sum(station_metric_count),
                    min_time = min(time_in), max_time = max(time_out)), by=Id ][  #rollup by Id
                        , proc_time := max_time - min_time]
rm(trnl_date); gc()
setkey(id_cnt, Id)

saveRDS(id_cnt, file='../data/train_date_station_features.rds')
