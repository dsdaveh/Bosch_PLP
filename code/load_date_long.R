if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(data.table)
library(tidyr)

source('bosch_plp_util.R')
tcheck(0)

nchunk <- 10
ds_switch <- "test" # "train"
input_csv <- sprintf("../input/%s_date.csv", ds_switch)
chunk_prefix <- sprintf("../data/%s_date_long_chunk_", ds_switch)

for(i in 1:nchunk) {
    cat("Loading chunk", i, "...\n")
    chunk <- read_raw_chunk(i, input = input_csv)
    cat(nrow(chunk), "rows\n")
    chunk <- chunk %>% gather(metric, value, -Id) %>% data.table()
    chunk <- chunk[ ! is.na(value)]
    
    chunk_name <- paste0(chunk_prefix, i, ".rds")
    saveRDS(chunk, file = chunk_name)
    rm(chunk)
    gc(verbose = FALSE)
    
    tcheck(desc='loaded chunk')

    skip_rows <- skip_rows + chunk_size 
}
