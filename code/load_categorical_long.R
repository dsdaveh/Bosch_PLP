if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(data.table)
library(tidyr)

source('bosch_plp_util.R')

nchunk <- 10
chunk_prefix <- "../data/train_categorical_long_chunk_"
for(i in 1:nchunk) {
    chunk <- read_raw_chunk(i, input="../input/train_categorical.csv"); tcheck(desc=paste0('chunk', i))
}
rm(chunk, chunk_prefix)
gc()
