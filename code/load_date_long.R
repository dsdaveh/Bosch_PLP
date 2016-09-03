library(data.table)
library(tidyr)

nrows <- n_remaining <- 1183748 - 1
nchunk <- 10
chunk_prefix <- "../data/train_date_long_chunk_"
chunk_size <- ceiling( nrows / 10)

t0 <- t1 <- proc.time()

header <- fread(input = "../input/train_date.csv", nrows=1, header=TRUE)

skip_rows <- 1
for(i in 1:nchunk) {
    cat("Loading ", i, "th part.\n", sep = "")
    chunk <- fread(input = "../input/train_date.csv", skip=skip_rows, header=FALSE,
                             nrows = chunk_size )
    read_rows <- nrow(chunk)
    names(chunk) <- names(header)
    chunk <- chunk %>% gather(metric, value, -Id) %>% data.table()
    chunk <- chunk[ ! is.na(value)]
    
    chunk_name <- paste0(chunk_prefix, i, ".rds")
    saveRDS(chunk, file = chunk_name)
    rm(chunk)
    gc(verbose = FALSE)
    
    t2 <- proc.time()
    cat('...elapsed', (t2-t1)[3], read_rows,'rows\n' )
    t1 <- t2
    
    skip_rows <- skip_rows + chunk_size 
}

