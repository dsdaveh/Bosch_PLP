library(data.table)
library(tidyr)

train_long <- data.table()
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
                             

    train_long <- rbind(train_long, train_data_temp[! is.na(value)] )
    if (i %% 10 == 0) { saveRDS(train_long, file = "../data/train_numeric_long.rds"); cat('...saving...\n') }
    t2 <- proc.time()
    cat('...elapsed', (t2-t1)[3], '\n' )
    t1 <- t2

}

gc()
saveRDS(train_long, file = "../data/train_numeric_long.rds")
# train_long <- readRDS(file = "../data/train_numeric_long.rds")

cat('total elapsed', (t1-t0)[3], '\n' )

#probably should have broken it up during initial read and assembled chunks, but its done now
nchunk <- 10
chunk_prefix <- "../data/train_numeric_long_chunk_"
ids <- sort(unique(train_long$Id))
chunk_size <- ceiling( length(ids) / 10)
setkey(train_long, Id)

for( i in 2:nchunk) {
    id_last <- ids[chunk_size]
    ids <- ids[-(1:chunk_size)]
    chunk <- train_long[ Id <= id_last ]
    chunk_name <- paste0(chunk_prefix, i-1, ".rds")
    saveRDS(chunk, file = chunk_name)
    rm(chunk)
    gc(verbose = FALSE)
    train_long <- train_long[ Id > id_last ]
    gc()
    t2 <- proc.time()
    cat('...elapsed',chunk_name, (t2-t1)[3], '\n' )
    t1 <- t2
}
chunk_name <- paste0(chunk_prefix, i, ".rds")
saveRDS(train_long, file = paste0(chunk_prefix, i, ".rds"))
t2 <- proc.time()
cat('...elapsed',chunk_name, (t2-t1)[3], '\n' )
rm( chunk_name, train_long, ids)
