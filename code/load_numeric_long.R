library(data.table)
library(tidyr)

train_long <- data.table()

nbatch <- 50
nsize <- round(970/(nbatch))
col_first <- seq(2, 970, by=nsize )
col_last <- col_first + nsize - 1
t0 <- t1 <- proc.time()
for(i in seq_along(col_first)) {
    
    cat("Loading ", i, "th part.\n", sep = "")
    train_data_temp <- fread(input = "../input/train_numeric.csv",
                             select = c(1, col_first[i]:col_last[i]),
                             header = TRUE,
                             sep = ",",
                             stringsAsFactors = FALSE,
                             colClasses = rep("numeric", 970),
                             data.table = TRUE) %>% 
        gather(metric, value, -Id) %>% data.table()
    gc(verbose = FALSE)
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
