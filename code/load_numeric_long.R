library(data.table)
library(tidyr)

#save the response into a separate file for easy lookup
truth <- fread(input = "../input/train_numeric.csv", 
               select = c(1, 970), colClasses = c("integer", rep("numeric", 968), "factor"))
saveRDS(truth, file="../data/train_response.rds")
nrows <- nrow(truth) # 1183747
ids_fail <- truth[ Response == '1']$Id
rm(truth)

nchunk <- 10
chunk_prefix <- "../data/train_numeric_long_chunk_"
chunk_size <- ceiling( nrows / 10)

t0 <- t1 <- proc.time()

header <- fread(input = "../input/train_numeric.csv", nrows=1, header=TRUE)

fail_obs <- data.table()

skip_rows <- 1
for(i in 1:nchunk) {
    cat("Loading ", i, "th part.\n", sep = "")
    chunk <- fread(input = "../input/train_numeric.csv", skip=skip_rows, header=FALSE,
                             nrows = chunk_size )
    saveRDS(chunk, file=sprintf('../data/train_numeric_raw_chunk%d.rds', i))
    read_rows <- nrow(chunk)
    names(chunk) <- names(header)
    
    fail_obs <- rbind(fail_obs, chunk[ Id %in% ids_fail])
    
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
save(fail_obs, file = '../data/fail_obs.RData')
cat ("Total time", (t2-t0)[3], "\n")
rm(fail_obs, header, chunk_prefix, skip_rows, i, chunk_size, chunk_name, nrows, t0, t1, t2)
gc()
