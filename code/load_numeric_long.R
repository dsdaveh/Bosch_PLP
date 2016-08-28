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

