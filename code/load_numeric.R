library(data.table)
library(recommenderlab)

for (i in 1:97) {
    
    cat("Loading ", i, "th part.\n", sep = "")
    train_data_temp <- fread(input = "../input/train_numeric.csv",
                             select = (10*(i-1)+1):(10*i),
                             header = TRUE,
                             sep = ",",
                             stringsAsFactors = FALSE,
                             colClasses = rep("numeric", 970),
                             data.table = TRUE)
    gc(verbose = FALSE)
    
    if (i > 1) {
        
        cat("Coercing to matrix.\n", sep = "")
        train_numeric_temp <- as.matrix(train_data_temp)
        rm(train_data_temp)
        gc(verbose = FALSE)
        
        cat("Coercing into dgCMatrix with NA as blank.\n", sep = "")
        train_numeric_temp <- dropNA(train_numeric_temp)
        gc(verbose = FALSE)
        
        cat("Column binding the full matrix with the newly created matrix.\n", sep = "")
        train_numeric <- cbind(train_numeric, train_numeric_temp)
        rm(train_numeric_temp)
        gc(verbose = FALSE)
        
    } else {
        
        cat("Coercing to matrix.\n", sep = "")
        train_numeric_temp <- as.matrix(train_data_temp)
        rm(train_data_temp)
        gc(verbose = FALSE)
        
        cat("Coercing into dgCMatrix with NA as blank.\n", sep = "")
        train_numeric <- dropNA(train_numeric_temp)
        gc(verbose = FALSE)
        
    }
    
}

gc()
saveRDS(train_numeric, file = "../data/train_numeric.rds")
# train_numeric <- readRDS(file = "../data/train_numeric.rds")