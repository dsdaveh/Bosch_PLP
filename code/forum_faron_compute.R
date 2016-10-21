if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

# Dependencies
library(data.table)
library(Matrix)
library(caret)
library(xgboost)
library(matrixStats)

# Define file paths
date_csv <- "../input/train_date.csv"
numeric_csv <- "../input/train_numeric.csv"
test_date_csv <- "../input/test_date.csv"
test_numeric_csv <- "../input/test_numeric.csv"

# Parameters for Faron computation
number_of_chunks <- 20
n_train <- 1183747
n_test <- 1183748

# Features
numeric_features <- c('Id',
                      'L1_S24_F1604',
                      'L1_S24_F1695', 
                      'L1_S24_F1846', 
                      'L3_S29_F3407', 
                      'L3_S32_F3850',
                      'L3_S33_F3855', 
                      'L3_S33_F3865')
date_features <- c('Id',
                   'L3_S30_D3496', 
                   'L3_S30_D3501',
                   'L3_S30_D3506', 
                   'L3_S32_D3852', 
                   'L3_S33_D3856')


## 1: EXTRACT MIN DATE
## ===================

# TRAIN
print("Extract min date: TRAIN")

dt_final <- data.table()
dt_names <- names(fread(date_csv,
                        nrows = 1))
dt <- fread(date_csv,
            select = c("Id"))

# Loop through chunks
chunk_size <- floor(n_train / number_of_chunks)
for (chunk_number in 0:number_of_chunks) {
    
    # read in a chunk
    dt_chunk <- fread(date_csv,
                      skip = (chunk_size * chunk_number + 1),
                      nrows = chunk_size)
    setnames(dt_chunk, dt_names)
    
    # get stats
    dt_dateorder <- data.table(Id = dt_chunk$Id, 
                               minDate = rowMins(as.matrix(dt_chunk[, -"Id", with=F]),
                                                 na.rm = T))
    
    dt_dateorder[is.infinite(minDate), minDate := NA]
    
    # concat
    dt_final <- rbind(dt_final, dt_dateorder)

}


# TEST
print("Extract min date: TEST")

dt_final_test <- data.table()
dt_names <- names(fread(test_date_csv,
                        nrows = 1))
dt <- fread(test_date_csv,
            select = c("Id"))

# Loop through chunks
chunk_size <- floor(n_test / number_of_chunks)
for (chunk_number in 0:number_of_chunks){
    
    # read in a chunk
    dt_chunk <- fread(test_date_csv,
                      skip = (chunk_size * chunk_number + 1),
                      nrows = chunk_size)
    setnames(dt_chunk, dt_names)
    
    # get stats
    dt_dateorder <- data.table(Id = dt_chunk$Id, 
                               minDate = rowMins(as.matrix(dt_chunk[, -"Id", with=F]),
                                                 na.rm = T))
    
    dt_dateorder[is.infinite(minDate), minDate := NA]
    
    # concat
    dt_final_test <- rbind(dt_final_test, dt_dateorder)
}


## 2: FARON
## ========

print("Compute Faron features")

dt <- rbind(dt_final, dt_final_test)
rm(dt_final)
rm(dt_final_test)

setkey(dt, Id)
ord_time <- order(dt$minDate, dt$Id)

dt$magic1 <- c(9999999L, as.integer( diff(dt$Id)))
dt$magic2 <- c(-as.integer( diff(dt$Id)), 9999999L)

dt$magic3[ord_time] <- c(9999999L, as.integer(diff(dt$Id[ord_time])))
dt$magic4[ord_time] <- c(-as.integer(diff(dt$Id[ord_time])), 9999999L)

saveRDS(dt, file='../data/faron_magic4.rds')
