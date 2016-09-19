# simple script based on forum post from F.J.Martinez-de-Pison
# https://www.kaggle.com/c/bosch-production-line-performance/forums/t/23135/questions-about-categorical-csv

# Read columns
library(data.table)
NUM.ROWS <- 100000
train_data_cat <- fread(input = "../input/train_categorical.csv",
                        nrows = NUM.ROWS,
                        select=2:2140,
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = FALSE,
                        na.strings="",
                        colClasses = "character",
                        data.table=FALSE)

categories <- train_data_cat[!is.na(train_data_cat)]
unique(categories)

numeric.cat <- as.numeric(gsub("T","",unique(categories)))
range(numeric.cat) # -2147483648    33554944
# numeric.cat[numeric.cat<0] <- (2^32)+numeric.cat[numeric.cat<0]
# print(as.hexmode(numeric.cat))
numeric.cat.low <- numeric.cat[numeric.cat<=2147483647]
print(as.hexmode(numeric.cat.low))

# Remove more significat bit in big numbers to look them
numeric.cat.high <- numeric.cat[numeric.cat>2147483647]
numeric.cat.high <- numeric.cat.high - 2^31
print(as.hexmode(numeric.cat.high))
