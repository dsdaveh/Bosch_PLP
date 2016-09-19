# script to identify flow paths of parts
# change nrows = 100L as needed

########################################
##### aggregate at the Station level ###
########################################

library(data.table)

# get categorical data
catcols <- fread("./input/train_categorical.csv", nrows = 0L, skip = 0L)  
classvector <- c("integer", rep.int("character", ncol(catcols)-1)) 
cats <- fread("./input/train_categorical.csv", colClasses = classvector, na.strings=""
              , stringsAsFactors=FALSE, nrows = 100L, skip=0L
              )  

# reshape and get stations for aggregating
cats2 = melt(cats, 'Id', variable.name='feature',  variable.factor = FALSE, value.name='measurement')
cats2[, measurement := gsub("T", "", measurement)]
cats2[, measurement := as.numeric(measurement)] 
cats2[, station := substr(feature, 1L, 6L)]
cats2[, feature := NULL] 

# get numeric data
numcols <- fread("./input/train_numeric.csv", nrows = 0L, skip = 0L)  
classvector <- c("integer", rep.int("numeric", ncol(numcols)-1)) 
nums <- fread("./input/train_numeric.csv", colClasses = classvector, na.strings="", stringsAsFactors=FALSE, 
              nrows = 100L, skip=0L
              )

# reshape and get stations for aggregating
resps <- nums[, .(Id, Response)] # saving this for later
nums[, Response := NULL] 

nums2 = melt(nums, 'Id', variable.name='feature',  variable.factor = FALSE, value.name='measurement')
nums2[, station := substr(feature, 1L, 6L)]
nums2[, feature := NULL]

# put it together into one long skinny table
cats2 <- rbind(cats2, nums2)

# aggregate at the station level
partssum <- cats2[, .(meas = mean(as.numeric(measurement), na.rm = TRUE)), by= .(station, Id)]

#re-reshape and clean up
partssum =  dcast(partssum, Id ~ station, value.var="meas")
setnames(partssum, c("L0_S0_", "L0_S1_", "L0_S2_", "L0_S3_", "L0_S4_", "L0_S5_"
                 , "L0_S6_" , "L0_S7_", "L0_S8_", "L0_S9_")
               , c("L0_S00", "L0_S01", "L0_S02", "L0_S03", "L0_S04", "L0_S05" 
                 , "L0_S06", "L0_S07", "L0_S08", "L0_S09")
        )


#####################################
#### determine unique flow paths ####       
#####################################

library(tidyr)

# convert everything to 1s and 0s
Id <- partssum[, Id]
partssum[!is.na(partssum)] <- 1
partssum[is.na(partssum)] <- 0
partssum <- cbind(Id, partssum[, 2:53, with = FALSE])

# concatenate the 1s and 0s
paths <- unite(partssum, path, L0_S00:L3_S51, sep = "", remove = TRUE)

# add responses
paths <- merge(resps, paths, by = "Id")

# count the 1s
stations <- (paths$path)
g2 <- sapply(regmatches(stations, gregexpr("1", stations)), length)
paths[, stationcount := g2]

# aggregate by path
flowpaths <- paths[, .(count = .N
                       , fails = sum(Response)
                       , failpct = sum(Response)/.N
                       ), 
                      by = path
                   ]

fwrite(flowpaths, "flowpaths.csv")
