if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

source('bosch_plp_util.R')

##############  
input_csv <- '../input/test_numeric.csv'
col_names <- fread(input_csv, nrows=0L) %>% names()
f1_col <- col_names[ grepl("L0_S0", col_names)][1]
f2_col <- col_names[ grepl("L0_S12", col_names)][1]
f3_cola <- col_names[ grepl("L1_S24", col_names)][1]
f3_colb <- col_names[ grepl("L1_S25", col_names)][1]
fcols <- c( f1_col, f2_col, f3_cola, f3_colb)
#trn_fcols <- fread(input_csv, select=c("Id", "Response", fcols) )
#################
cols_used <- oos_chunk_results[[1]]$cols_used   # identical for all models

dt.f2 <- data.table()
for (ichunk in 1:10) {
    dt <- read_raw_chunk(ichunk, input= input_csv)
    dt <- dt[ ! is.na(L0_S12_F330) ]
    dt.f2 <- rbind(dt.f2, dt)
    print(ichunk)
}
rm(dt, ichunk)

dim(dt.f2) # 242415    969
object.size(dt.f2) /1e9 # 1.86 GB

dt.f2$F1 <- ! is.na( dt.f2[ , f1_col, with=FALSE])
dt.f2$F2 <- ! is.na( dt.f2[ , f2_col, with=FALSE])
dt.f2$F3 <- ! is.na( dt.f2[ , f3_cola, with=FALSE]) | ! is.na( dt.f2[ , f3_colb, with=FALSE])
dt.f2[, F12 := F1 & F2]
dt.f2[, F13 := F1 & F3]
dt.f2[, F23 := F2 & F3]
dt.f2[, pure := F1 + F2 + F3 == 1]
dt.f2[, F4 := F1 + F2 + F3 == 0 ]

dt.f2.info <- dt.f2 %>% select( starts_with("F")) %>% 
    gather( family, val ) %>% filter( val == TRUE ) %>% data.frame()

dt.f2.info %>%
    ggplot( aes( family)) + geom_bar() + ggtitle ('Families in training_ data')
dt.f2.info %>% count( family, sort = TRUE )

dt.f2 <- dt.f2[ F2 == TRUE & pure == TRUE ]

dim(dt.f2) #  241399    977
object.size(dt.f2) /1e9 # 1.86 GB

purify_station_data_2 <- function( dt, Sa, Sb) {
    Sac <- names(dt)[ grepl(Sa, names(dt)) ]; length(Sac)  
    Sbc <- names(dt)[ grepl(Sb, names(dt)) ]; length(Sbc)
    stopifnot( length(Sac) == length(Sbc))
    
    ix_Sa <- which( ! is.na( dt[, (Sac[1]), with=FALSE ]))
    ix_Sb <- which( ! is.na( dt[, (Sbc[1]), with=FALSE ]))
    both <- intersect( ix_Sa, ix_Sb)
    ix_Sa <- setdiff( ix_Sa, both)
    ix_Sb <- setdiff( ix_Sb, both)
    neither <- setdiff(1:nrow(dt), c(ix_Sa, ix_Sb, both))
    
    cat(length(ix_Sa), length(ix_Sb), length(both), length(neither))# 
    #remove special cases
    dt <- dt[ -c(both, neither)]
    return(dt)
}

purify_station_data_3 <- function( dt, Sa, Sb, Sc) {
    Sac <- names(dt)[ grepl(Sa, names(dt)) ]
    Sbc <- names(dt)[ grepl(Sb, names(dt)) ]
    Scc <- names(dt)[ grepl(Sc, names(dt)) ]  
    stopifnot( length(Sac) == length(Sbc))
    stopifnot( length(Scc) == length(Sbc))
    
    ix_Sa <- which( ! is.na( dt[, (Sac[1]), with=FALSE ]))
    ix_Sb <- which( ! is.na( dt[, (Sbc[1]), with=FALSE ]))
    ix_Sc <- which( ! is.na( dt[, (Scc[1]), with=FALSE ]))
    both_ab <- intersect( ix_Sa, ix_Sb)
    both_bc <- intersect( ix_Sb, ix_Sc)
    both_ac <- intersect( ix_Sa, ix_Sc)
    both <- unique( c(both_ab, both_bc, both_bc))
    
    ix_Sa <- setdiff( ix_Sa, both)
    ix_Sb <- setdiff( ix_Sb, both)
    ix_Sc <- setdiff( ix_Sc, both)
    neither <- setdiff(1:nrow(dt), c(ix_Sa, ix_Sb, ix_Sc, both))
    
    cat(length(ix_Sa), length(ix_Sb), length(ix_Sc), length(both), length(neither))# 81080 79915 80120 99 0
    #remove special cases
    dt <- dt[ -c(both, neither)]
    return(dt)
}

#L0_S14 and L0_S15 are parallel
dt.f2 <- purify_station_data_2(dt.f2, 'L0_S14', 'L0_S15')# 120525 120725 64 85

#L0_S16 and L0_S17 are parallel
dt.f2 <- purify_station_data_2(dt.f2, 'L0_S16', 'L0_S17')# 118932 122245 68 5

#L0_S18 and L0_S19 are parallel
dt.f2 <- purify_station_data_2(dt.f2, 'L0_S18', 'L0_S19')# 120551 120624 2 0

#L0_S21 and L0_S22 and L0_S23 are parallel
dt.f2 <- purify_station_data_3(dt.f2, 'L0_S21', 'L0_S22', 'L0_S23')# 81080 79915 80120 99 0

# #L0_S35 and L0_S36 MIGHT BE IN parallel  (CATEGORICAL ?)

dim(dt.f2) #  241076    977
object.size(dt.f2) /1e9 # 1.86 GB

######################### now consolidate

combine_stations_2 <- function( dt, Sa, Sb) {
    Sac <- names(dt)[ grepl(Sa, names(dt)) ]
    Sbc <- names(dt)[ grepl(Sb, names(dt)) ]
    stopifnot( length(Sac) == length(Sbc))
    
    ix_Sa <- which( ! is.na( dt[, (Sac[1]), with=FALSE ]))
    ix_Sb <- which( ! is.na( dt[, (Sbc[1]), with=FALSE ]))
    
    comb_names <- paste( 'C', Sa, Sb, 1:length(Sac), sep = '_')
    
    # dt.f2[ix_Sa, (comb_names[i]) := Sac[i], with=FALSE ] #this doesn't work like I expect
    oha <- paste0('oh_', Sa)
    ohb <- paste0('oh_', Sb)
    dt[ , c(oha, ohb) := FALSE]
    for (i in 1:length(Sac)) dt[ix_Sa, (comb_names[i]) := dt[ix_Sa,(Sac[i]), with=FALSE] ][ix_Sa, (oha) := TRUE]
    for (i in 1:length(Sac)) dt[ix_Sb, (comb_names[i]) := dt[ix_Sb,(Sbc[i]), with=FALSE] ][ix_Sb, (ohb) := TRUE]
    dt[, c(Sac, Sbc) := NULL]
    return(dt)
}

combine_stations_3 <- function( dt, Sa, Sb, Sc) {
    Sac <- names(dt)[ grepl(Sa, names(dt)) ]
    Sbc <- names(dt)[ grepl(Sb, names(dt)) ]
    Scc <- names(dt)[ grepl(Sc, names(dt)) ]
    stopifnot( length(Sac) == length(Sbc))
    stopifnot( length(Scc) == length(Sbc))
    
    ix_Sa <- which( ! is.na( dt[, (Sac[1]), with=FALSE ]))
    ix_Sb <- which( ! is.na( dt[, (Sbc[1]), with=FALSE ]))
    ix_Sc <- which( ! is.na( dt[, (Scc[1]), with=FALSE ]))
    
    comb_names <- paste( 'C', Sa, Sb, Sc, 1:length(Sac), sep = '_')
    
    # dt.f2[ix_Sa, (comb_names[i]) := Sac[i], with=FALSE ] #this doesn't work like I expect
    oha <- paste0('oh_', Sa)
    ohb <- paste0('oh_', Sb)
    ohc <- paste0('oh_', Sc)
    dt[ , c(oha, ohb, ohc) := FALSE]
    for (i in 1:length(Sac)) dt[ix_Sa, (comb_names[i]) := dt[ix_Sa,(Sac[i]), with=FALSE] ][ix_Sa, (oha) := TRUE]
    for (i in 1:length(Sac)) dt[ix_Sb, (comb_names[i]) := dt[ix_Sb,(Sbc[i]), with=FALSE] ][ix_Sb, (ohb) := TRUE]
    for (i in 1:length(Sac)) dt[ix_Sc, (comb_names[i]) := dt[ix_Sc,(Scc[i]), with=FALSE] ][ix_Sc, (ohc) := TRUE]
    dt[, c(Sac, Sbc, Scc) := NULL]
    return(dt)
}

#L0_S14 and L0_S15 are parallel
dt.f2 <- combine_stations_2( dt.f2, 'L0_S14', 'L0_S15')

#L0_S16 and L0_S17 are parallel
dt.f2 <- combine_stations_2( dt.f2, 'L0_S16', 'L0_S17')

#L0_S18 and L0_S19 are parallel
dt.f2 <- combine_stations_2( dt.f2, 'L0_S18', 'L0_S19')

#L0_S21 and L0_S22 and L0_23 are parallel
dt.f2 <- combine_stations_3( dt.f2, 'L0_S21', 'L0_S22', 'L0_S23')

dim(dt.f2) #  241076    944
object.size(dt.f2) /1e9 # 1.79 GB

keep_cols <- intersect( c('Id', cols_used), names(dt.f2))
dt.f2 <- dt.f2[ ,keep_cols, with=FALSE]
dim(dt.f2) #  241076    634
object.size(dt.f2) /1e9 # 1.19 GB

