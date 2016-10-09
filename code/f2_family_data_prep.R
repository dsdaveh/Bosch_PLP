if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

source('bosch_plp_util.R')

##############  
input_csv <- '../input/train_numeric.csv'
col_names <- fread(input_csv, nrows=0L) %>% names()
f1_col <- col_names[ grepl("L0_S0", col_names)][1]
f2_col <- col_names[ grepl("L0_S12", col_names)][1]
f3_cola <- col_names[ grepl("L1_S24", col_names)][1]
f3_colb <- col_names[ grepl("L1_S25", col_names)][1]
fcols <- c( f1_col, f2_col, f3_cola, f3_colb)
#trn_fcols <- fread(input_csv, select=c("Id", "Response", fcols) )
#################
trnw.f2 <- data.table()
for (ichunk in 1:10) {
    trnw <- read_raw_chunk(ichunk, input= input_csv)
    trnw <- trnw[ ! is.na(L0_S12_F330) ]
    trnw.f2 <- rbind(trnw.f2, trnw)
    print(ichunk)
    rm(trnw)
}
sum(trnw.f2[, Response]) # 1323

#do this step in two phases (to reduce memory early on)
na_cols <- lapply(trnw.f2, function(x) all(is.na(x)))
na_col_names <- names(trnw.f2)[which( unlist(na_cols))]
trnw.f2[, (na_col_names) := NULL]

dim(trnw.f2) # 242061    892
object.size(trnw.f2) /1e9 # 1.71 GB
sum(trnw.f2[, Response]) # 1323

trnw.f2$F1 <- ! is.na( trnw.f2[ , f1_col, with=FALSE])
trnw.f2$F2 <- ! is.na( trnw.f2[ , f2_col, with=FALSE])
trnw.f2$F3 <- ! is.na( trnw.f2[ , f3_cola, with=FALSE]) | ! is.na( trnw.f2[ , f3_colb, with=FALSE])
trnw.f2[, F12 := F1 & F2]
trnw.f2[, F13 := F1 & F3]
trnw.f2[, F23 := F2 & F3]
trnw.f2[, pure := F1 + F2 + F3 == 1]
trnw.f2[, F4 := F1 + F2 + F3 == 0 ]

trnw.f2.info <- trnw.f2 %>% select( starts_with("F")) %>% 
    gather( family, val ) %>% filter( val == TRUE ) %>% data.frame()

trnw.f2.info %>%
    ggplot( aes( family)) + geom_bar() + ggtitle ('Families in training_ data')
trnw.f2.info %>% count( family, sort = TRUE )

trnw.f2 <- trnw.f2[ F2 == TRUE & pure == TRUE ]
sum(trnw.f2[, Response]) # 1312

#repeat this now the intersections are gone
na_cols <- lapply(trnw.f2, function(x) all(is.na(x)))
na_col_names <- names(trnw.f2)[which( unlist(na_cols))]
trnw.f2[, (na_col_names) := NULL]

dim(trnw.f2) #  240976    668
object.size(trnw.f2) /1e9 # 1.27 GB

# there are no measurement cols that have all constant values (no NA's and sd=0)
# no_sd <- lapply(trnw.f2, sd, na.rm = TRUE)
# no_na <- lapply(trnw.f2, function(x) ! any(is.na(x)))
# no_sd_names <- names(no_sd)[which( unlist(no_sd == 0))]
# no_na_names <- names(no_na)[which( unlist(no_na))]
# intersect(no_sd_names, no_na_names)

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
trnw.f2 <- purify_station_data_2(trnw.f2, 'L0_S14', 'L0_S15')# 120021 120798 78 79

#L0_S16 and L0_S17 are parallel
trnw.f2 <- purify_station_data_2(trnw.f2, 'L0_S16', 'L0_S17')# 118461 122276 76 6

#L0_S18 and L0_S19 are parallel
trnw.f2 <- purify_station_data_2(trnw.f2, 'L0_S18', 'L0_S19')# 120396 120340 1 0

#L0_S21 and L0_S22 and L0_S23 are parallel
trnw.f2 <- purify_station_data_3(trnw.f2, 'L0_S21', 'L0_S22', 'L0_S23')# 80893 80040 79740 98 0

# #L0_S35 and L0_S36 MIGHT BE IN parallel  (CATEGORICAL ?)

sum(trnw.f2[, Response]) # 1310
dim(trnw.f2) #  240638    668
object.size(trnw.f2) /1e9 # 1.26 GB

######################### now consolidate

combine_stations_2 <- function( dt, Sa, Sb) {
    Sac <- names(dt)[ grepl(Sa, names(dt)) ]
    Sbc <- names(dt)[ grepl(Sb, names(dt)) ]
    stopifnot( length(Sac) == length(Sbc))
    
    ix_Sa <- which( ! is.na( dt[, (Sac[1]), with=FALSE ]))
    ix_Sb <- which( ! is.na( dt[, (Sbc[1]), with=FALSE ]))
    
    comb_names <- paste( 'C', Sa, Sb, 1:length(Sac), sep = '_')
    
    # trnw.f2[ix_Sa, (comb_names[i]) := Sac[i], with=FALSE ] #this doesn't work like I expect
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
    
    # trnw.f2[ix_Sa, (comb_names[i]) := Sac[i], with=FALSE ] #this doesn't work like I expect
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
trnw.f2 <- combine_stations_2( trnw.f2, 'L0_S14', 'L0_S15')

#L0_S16 and L0_S17 are parallel
trnw.f2 <- combine_stations_2( trnw.f2, 'L0_S16', 'L0_S17')

#L0_S18 and L0_S19 are parallel
trnw.f2 <- combine_stations_2( trnw.f2, 'L0_S18', 'L0_S19')

#L0_S21 and L0_S22 and L0_23 are parallel
trnw.f2 <- combine_stations_3( trnw.f2, 'L0_S21', 'L0_S22', 'L0_S23')

sum(trnw.f2[, Response]) # 1310
dim(trnw.f2) #  240638    635
object.size(trnw.f2) /1e9 # 1.19 GB

nrow(trnw.f2) / sum(trnw.f2[, Response]) # pass:fail = 184:1


