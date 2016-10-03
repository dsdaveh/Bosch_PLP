trnw.f2 <- data.table()
for (ichunk in 1:10) {
    trnw <- read_raw_chunk(ichunk, input= input_csv)
    trnw <- trnw[ ! is.na(L0_S12_F330) ]
    trnw.f2 <- rbind(trnw.f2, trnw)
    print(ichunk)
}

na_cols <- lapply(trnw.f2, function(x) all(is.na(x)))
na_col_names <- names(trnw)[which( unlist(na_cols))]
trnw.f2[, (na_col_names) := NULL]

# no_sd <- lapply(trnw.f2, sd, na.rm = TRUE)
# na_col_names <- names(trnw)[which( unlist(na_cols))]
# trnw.f2[, (na_col_names) := NULL]

dim(trnw.f2) # 242061  892
object.size(trnw.f2) /1e9 # 1.71 GB

#L0_S14 and L0_S15 are parallel
sum( grepl("L0_S14", names(trnw.f2))) #9
sum( grepl("L0_S15", names(trnw.f2))) #9

S14c <- names(trnw.f2)[ grepl("L0_S14", names(trnw.f2)) ]
S15c <- names(trnw.f2)[ grepl("L0_S15", names(trnw.f2)) ]

ix_S14 <- which( ! is.na( trnw.f2[, (S14c[1]), with=FALSE ]))
ix_S15 <- which( ! is.na( trnw.f2[, (S15c[1]), with=FALSE ]))
both <- intersect( ix_S14, ix_S15)
ix_S14 <- setdiff( ix_S14, both)
ix_S15 <- setdiff( ix_S15, both)

trnw.f2$cw14 <- FALSE
trnw.f2$cw15 <- FALSE
trnw.f2[ c(ix_S14, both), cw14 := TRUE]
trnw.f2[ c(ix_S15, both), cw15 := TRUE]

sum(trnw.f2[both, Response]) # 0  
