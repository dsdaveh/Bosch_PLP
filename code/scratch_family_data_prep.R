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

no_sd <- lapply(trnw.f2, sd, na.rm = TRUE)
na_col_names <- names(trnw)[which( unlist(na_cols))]
trnw.f2[, (na_col_names) := NULL]