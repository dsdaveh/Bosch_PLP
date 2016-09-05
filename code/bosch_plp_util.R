library(dplyr)

EOL = "\n"

normalize <- function(x) (x - min(x)) / diff(range(x))

calc_mcc <- function(v) {
    # v is a vector or table produces with table(truth, predition)
    # this will yield values in the following order tn, fn, fp, tp
    v <- as.numeric(v) 
    tn <- v[1]; fn <- v[2]; fp <- v[3]; tp <- v[4]
    (tp*tn - fp*fn) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
}

find_cutoff_by_ratio <- function( p, r=1) {
    #p is a vector of probablilites
    #r is a ratio the desired ratio of positive : negative
    np <- length(p)
    cd <- data.frame(p) %>% count(p) %>% arrange(p) %>%
        mutate( cumsum(n),
                rat = (np - cumsum(n)) / cumsum(n),
                cum_dist = r - (np - cumsum(n)) / cumsum(n) )
    with(cd, p[which.min( abs(cum_dist)) ])
}


if (! exists("tcheck.print")) tcheck.print = FALSE
if (! exists("tcheck.df")) tcheck.df <- data.frame( stringsAsFactors = FALSE)
tcheck.default_string <- function() sprintf( "t=%d", nrow(tcheck.df))
tcheck.tx <- list( proc.time()) 
tcheck <- function(t=1, desc = tcheck.default_string() ) {
    # t=0 to reset counter, t=1 incremental time output,  t=n time difference from n intervals
    #
    # use:
    # tcheck(0) #reset the counter
    # <computation 1>
    # tcheck()
    # <computation 2>
    # tcheck()
    # tcheck(2)  # for total time
    #
    t <- min( t, length(tcheck.tx))
    pt <- proc.time()
    if (t == 0) { 
        tcheck.tx <<- list( proc.time()) 
        tcheck.df <<- data.frame( elapsed = pt[3], desc = desc,stringsAsFactors = FALSE )
    } else {
        tcheck.tx <<- c( tcheck.tx, list(pt))
        tcheck.df <<- rbind( tcheck.df, data.frame( elapsed = pt[3], desc = desc, stringsAsFactors = FALSE ) )
        tn <- nrow( tcheck.df )
        elapsed_delta <- diff( tcheck.df[ c(tn-t, tn),]$elapsed )
        out_str <- ifelse ( t == 1
                            , sprintf("%f elapsed for %s", elapsed_delta
                                      , tcheck.df[tn, "desc"] )
                            , sprintf("%f elapsed from %s:%s", elapsed_delta
                                      , tcheck.df[tn, "desc"], tcheck.df[tn-t, "desc"]) )
        if (tcheck.print) print( out_str)
        return( out_str )
        #         tn <- length(tcheck.tx)
        #         print ( tcheck.tx[[tn]] - tcheck.tx[[tn-t]]) 
    }
}
get_tcheck <- function() tcheck.df %>% mutate( delta=c( 0, diff(elapsed)) ) %>% select( desc, delta)

# read a chunk from the original input files
read_raw_chunk <- function( i, nchunk = 10, input = '../input/train_numeric.csv', cache = TRUE ) {
    
    chunk_name <- gsub("\\.csv", sprintf("_chunk%d.rds", i), input)
    chunk_name <- gsub("/input/", "/data/", chunk_name)
    
    #check to see if we already squirreled away this file
    if (nchunk == 10 & cache & file.exists(chunk_name)) {
        chunk <- readRDS(file=chunk_name)
        return(chunk)
    }
    ncols <- ifelse( grepl("train_numeric", input), 970, 969)
    nrows <- 1183747
    chunk_size <- ceiling( nrows / 10)
    
    header <- fread(input = input, nrows=1, header=TRUE)
    
    skip_rows <- (i - 1) * chunk_size + 1
    chunk <- fread(input = "../input/train_numeric.csv", skip=skip_rows, header=FALSE,
                   nrows = chunk_size )
    names(chunk) <- names(header)
    if (nchunk == 10 & cache) { 
        warning('...saving chunk as',chunk_name, '\n')
        loadRDS(chunk, file=chunk_name)
    }
    return(chunk)
}
