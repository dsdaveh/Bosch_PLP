library(dplyr)
library(data.table)

EOL = "\n"

fea_ff <- function(dt, mag) {
    frate <- dt[, .(Id, Response, min_time, tval = round(min_time * 100 / 10**mag, 0))]
    frate[ , frate := sum(Response)/.N, tval]
    return( frate$frate )
}

add_cv_feature_lkp <- function(dt, dt_lkp) {
    mt_lkp <- dt_lkp[ , .(
        ff3 = max(ff3),
        ff4 = max(ff4)
        #ff5 = max(ff5)
        ),         min_time]
    setkey(dt, min_time)
    setkey(mt_lkp, min_time)
    dt <- mt_lkp[dt]
    
    #fix rows where m_time did not match
    ix_missing <- which( is.na(dt$ff3))
    niter <- 1
    while( length(ix_missing > 0)) {
        ix_replace <- ifelse(ix_missing == 1, 1 + niter, ix_missing + 1)
        id_orig <- dt[ix_missing, Id]
        dt[ix_missing] <- dt[ix_replace]
        dt[ix_missing]$Id <- id_orig
        ix_missing <- which( is.na(dt$ff3))
        niter <- niter + 1
    }
    return(dt)
}

add_cv_feature <- function( dt, exclude_fold=0) {
    #exclude_fold > 0 (1:nfolds) build features separately for train/validation
    
    #failure frequency features
    ix <- dt$kfold != exclude_fold
    fea <- dt[ix, .(Id, min_time)]
    #fea$ff0 <- fea_ff(dt[ix], 0)
    #fea$ff1 <- fea_ff(dt[ix], 1)
    #fea$ff2 <- fea_ff(dt[ix], 2)
    fea$ff3 <- fea_ff(dt[ix], 3)
    fea$ff4 <- fea_ff(dt[ix], 4)
    #fea$ff5 <- fea_ff(dt[ix], 5)
    
    if (exclude_fold > 0) {
        fea_val <-  add_cv_feature_lkp( dt[ kfold == exclude_fold, .(Id, min_time)], fea)
        fea <- rbind(fea, fea_val)[, min_time := NULL]
    }
    
    setkey(fea, Id)
    setkey(dt, Id)
    return( dt[fea, nomatch=FALSE])
}

add_magic <- function(dt) {
    magic <- readRDS(file = '../data/faron_magic4.rds')
    setkey(dt, Id)
    setkey(magic, Id)
    dt <- dt[ magic, nomatch=FALSE]
    return(dt)
}

add_time_station <- function(dt, ds='train') {
    dsname <- sprintf('../data/%s_date_station_features.rds', ds)
    stopifnot(file.exists(dsname))
    #from: source('data_prep_date_station.R')
    id_cnt <- readRDS(file = dsname)
    
    setkey(dt, Id)
    dt <- dt[ id_cnt, nomatch=FALSE]
    return(dt)
}

normalize <- function(x) (x - min(x)) / diff(range(x))

calc_mcc <- function(v) {
    # v is a vector or table produces with table(truth, predition)
    # this will yield values in the following order tn, fn, fp, tp
    v <- as.numeric(v) 
    if (length(v) != 4) return(0)
    tn <- v[1]; fn <- v[2]; fp <- v[3]; tp <- v[4]
    denom <- (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
    mcc <- ifelse( denom == 0, 0,
                   (tp*tn - fp*fn) / sqrt(denom))
    return(mcc)
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


find_cutoff_by_count <- function( p, np=1) {
    #p is a vector of probablilites
    #np is the number of positives to return
    cd <- data.frame(p) %>% count(p) %>% arrange(desc(p)) %>%
        mutate( cum = cumsum(n) , n_dist = abs(np - cumsum(n)))
    with(cd, p[which.min( n_dist) ])
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
get_tcheck <- function() tcheck.df %>% 
    mutate( delta=c( 0, diff(elapsed)) ) %>% 
    select( desc, delta) %>% 
    mutate( elapsed=cumsum(delta))

# read a chunk from the original input files
read_raw_chunk <- function( i, nchunk = 10, input = '../input/train_numeric.csv', cache = TRUE ) {
    #assumes categorical data has been converted to numeric with
    # sed -i.orig 's/T//g' XXX_categorical.csv    # XXX = [train|test]
    
    chunk_name <- gsub("\\.csv", sprintf("_raw_chunk%d.rds", i), input)
    chunk_name <- gsub("/input/", "/data/", chunk_name)
    
    #check to see if we already squirreled away this file
    if (nchunk == 10 & cache & file.exists(chunk_name)) {
        chunk <- readRDS(file=chunk_name)
        return(chunk)
    }

    nrows <- ifelse( grepl("train_",        input), 1183747, 1183748)
    chunk_size <- ceiling( nrows / 10)
    
    header <- fread(input = input, nrows=1, header=TRUE)
#     nfea <- ncol(header) - 1
#     ctype <- ifelse( grepl("categorical",   input), "character", "numeric")
#     ctypes <- c("integer", rep(ctype, nfea))
#     if (grepl("train_numeric", input)) {
#         nfea <- nfea - 1 # last column is response
#         ctypes <- c(ctypes, "integer")
#     } 
    
    skip_rows <- (i - 1) * chunk_size + 1
    chunk <- fread(input = input, skip=skip_rows, header=FALSE, nrows = chunk_size)
#                   colClasses = ctypes, na.strings = "", stringsAsFactors = TRUE )
    names(chunk) <- names(header)
    if (nchunk == 10 & cache) { 
        cat('...saving chunk as',chunk_name, '\n')
        saveRDS(chunk, file=chunk_name)
    }
    return(chunk)
}

#### performance ROCR ... double precision
.performance.phi <- function (predictions, labels, cutoffs, fp, tp, fn, tn, n.pos, 
                              n.neg, n.pos.pred, n.neg.pred) 
{
    list(cutoffs, (tn * tp - fn * fp)/sqrt(as.double(n.pos) * n.neg * n.pos.pred * 
                                               n.neg.pred))
}

performance_double <- function (prediction.obj, measure, x.measure = "cutoff", ...) 
{
    envir.list <- .define.environments()
    long.unit.names <- envir.list$long.unit.names
    function.names <- envir.list$function.names
    obligatory.x.axis <- envir.list$obligatory.x.axis
    optional.arguments <- envir.list$optional.arguments
    default.values <- envir.list$default.values
    if (class(prediction.obj) != "prediction" || !exists(measure, 
                                                         where = long.unit.names, inherits = FALSE) || !exists(x.measure, 
                                                                                                               where = long.unit.names, inherits = FALSE)) {
        stop(paste("Wrong argument types: First argument must be of type", 
                   "'prediction'; second and optional third argument must", 
                   "be available performance measures!"))
    }
    if (exists(x.measure, where = obligatory.x.axis, inherits = FALSE)) {
        message <- paste("The performance measure", x.measure, 
                         "can only be used as 'measure', because it has", 
                         "the following obligatory 'x.measure':\n", get(x.measure, 
                                                                        envir = obligatory.x.axis))
        stop(message)
    }
    if (exists(measure, where = obligatory.x.axis, inherits = FALSE)) {
        x.measure <- get(measure, envir = obligatory.x.axis)
    }
    if (x.measure == "cutoff" || exists(measure, where = obligatory.x.axis, 
                                        inherits = FALSE)) {
        
        optional.args <- list(...)
        argnames <- c()
        if (exists(measure, where = optional.arguments, inherits = FALSE)) {
            
            argnames <- get(measure, envir = optional.arguments)
            default.arglist <- list()
            for (i in 1:length(argnames)) {
                default.arglist <- c(default.arglist, get(paste(measure, 
                                                                ":", argnames[i], sep = ""), envir = default.values, 
                                                          inherits = FALSE))
            }
            names(default.arglist) <- argnames
            for (i in 1:length(argnames)) {
                templist <- list(optional.args, default.arglist[[i]])
                names(templist) <- c("arglist", argnames[i])
                optional.args <- do.call(".farg", templist)
            }
        }
        optional.args <- .select.args(optional.args, argnames)
        function.name <- get(measure, envir = function.names)
        print(c('dah-modified', function.name))
        x.values <- list()
        y.values <- list()
        
        for (i in 1:length(prediction.obj@predictions)) {
            argumentlist <- .sarg(optional.args, predictions = prediction.obj@predictions[[i]], 
                                  labels = prediction.obj@labels[[i]], cutoffs = prediction.obj@cutoffs[[i]], 
                                  fp = prediction.obj@fp[[i]], tp = prediction.obj@tp[[i]], 
                                  fn = prediction.obj@fn[[i]], tn = prediction.obj@tn[[i]], 
                                  n.pos = prediction.obj@n.pos[[i]], n.neg = prediction.obj@n.neg[[i]], 
                                  n.pos.pred = prediction.obj@n.pos.pred[[i]], 
                                  n.neg.pred = prediction.obj@n.neg.pred[[i]])
            ans <- do.call(function.name, argumentlist)
            if (!is.null(ans[[1]])) 
                x.values <- c(x.values, list(ans[[1]]))
            y.values <- c(y.values, list(ans[[2]]))
        }

        if (!(length(x.values) == 0 || length(x.values) == length(y.values))) {
            stop("Consistency error.")
        }
        return(new("performance", x.name = get(x.measure, envir = long.unit.names), 
                   y.name = get(measure, envir = long.unit.names), alpha.name = "none", 
                   x.values = x.values, y.values = y.values, alpha.values = list()))
    }
    else {
        perf.obj.1 <- performance(prediction.obj, measure = x.measure, 
                                  ...)
        perf.obj.2 <- performance(prediction.obj, measure = measure, 
                                  ...)
        return(.combine.performance.objects(perf.obj.1, perf.obj.2))
    }
}
