correlate.roughly <-
function(data, corm, tol, conv, cores, splitsize, verbose) { 
  if (splitsize <= nrow(data)) {
    if (cores > 1 & require(parallel)) {
      if (Sys.info()["sysname"] == "Windows") {
        cl <- parallel::makeCluster(cores)
        ldata <- correlate.split(data, splitsize=splitsize)
        temp <- parallel::parLapply(cl, ldata, function(x)
          correlate.roughly.permutate(x, corm, tol, conv, verbose))
        parallel::stopCluster(cl = cl)
      } else {
        temp <- parallel::mclapply(correlate.split(data, splitsize=splitsize), function(x)
          correlate.roughly.permutate(x, corm, tol, conv, verbose),
                         mc.cores=cores)
      }
    } else {
      temp <- lapply(correlate.split(data, splitsize=splitsize), function(x)
        correlate.roughly.permutate(x, corm, tol, conv, verbose))
    }
    return(do.call(rbind, temp))
  }
  correlate.roughly.permutate(data, corm, tol, conv, verbose)
}
