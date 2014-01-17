correlate <-
function(data, corm, tol=0.005, conv=10000, cores=2,
                      splitsize=1000, verbose=T) {
  if (is.vector(corm)) {
    corm <- createCorMatrix(data, corm)
  }
  if (is.vector(tol)) {
    tol <- createTolMatrix(data, tol)
  }
  
  # identifiers of the direction; e.g. from 0.3 to 0.5 
  bool1 <- cor(data) - corm < 0
  bool2 <- abs(corm) > cor(data)
  
  # Step 1: roughly
  data <- correlate.roughly(data, corm, tol, conv, cores, 25 * ncol(corm), verbose)
  
  # Step 2: until the end.
  while(!all( 
    ifelse(bool1, corm < cor(data), cor(data) < corm)[tol != 1],
    ifelse(bool1, ifelse(bool2, cor(data) < corm + tol, cor(data) < corm - tol),
           ifelse(bool2, corm - tol < cor(data),
                  corm - tol < cor(data)))[tol != 1])) {
    
    data <- data[sample(nrow(data), nrow(data)), ]
    
    ldata <- correlate.split(data, splitsize)
    
    splits <- length(ldata)
    
    if (cores > 1 && splits > 1) {
      if (Sys.info()["sysname"] == "Windows") {
        cl <- parallel::makeCluster(cores)
        temp <- parallel::parLapply(cl, ldata, function(x)
          correlate.permutate(x, corm, tol, bool1, bool2))
        parallel::stopCluster(cl = cl) 
      } else {
        temp <- parallel::mclapply(ldata, function(x)
          correlate.permutate(x, corm, tol, bool1, bool2),
                         mc.cores=cores)
      }
    } else {
      temp <- lapply(ldata, function(x)
        correlate.permutate(x, corm, tol, bool1, bool2))
    }
    data <- do.call(rbind, temp)
    splitsize <- splitsize * 4
  }
  if (verbose) {print(cor(data)) }      
  return(data)
}
