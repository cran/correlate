correlate.roughly.permutate <-
function(data, corm, tol, conv, verbose) {
  on.exit(return(data)) 
  for (row in 1:c(nrow(corm)-1)) {
    I <- 0  # track iterations 
    row <- row + 1
    cells <- which(tol[row, 1:row] != 1)  
    while (sum(abs(cor(data) - corm)[row, cells] - tol[row, cells] < 0) < length(cells))  {
      I <- I + 1 
      
      index <- sample(nrow(data), 1)
      random.index <- unique(c(index, sample(nrow(data), 10)))[1:10]
      cor.proposals <- matrix(1, nrow=length(random.index), ncol = length(cells))
      
      for (j in 1:length(random.index)) {
        switcher <- data[ , 1:row]
        switcher[c(index, random.index[j]), row] <- switcher[c(random.index[j], index), row]
        
        cor.proposals[j, ] <- cor(switcher[ , 1:row])[row, cells]
      }
      tokeep <- which(
        colSums(abs(t(cor.proposals) - corm[row, cells]) - tol[row,cells])
        == min(colSums(abs(t(cor.proposals) - corm[row,cells ]) - tol[row, cells])))[1]
      
      ## Do the switch for real.
      data[c(index, random.index[tokeep]), row] <- data[c(random.index[tokeep], index), row]
      
      if (I == conv) {
        if (verbose == TRUE) { warning(paste0("no convergence in row ", row)) }
        return(data)
      }
    } 
  }
}
