correlate.permutate <-
function(data, corm, tol, bool1, bool2) {
  on.exit(return(data))
  for (row in 1:c(nrow(corm)-1)) {
    row <- row + 1  
    cells <- which(tol[row, 1:row] != 1)
    no.change <- 0
    while (!all(
      ifelse(bool1, corm < cor(data), cor(data) < corm)[row, cells],
      ifelse(bool1, ifelse(bool2, cor(data) < corm + tol,
                           cor(data) < corm - tol),
             ifelse(bool2, corm - tol < cor(data),
                    corm - tol < cor(data)))[row, cells])) {
      
      index <- sample(nrow(data), 1)
      
      random.index <- unique(c(index, sample(nrow(data), 10)))[1:10]
      cor.proposals <- matrix(1, nrow=length(random.index),
                              ncol = length(cells))
      for (j in 1:length(random.index)) {
        switcher <- data[ , 1:row]
        switcher[c(index, random.index[j]), row] <- switcher[c(random.index[j],
                                                               index), row]
        cor.proposals[j, ] <- cor(switcher[ , 1:row])[row, cells]
      }
      tokeep <- which(
        colSums(abs(t(cor.proposals) - corm[row, cells]) - tol[row,cells])
        == min(colSums(abs(t(cor.proposals) - corm[row, cells]) - tol[row, cells])))[1]
      # Do the switch for real.
      oldcor <- cor(data)
      data[c(index, random.index[tokeep]), row] <- data[c(random.index[tokeep], index), row]
      
      newcor <- cor(data)
      ifelse(all(oldcor == newcor), no.change <- no.change + 1, no.change <- 0)
      if (no.change == 3) { break }
    }
  }
}
