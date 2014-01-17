createCorMatrix <-
function(data, corm) {
  CorMatrix <- matrix(0, nrow = dim(data)[2], ncol = dim(data)[2])
  diag(CorMatrix) <- 1
  CorMatrix[1,-1] <- corm
  CorMatrix[-1,1] <- corm
  return(CorMatrix)
}
