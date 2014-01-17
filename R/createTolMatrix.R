createTolMatrix <-
function(data, tol) {
  holder <- matrix(1, nrow = dim(data)[2], ncol = dim(data)[2])
  holder[-1 ,1] <- tol
  return(holder)
}
