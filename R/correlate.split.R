correlate.split <-
function(data, splitsize=1000) {
  nr <- nrow(data)
  if (splitsize > nr) {
    splitsize <- nr
  }
  splits <- floor(nr / splitsize)
  list <- lapply(split(data[seq_len(splits*splitsize), ],
                       seq_len(splits)), function(x) matrix(x, splitsize))
  if (nr %% splitsize != 0) {
    list$last <- data[(splits * splitsize + 1):nr, ]
  }
  list
}
