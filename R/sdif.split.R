sdif.split <- function(x, label="") {
  if (!is.factor(x)) {
    stop("Variable passed to sdif.split() is not a factor.")
  }
  levs <- levels(x)
  n <- length(levs)
  cmat <- contr.sdif(n)
  cols <- lapply(seq_len(n-1), function(i) {
    name <- paste(label, levs[i+1], ".", levs[i], sep="")
    d <- data.frame(cmat[as.numeric(x), i])
    names(d) <- name
    return(d)
  })
  do.call(cbind, cols)
}
