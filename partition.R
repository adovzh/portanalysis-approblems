partition <- function(v, comp) {
  rfunc <- function(acc, elem) {
    if (comp(acc$last, elem)) {
      if (length(acc$part) > 0) {
        acc$part[1] <- acc$part[1] + 1
      } else {
        acc$part <- 1
      }
    } else {
      acc$part <- c(1, acc$part)
    }
    
    acc$last <- elem
    return(acc)
  }
  
  r <- Reduce(rfunc, v, list(part=NULL, last=NULL))
  rev(r$part)
}

comp.returns <- function(prev, curr) {
  is.null(prev) || xor(prev > 0, curr <= 0)
}

partition.returns <- function(rtn) partition(rtn, comp.returns)

