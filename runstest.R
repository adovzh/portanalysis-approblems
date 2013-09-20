source("partition.R")

runs.test <- function(v) {
  psp <- partition.returns(v)
  spsp <- psp * rep(c(1,-1), length.out=length(psp))
  upmonth.prob <- 100 * sum(spsp[spsp > 0]) / sum(psp)
  downmonth.prob <- 100 * sum(-spsp[spsp < 0]) / sum(psp)
  longest.run <- max(psp)

  ar <- length(psp)
  np <- sum(spsp[spsp > 0])
  nn <- sum(-spsp[spsp < 0])
  nt <- sum(psp)
  
  er <- 2 * np * nn / nt + 1
  sigmar <- sqrt(2 * np * nn * (2 * np * nn - nt) / (nt^2 * (nt - 1)))
  Z <- (ar - er) / sigmar
  pval <- 2 * (1 - pnorm(abs(Z)))
  
  o <- list(runs=psp, upmonth.prob=upmonth.prob, downmonth.prob=downmonth.prob,
            longest.run=longest.run, Z=Z, pval=pval)
  return(o)
}