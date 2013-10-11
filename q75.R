library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
weekly <- weekly[,c(1:3, 5:15)]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
pointcut <- as.Date(max(time(weekly))) - 731
history.returns <- window(weekly.returns, 
                          start=start(weekly.returns), 
                          end=pointcut)
investment.returns <- window(weekly.returns, 
                             start=pointcut+1, 
                             end=end(weekly.returns))


Box.M.test <- function(...) {
  populations <- list(...)
  stopifnot(is.list(populations))
  # test all arguments are timeSeries
  stopifnot(all(sapply(populations, is.timeSeries)))
  # test there's more than one argument
  stopifnot(length(populations) > 1)
  pnames <- lapply(populations, colnames)
  # test the names are all equal
  stopifnot(sapply(seq_along(pnames)[-1], function(j) identical(pnames[[1]], pnames[[j]])))
  
  # k - number of variables
  knames <- pnames[[1]]
  k <- length(knames)
  
  # m - number of matrices
  m <- length(populations)
  
  # nj - vector of the numbers of observations
  nj <- sapply(populations, nrow)
  
  # n - overall number of observations
  n <- sum(nj)
  
  # Sj - cube of covariance matrices
  Sj <- sapply(populations, function(...) c(cov(...)))
  dim(Sj) <- c(k, k, m)
  dimnames(Sj) <- list(knames, knames, seq(m))
  
  # pooled covariance matrix
  S <- Reduce('+', lapply(seq(m), function(j) {
    nj[j] * Sj[,,j]
  })) / (n - m)
  
  # M-statistic
  M <- c((n - m) * determinant(S)$modulus - 
    crossprod(nj, apply(Sj, 3, function(cm) determinant(cm)$modulus)))
  c <- (sum((nj-1)^(-1)) - (n - m)^(-1)) * (2*k^2+3*k-1) / (6*(k+1)*(m-1))
  df <- k * (k + 1) * (m - 1) / 2
  pval <- 1 - pchisq(as.numeric(M*(1-c)), df)
  
  c2 <- (sum((nj-1)^(-2)) - (n-m)^(-2)) * (k - 1) * (k + 2) / (6 * (m - 1))
  df2 <- (df + 2) / abs(c2 - c * c)
  
  if (c2 > c * c) {
    F <- M / (df / (1 - c - df/df2))
  } else {
    F <- df2 * M / (df * ((df2 / (1 - c - 2/df2)) - M))
  }

  f.pval <- 1 - pf(as.numeric(F), df, df2)  
  list(M=M, c=c, df=df, chisq.pval=pval, c2=c2, df2=df2, f.pval=f.pval)
}

bmt <- Box.M.test(history.returns, investment.returns)
cat(sprintf("Box Chi test p-value: %g\n", bmt$chisq.pval))
cat(sprintf("F-test p-value: %g\n", bmt$f.pval))
