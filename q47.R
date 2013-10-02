library(timeSeries)
library(quadprog)
source("common.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))

# stocks <- sample(1:5, 3)
stocks <- c(5,2,3)
data <- as.timeSeries(weekly[,stocks])

absMinRiskWeights <- function(tS) {
  om <- cov(tS)
  ominv <- solve(om)
  l <- rep(1, ncol(tS))
  return(ominv %*% l %*% (t(l) %*% ominv %*% l))
}

print(absMinRiskWeights(data))


s <- solve.QP(cov(data), 
              matrix(rep(0, ncol(data))), 
              matrix(rep(1, ncol(data))), 
              1, 1)
