library(timeSeries)
library(quadprog)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))

# stocks <- sample(1:15, 3)
stocks <- c(1,2,3)
data <- as.timeSeries(weekly.returns[,stocks])

absMinRiskWeights <- function(tS) {
  om <- cov(tS)
  ominv <- solve(om)
  l <- rep(1, ncol(tS))
  return(ominv %*% l %*% solve(t(l) %*% ominv %*% l))
}

cat("Weights using formula:\n")
print(w1 <- absMinRiskWeights(data))


s <- solve.QP(cov(data), 
              matrix(rep(0, ncol(data))), 
              matrix(rep(1, ncol(data))), 
              1, 1)
cat("\nWeights solving quadratic programming problem:\n")
print(w2 <- matrix(s$solution, dimnames=list(colnames(data))))

cat("\nWeights solving system of linear equations:\n")
print(w3 <- matrix(findGMVP(cov(data)), dimnames=list(colnames(data))))

cat(sprintf("\nAll equal: %s\n", all.equal(w1, w2) && all.equal(w1, w3)))
