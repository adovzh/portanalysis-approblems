library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52

stocks <- names(weekly)[c(1:3, 5:15)]
sigma <- cov(weekly.returns[,stocks]) * 52

library(quadprog)

# expected return 10%, backing stocks 25%
s <- solve.QP(sigma,
              matrix(rep(0, ncol(sigma))),
              cbind(rep(1, ncol(sigma)),
                    means[stocks],
                    stocks %in% BANKING_STOCKS),
              c(1, .1, .25), 3)

print(asReport(s$solution, "Weight", colnames(sigma)))
cat(sprintf("\nExpected return: %g%%\n", 
            100 * crossprod(s$solution, means[stocks])))
cat(sprintf("The sum of weights: %g%%\n", 100 * sum(s$solution)))
cat(sprintf("The sum of weights in banking sector: %g%%\n\n",
          100 * sum(s$solution[stocks %in% BANKING_STOCKS])))

# expected return 15%
s2 <- solve.QP(sigma,
              matrix(rep(0, ncol(sigma))),
              cbind(rep(1, ncol(sigma)),
                    means[stocks]),
              c(1, .15), 2)

print(asReport(s2$solution, "Weight", colnames(sigma)))
cat(sprintf("\nExpected return: %g%%\n", 
            100 * crossprod(s2$solution, means[stocks])))
cat(sprintf("The sum of weights: %g%%\n", 100 * sum(s2$solution)))
cat(sprintf("The sum of weights in banking sector: %g%%\n",
            100 * sum(s2$solution[stocks %in% BANKING_STOCKS])))
