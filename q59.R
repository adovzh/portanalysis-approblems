library(timeSeries)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52

# stocks <- names(weekly)[c(1:3, 5:11)]
stocks <- names(weekly)[c(1:3, 5:15)]
sigma <- cov(weekly.returns[,stocks]) * 52

library(quadprog)

# expected return 10%, no short sales
s <- solve.QP(sigma,
              matrix(rep(0, ncol(sigma))),
              cbind(rep(1, ncol(sigma)),
                    means[stocks],
                    diag(ncol(sigma))),
              c(1, .0472, rep(0, ncol(sigma))), 2)
