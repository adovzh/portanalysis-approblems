library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
weekly <- cbind(weekly, SPAUD=weekly[,"S.PCOMP"] / weekly[,"USAUSTR"])
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52

stocks <- c(names(weekly)[c(1:3, 5:11)], "SPAUD")
sigma <- cov(weekly.returns[,stocks]) * 52

library(quadprog)

s <- solve.QP(sigma,
              matrix(rep(0, ncol(sigma))),
              cbind(rep(1, ncol(sigma)),
                    means[stocks],
                    c(rep(0, ncol(sigma)-1), 1)),
              c(1, .1, .2), 3)

print(asReport(s$solution, "Weight", colnames(sigma)))
cat(sprintf("\nExpected return: %g%%\n", 
            100 * crossprod(s$solution, means[stocks])))
