library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
# exclude GFC data
weekly.returns <- window(weekly.returns, start="2009-01-01",
                         end=Sys.timeDate())
means <- colMeans(weekly.returns) * 52

stocks <- names(weekly)[c(1:3, 5:15)]
sigma <- cov(weekly.returns[,stocks]) * 52

library(quadprog)

# expected return 10%, no short sales
s <- solve.QP(sigma,
              matrix(rep(0, ncol(sigma))),
              cbind(rep(1, ncol(sigma)),
                    means[stocks],
                    diag(ncol(sigma))),
              c(1, .1, rep(0, ncol(sigma))), 2)

sol <- asReport(refine.weights(s$solution), "[No SS]",
                colnames(sigma))

# expected return 10%, short sales allowed
s <- solve.QP(sigma,
              matrix(rep(0, ncol(sigma))),
              cbind(rep(1, ncol(sigma)),
                    means[stocks]),
              c(1, .1), 2)

sol2 <- asReport(refine.weights(s$solution), "[SS allowed]",
                 colnames(sigma))

report <- cbind(sol, sol2)

print(report)
cat("\nAssets included:\n")
print(colStats(report, function(x) length(x[abs(x)>EPS])))
