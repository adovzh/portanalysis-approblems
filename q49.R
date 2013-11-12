library(timeSeries)
# library(quadprog)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly <- weekly[, colnames(weekly) != "WSF"]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52
target.return <- .1

# find efficient minimum variance portfolio
w <- findMVP(cov(weekly.returns), means, target.return)

# sanity check
stopifnot(abs(sum(w) - 1) < 1e-9)
stopifnot(abs(sum(means * w) - target.return) < 1e-9)

print(as.matrix(w))

# s <- solve.QP(cov(weekly.returns),
#               matrix(rep(0, ncol(weekly))),
#               matrix(c(rep(1, ncol(weekly)), means), ncol=2),
#               c(1,.1), 2)
