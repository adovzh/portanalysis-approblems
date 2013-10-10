library(timeSeries)
library(quadprog)
source("common.R")
source("bonds.R")

weekly <- toTimeSeries(loadWeekly())
# exclude GFC data
weekly <- window(weekly, start="2009-01-01",
                 end=Sys.timeDate())
weekly.stocks <- weekly[,c(1:3, 5:15)]
stock.returns <- na.omit(fapply(weekly.stocks, FUN=returns))

weekly.bills <- weekly[,"ADBR090"]
bill.returns <- bill.hpr.weekly(weekly.bills[-length(weekly.bills)],
                                weekly.bills[-1])
bill.returns <- timeSeries(bill.returns, time(stock.returns))

weekly.bonds <- weekly[,"ABND10Y"]
bond.returns <- bond.hpr.weekly(weekly.bonds[-length(weekly.bonds)],
                                weekly.bonds[-1])
bond.returns <- timeSeries(bond.returns, time(stock.returns))
weekly.returns <- cbind(stock.returns, bond.returns, bill.returns)

means <- colMeans(weekly.returns) * 52
sigma <- cov(weekly.returns) * 52


w1 <- solve.QP(sigma,
               matrix(rep(0, ncol(sigma))),
               cbind(rep(1, ncol(sigma)),
                     means,
                     diag(ncol(sigma))),
               c(1, .16, rep(0, ncol(sigma))), 2)$solution

w2 <- solve.QP(sigma,
               matrix(rep(0, ncol(sigma))),
               cbind(rep(1, ncol(sigma)),
                     means,
                     c(rep(1, ncol(sigma) - 2), rep(0, 2)),
                     diag(ncol(sigma))),
               c(1, .15, .8, rep(0, ncol(sigma))), 3)$solution

w3 <- solve.QP(sigma,
               matrix(rep(0, ncol(sigma))),
               cbind(rep(1, ncol(sigma)),
                     means,
                     c(rep(1, ncol(sigma) - 2), rep(0, 2)),
                     diag(ncol(sigma))),
               c(1, .15, .8, rep(0, ncol(sigma))), 2)$solution

report <- rbind(w1, w2, w3)
dimnames(report) <- list(c("Unrestricted", "Equality", "Inequality"), 
                         c(names(weekly.stocks), "10-Yr", "90-Day"))

cat("Weights:\n")
print(apply(report, 1, refine.weights))
cat("\nStatistics:\n")
print(apply(report, 1, function(w) {
  v <- t(w) %*% sigma %*% w
  c(Sum=sum(w), Return=crossprod(w, means), Variance=v, StDeviation=sqrt(v))
}))

cols <- c("#9A3866", "#9BA1FC", "darkblue")
yrng <- pretty(c(0, w1, w2, w3))
p <- par(cex.axis=.8, las=2)
barplot(report, beside=T, col=cols, yaxt="n", ylim=range(yrng))
axis(2, at=yrng, 
     labels=sprintf("%g%%", yrng*100))
legend("top", dimnames(report)[[1]], col=cols, pch=15, cex=.8)
par(p)
