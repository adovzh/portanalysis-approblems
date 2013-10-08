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

solve.q66 <- function(p.returns) {
  means <- colMeans(p.returns) * 52
  sigma <- cov(p.returns) * 52

  s <- solve.QP(sigma,
                matrix(rep(0, ncol(sigma))),
                cbind(rep(1, ncol(sigma)),
                      means,
                      diag(ncol(sigma))),
                c(1, .15, rep(0, ncol(sigma))), 2)$solution
  risk <- t(s) %*% sigma %*% s
  list(weights=s, risk=risk)
}

# 14-stock portfolio
s1 <- solve.q66(stock.returns)
# stocks + bonds
s2 <- solve.q66(cbind(stock.returns, bond.returns))
# stocks + bonds + bills
s3 <- solve.q66(cbind(stock.returns, bond.returns, bill.returns))

