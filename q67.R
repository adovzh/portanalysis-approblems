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

solve.q67 <- function(p.returns) {
  means <- colMeans(p.returns) * 52
  sigma <- cov(p.returns) * 52
  
  s <- solve.QP(sigma,
                matrix(rep(0, ncol(sigma))),
                cbind(rep(1, ncol(sigma)),
                      diag(ncol(sigma))),
                c(1, rep(0, ncol(sigma))), 1)$solution
  risk <- t(s) %*% sigma %*% s
  list(weights=s, risk=risk)
}

# 14-stock portfolio
s1 <- solve.q67(stock.returns)
# stocks + bonds
s2 <- solve.q67(cbind(stock.returns, bond.returns))
# stocks + bonds + bills
s3 <- solve.q67(cbind(stock.returns, bond.returns, bill.returns))

weights <- cbind(c(refine.weights(s1$weights), c(NA,NA)),
                 c(refine.weights(s2$weights), NA),
                 refine.weights(s3$weights))
dimnames(weights) <- list(c(colnames(stock.returns), "Bond", "Bill"),
                          c("P1", "P2", "P3"))
print(weights, digits=3)

portfolio.risks <- c(P1=s1$risk, P2=s2$risk, P3=s3$risk)
cat("\nPorfolio standard deviation(%):\n")
print(portfolio.risks*100)

p <- par(cex.axis=.8)
yrng <- pretty(c(0, portfolio.risks))
barplot(portfolio.risks, col="#9BA1FC", yaxt="n", 
        ylim=range(yrng), main="Portfolio Standard Deviation")
axis(2, at=yrng, 
     labels=sprintf("%g%%", yrng*100), las=1)
par(p)
