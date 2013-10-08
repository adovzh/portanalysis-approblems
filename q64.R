library(timeSeries)
source("common.R")
source("bonds.R")

weekly <- toTimeSeries(loadWeekly())
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

weekly.returns <- cbind(stock.returns, 
                        B90D=bill.returns, 
                        B10Y=bond.returns)

p <- par(cex.axis=.7)
means <- colMeans(weekly.returns) * 52
risks <- colStdevs(weekly.returns) * sqrt(52)
xrng <- c(0, risks)
plot(risks, means, pch=16, xlim=range(xrng), ylim=range(means)*1.1,
     col=rainbow(15), axes=FALSE)
axis(1, at=pretty(xrng), 
     labels=sprintf("%.1f%%", pretty(xrng) * 100))
axis(2, at=pretty(means),
     labels=sprintf("%.2f%%", pretty(means) * 100))
text(risks, means, colnames(weekly.returns), 
     cex=.6, adj=c(NA,-0.8), offset=10)
box()
grid()
par(p)
