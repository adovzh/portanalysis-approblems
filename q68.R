library(timeSeries)
library(quadprog)
source("common.R")
source("bonds.R")
source("efficientp.R")

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
lambdas <- seq(-.3, .3, by=.02)
mv.coords <- mv.coords.gen(sigma, means)

# no short sales unconstrained portfolio
p1.w <- lapply(lambdas, function(lambda) {
  solve.QP(sigma,
           means * lambda,
           cbind(rep(1, ncol(sigma)),
                 diag(ncol(sigma))),
           c(1, rep(0,ncol(sigma))), 1)$solution      
})

mv.points1 <- sapply(p1.w, mv.coords)

# no short sales, equity = 80%
p2.w <- lapply(lambdas, function(lambda) {
  solve.QP(sigma,
           means * lambda,
           cbind(rep(1, ncol(sigma)),
                 c(rep(1, ncol(sigma) - 2), rep(0, 2)),
                 diag(ncol(sigma))),
           c(1, .8, rep(0,ncol(sigma))), 2)$solution      
})

mv.points2 <- sapply(p2.w, mv.coords)

p <- par(cex.axis=.8, las=1)
cols <- c("magenta", "orange")
xrng <- range(mv.points1[1,], mv.points2[1,])
yrng <- range(mv.points1[2,], mv.points2[2,])
plot(mv.points1[1,], mv.points1[2,], 
     type="o", pch=18, axes=FALSE, col=cols[1],
     xlab="St deviation", ylab="Expected return",
     xlim=xrng,
     ylim=yrng)
points(mv.points2[1,], mv.points2[2,], pch=17, col=cols[2], type="o")
axis(1, at=pretty(xrng), 
     labels=sprintf("%g%%", pretty(xrng)*100))
axis(2, at=pretty(yrng), 
     labels=sprintf("%g%%", pretty(yrng)*100))
box()
grid()
legend("right", c("Unconstrainted 16 assets", "Equality constrained (80%)"), 
       pch=c(18,17), col=cols, cex=.8)
par(p)
