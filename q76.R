library(timeSeries)
library(quadprog)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())
weekly <- weekly[,c(1:3, 5:15)]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
pointcut <- as.Date(max(time(weekly))) - 731
history.returns <- window(weekly.returns, 
                          start=start(weekly.returns), 
                          end=pointcut)
investment.returns <- window(weekly.returns, 
                             start=pointcut+1, 
                             end=end(weekly.returns))

q76 <- function(r) {
  means <- colMeans(r) * 52
  sigma <- cov(r) * 52
  lambdas <- seq(-.3, .3, by=.02)
  
  w <- lapply(lambdas, function(lambda) {
    solve.QP(sigma, means * lambda,
             cbind(rep(1, ncol(sigma)),
                   diag(ncol(sigma))),
             c(1, rep(0,ncol(sigma))), 1)$solution
  })
  sapply(w, mv.coords.gen(sigma, means))
}

mv.points1 <- q76(history.returns)
mv.points2 <- q76(investment.returns)

p <- par(cex.axis=.8, cex.lab=.8, font.lab=2, las=1)
cols <- c("magenta", "orange")
xrng <- range(mv.points1[1,], mv.points2[1,])
yrng <- range(mv.points1[2,], mv.points2[2,])
plot(mv.points1[1,], mv.points1[2,], 
     type="o", pch=18, axes=FALSE, col=cols[1],
     main="Minimum variance frontier",
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
legend("right", c("Historic period", "Investment period"), 
       pch=c(18,17), col=cols, cex=.8)
par(p)
