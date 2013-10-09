library(timeSeries)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
# exclude GFC data
weekly.returns <- window(weekly.returns, start="2009-01-01",
                         end=Sys.timeDate())
means <- colMeans(weekly.returns) * 52
alphas <- seq(-.3, .5, length.out=50)
lambdas <- seq(0, .3, length.out=50)

stocks <- names(weekly)[c(1:3, 5:15)]
sigma <- cov(weekly.returns[,stocks]) * 52

mv.coords <- mv.coords.gen(sigma, means[stocks])

library(quadprog)

# short saled allowed (SSA)
p1.w1 <- findMVP(sigma, means[stocks], min(means[stocks]))
p1.w2 <- findMVP(sigma, means[stocks], max(means[stocks]))
z1 <- z.gen(p1.w1, p1.w2)
mv.points1 <- sapply(alphas, function(alpha) mv.coords(z1(alpha)))

# short sales not allowed (SSN)
p2.w <- lapply(lambdas, function(lambda) {
  solve.QP(sigma,
           means[stocks] * lambda,
           cbind(rep(1, ncol(sigma)),
                 diag(ncol(sigma))),
           c(1, rep(0,ncol(sigma))), 1)$solution        
})
mv.points2 <- sapply(p2.w, mv.coords)

p <- par(mfcol=c(1,1), cex.axis=.8, las=1)
xrng <- range(mv.points1[1,], mv.points2[1,])
yrng <- range(mv.points1[2,], mv.points2[2,])
plot(mv.points1[1,], mv.points1[2,], 
     type="o", pch=15, axes=FALSE, col="orange",
     xlab="St deviation", ylab="Expected return",
     xlim=xrng,
     ylim=yrng)
points(mv.points2[1,], mv.points2[2,], pch=18, col="green", type="o")
axis(1, at=pretty(xrng), 
     labels=sprintf("%g%%", pretty(xrng)*100))
axis(2, at=pretty(yrng), 
     labels=sprintf("%g%%", pretty(yrng)*100))
legend("topleft", c("SSA", "SSN"), 
       pch=c(15,18), col=c("orange", "green"), cex=.8)
box()
grid()

# increased risk
# dff <- mv.points2[1,] - mv.points1[1,]
# plot(mv.points1[2,], dff,
#      type="o", pch=18, axes=FALSE, col="orange",
#      xlab="Target return", ylab="Increased risk",
#      xlim=range(mv.points1[2,]), ylim=range(dff))
# axis(1, at=pretty(mv.points1[2,]), 
#      labels=sprintf("%g%%", pretty(mv.points1[2,]) * 100))
# axis(2, at=pretty(dff), labels=sprintf("%g%%", pretty(dff) * 100))
# box()
# grid()
par(p)