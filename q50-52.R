library(timeSeries)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly <- weekly[, colnames(weekly) != "WSF"]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52
risks <- colStdevs(weekly.returns) * sqrt(52)
sigma <- cov(weekly.returns) * 52

w1 <- findMVP(sigma, 
              ret=means,
              target.return=min(means))
w2 <- findMVP(sigma, 
              ret=means,
              target.return=max(means))

mv.coords <- mv.coords.gen(sigma, means)
z <- z.gen(w1, w2)

alphas <- seq(-1, 1, by=.05)
mv.points <- sapply(alphas, function(alpha) mv.coords(z(alpha)))

eff.risks <- mv.points[1,]
eff.returns <- mv.points[2,]
xrng <- c(eff.risks)
plot(eff.risks, eff.returns, pch=16, axes=FALSE,
     xlab="Risk", ylab="Return")
axis(1, at=pretty(xrng), 
     labels=sprintf("%.0f%%", pretty(xrng)*100))
axis(2, at=pretty(eff.returns), 
     labels=sprintf("%.0f%%", pretty(eff.returns)*100))
box()
grid()

# q51
w3 <- findGMVP(sigma)
cat("Global MVP weights:\n")
print(as.matrix(w3))

# a52 - equally weighted portfolio
ew <- rep(1/ncol(weekly.returns), ncol(weekly.returns))
pew <- mv.coords(ew)
points(pew[1], pew[2], col="red", pch=16)
text(pew[1], pew[2], "EQ.WEIGHTED", cex=.6, adj=c(-.2, -.8))
