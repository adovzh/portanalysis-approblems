library(timeSeries)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())
weekly <- cbind(weekly, SPAUD=weekly[,"S.PCOMP"] / weekly[,"USAUSTR"])
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52
alphas <- seq(-1, 1, by=.05)


# 14-stock (ex-15 stock) portfolio
p1.stocks <- names(weekly)[c(1:3, 5:15)]
sigma1 <- cov(weekly.returns[,p1.stocks]) * 52
p1.w1 <- findMVP(sigma1, means[p1.stocks], min(means[p1.stocks]))
p1.w2 <- findMVP(sigma1, means[p1.stocks], max(means[p1.stocks]))
mv.coords1 <- mv.coords.gen(sigma1, means[p1.stocks])
z1 <- z.gen(p1.w1, p1.w2)
mv.points1 <- sapply(alphas, function(alpha) mv.coords1(z1(alpha)))

# 15-asset (14-stock + US asset) portfolio including US asset
p2.stocks <- c(names(weekly)[c(1:3, 5:15)], "SPAUD")
sigma2 <- cov(weekly.returns[,p2.stocks]) * 52
p2.w1 <- findMVP(sigma2, means[p2.stocks], min(means[p2.stocks]))
p2.w2 <- findMVP(sigma2, means[p2.stocks], max(means[p2.stocks]))
mv.coords2 <- mv.coords.gen(sigma2, means[p2.stocks])
z2 <- z.gen(p2.w1, p2.w2)
mv.points2 <- sapply(alphas, function(alpha) mv.coords2(z2(alpha)))

xrng <- range(mv.points1[1,], mv.points2[1,])
yrng <- range(mv.points1[2,], mv.points2[2,])
plot(mv.points1[1,], mv.points1[2,], pch=16, axes=FALSE,
     xlab="Risk", ylab="Return",
     xlim=xrng,
     ylim=yrng)
points(mv.points2[1,], mv.points2[2,], pch=16, col="blue")
axis(1, at=pretty(xrng), 
     labels=sprintf("%.0f%%", pretty(xrng)*100))
axis(2, at=pretty(yrng), 
     labels=sprintf("%.0f%%", pretty(yrng)*100))
legend("right", c("15(14)-stock", "11-stock incl. US"), 
       pch=16, col=c("black", "blue"), cex=.8)
box()
grid()
