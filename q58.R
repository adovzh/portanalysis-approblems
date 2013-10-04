library(timeSeries)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52
alphas <- seq(-1, 1, by=.05)

stocks <- names(weekly)[c(1:3, 5:11)]
sigma <- cov(weekly.returns[,stocks]) * 52

mv.coords <- mv.coords.gen(sigma, means[stocks])


library(quadprog)

# expected return min, backing stocks 25%
p1.w1 <- solve.QP(sigma,
               matrix(rep(0, ncol(sigma))),
               cbind(rep(1, ncol(sigma)),
                     means[stocks],
                     stocks %in% BANKING_STOCKS),
               c(1, min(means[stocks]), .25), 3)$solution

# expected return max, backing stocks 25%
p1.w2 <- solve.QP(sigma,
               matrix(rep(0, ncol(sigma))),
               cbind(rep(1, ncol(sigma)),
                     means[stocks],
                     stocks %in% BANKING_STOCKS),
               c(1, max(means[stocks]), .25), 3)$solution

z1 <- z.gen(p1.w1, p1.w2)
mv.points1 <- sapply(alphas, function(alpha) mv.coords(z1(alpha)))

# expected return min
p2.w1 <- solve.QP(sigma,
                  matrix(rep(0, ncol(sigma))),
                  cbind(rep(1, ncol(sigma)),
                        means[stocks]),
                  c(1, min(means[stocks])), 2)$solution

# expected return max
p2.w2 <- solve.QP(sigma,
                  matrix(rep(0, ncol(sigma))),
                  cbind(rep(1, ncol(sigma)),
                        means[stocks]),
                  c(1, max(means[stocks])), 2)$solution

z2 <- z.gen(p2.w1, p2.w2)
mv.points2 <- sapply(alphas, function(alpha) mv.coords(z2(alpha)))

xrng <- range(mv.points1[1,], mv.points2[1,])
yrng <- range(mv.points1[2,], mv.points2[2,])
plot(mv.points1[1,], mv.points1[2,], pch=16, axes=FALSE,
     xlab="Risk", ylab="Return",
     xlim=xrng,
     ylim=yrng)
points(mv.points2[1,], mv.points2[2,], pch=16, col="blue")
axis(1, at=pretty(xrng), 
     labels=sprintf("%g%%", pretty(xrng)*100))
axis(2, at=pretty(yrng), 
     labels=sprintf("%g%%", pretty(yrng)*100))
legend("topleft", c("Bank restricted", "Unrestricted"), 
       pch=16, col=c("black", "blue"), cex=.8)
box()
grid()
