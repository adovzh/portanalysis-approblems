library(timeSeries)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly <- weekly[, colnames(weekly) != "WSF"]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52
risks <- colStdevs(weekly.returns) * sqrt(52)
sigma <- cov(weekly.returns) * 52

w1 <- findGMVP(sigma)
w2 <- findMVP(sigma, 
              ret=means,
              target.return=max(means))

# conv.comb.f <- function(w1, w2) {
#   ms <- means %*% cbind(w1, w2)
#   function(alpha) crossprod(c(alpha, 1 - alpha), c()
# }

z <- function(alpha) {
  cbind(w1, w2) %*% c(alpha, 1 - alpha)
}

mv.coords <- function(alpha) {
  risk <- t(z(alpha)) %*% sigma %*% z(alpha)
  ret <- means %*% z(alpha)
  c(risk, ret)
}

alphas <- seq(-1, 1, by=.1)
mv.points <- sapply(alphas, mv.coords)

eff.risks <- mv.points[1,]
eff.returns <- mv.points[2,]
xrng <- c(eff.risks)
plot(eff.risks, eff.returns, pch=16, axes=FALSE)
axis(1, at=pretty(xrng), 
     labels=sprintf("%.2f%%", pretty(xrng)*100))
axis(2, at=pretty(eff.returns), 
     labels=sprintf("%.2f%%", pretty(eff.returns)*100))
box()
grid()
