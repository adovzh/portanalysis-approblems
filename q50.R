library(timeSeries)
source("common.R")
source("efficientp.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly <- weekly[, colnames(weekly) != "WSF"]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
means <- colMeans(weekly.returns) * 52
sigma <- cov(weekly.returns) * 52

gmvp <- findGMVP(sigma)
max.ret.mvp <- findMVP(sigma=cov(weekly.returns), 
                       ret=means,
                       target.return=max(means))

conv.comb.f <- function(w1, w2) {
  ms <- means %*% cbind(w1, w2)
  function(alpha) crossprod(c(alpha, 1 - alpha), c()
}

aa <- function(alpha) {
  z <- c(alpha, 1 - alpha) %*% rbind(gmvp, max.ret.mvp)
}