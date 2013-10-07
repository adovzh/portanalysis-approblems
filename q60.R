library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
# exclude GFC data
weekly.returns <- window(weekly.returns, start="2009-01-01",
                         end=Sys.timeDate())
means <- colMeans(weekly.returns) * 52

stocks <- names(weekly)[c(1:3, 5:15)]
sigma <- cov(weekly.returns[,stocks]) * 52

library(quadprog)

# short sales allowed (SSA)
ssa <- findGMVP(sigma)

# short sales not allowed (SSN)
s <- solve.QP(sigma,
              matrix(rep(0, ncol(sigma))),
              cbind(rep(1, ncol(sigma)),
                    diag(ncol(sigma))),
              c(1, rep(0, ncol(sigma))), 1)

report <- cbind(asReport(ssa, "[SSA]"), 
                asReport(refine.weights(s$solution), "[SSN]", colnames(sigma)))

stats <- colStats(report, function(x) {
  ret <- crossprod(x, means[stocks])
  vr <- t(x) %*% sigma %*% x
  c(Sum=sum(x), 
    Return=ret,
    Variance=vr,
    StDeviation=sqrt(vr))
})

print(report, digits=2)
print(stats * 100, digits=3)

ylab <- pretty(report)
cols <- c("#9BA1FC", "#9A3866")
barplot(t(report), col=cols, beside=TRUE, las=2,
        yaxt="n")
axis(2, ylab, labels=sprintf("%g%%", ylab * 100) ,las=2)
legend("topleft", c("SSA", "SSN"), col=cols, pch=15)
box()
