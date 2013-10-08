library(timeSeries)
source("common.R")
source("bonds.R")

bill <- "ADBR090"
bond <- "ABND10Y"
bonds <- c(bill, bond)

weekly <- toTimeSeries(loadWeekly())
weekly <- weekly[,bonds]

# bill holding period returns
bill.hpy <- function(X) {
  X <- as.matrix(X)
  rbind(X[1,, drop=FALSE] * NA, apply(X, 2, function(x) {
    rt <- x[-1]
    rtm1 <- x[-length(x)]
    bill.hpr.weekly(rtm1, rt)
  }))
}

weekly.bill.hpy <- na.omit(fapply(weekly[,bill], FUN=bill.hpy))
cat("Bill annualised HPY:\n")
hpy.bill.stats <- colStats(weekly.bill.hpy, function(x) {
  c(mean=mean(x)*52 * 100, stdev=sd(x) * sqrt(52) * 100)
})
print(hpy.bill.stats, digits=3)

cat("\nBill raw yields:\n")
raw.bill.stats <- colStats(weekly[,bill], function(x) {
  c(mean=mean(x) * 100, stdev=sd(x) * 100)
})
print(raw.bill.stats, digits=3)

# bond
bond.hpy <- function(X) {
  X <- as.matrix(X)
  rbind(X[1,, drop=FALSE] * NA, apply(X, 2, function(x) {
    bond.hpr.weekly(X[-length(X)], X[-1])
  }))
}
weekly.bond.hpy <- na.omit(fapply(weekly[,bond], FUN=bond.hpy))
cat("\nBond annualised HPY:\n")
hpy.bond.stats <- colStats(weekly.bond.hpy, function(x) {
  c(mean=mean(x)*52 * 100, stdev=sd(x) * sqrt(52) * 100)
})
print(hpy.bond.stats, digits=3)

cat("\nBond raw yields:\n")
raw.bond.stats <- colStats(weekly[,bond], function(x) {
  c(mean=mean(x) * 100, stdev=sd(x) * 100)
})
print(raw.bond.stats, digits=3)

h <- t(cbind(raw.bill.stats, hpy.bill.stats,
             raw.bond.stats, hpy.bond.stats))
dim(h) <- c(2,2,2)
dimnames(h) <- list(c("Nominal yields", "Weekly HPY"),
                    c("90-Day", "10-Yr"), 
                    c("Average yields", "St.deviation of yields"))
cols <- c("#9BA1FC", "#9A3866")
p <- par(mfcol=c(2,1), las=1, cex.axis=.7, cex.lab=.8)

for (i in dimnames(h)[[3]]) {
  barplot(h[,,i], beside=TRUE, col=cols, yaxt="n", ylab=i)
  axis(2, at=pretty(h), labels=sprintf("%g%%", pretty(h)))
  legend("topleft", dimnames(h)[[1]], col=cols, pch=15, cex=.8)
  box()
}


par(p)
