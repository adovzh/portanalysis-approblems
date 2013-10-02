library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))

p <- par(cex.axis=.7)
means <- colMeans(weekly.returns)
risks <- colStdevs(weekly.returns)
plot(risks, means, pch=16, col=rainbow(15), axes=FALSE)
axis(1, at=pretty(risks), 
     labels=sprintf("%.1f%%", pretty(risks) * 100))
axis(2, at=pretty(means),
     labels=sprintf("%.2f%%", pretty(means) * 100))
text(risks, means, colnames(weekly), cex=.6, adj=c(NA,-0.8), offset=10)
grid()
par(p)

# mean-variance efficient
mve <- function(i) {
  all(means[i] > means[which(risks < risks[i])])
}

cat("Efficient mean-variance stocks are:\n")
print(colnames(weekly)[which(sapply(1:15, mve))])
