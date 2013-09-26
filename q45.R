library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
cum.returns <- colCumsums(weekly.returns)

oldpar <- par(mfrow=c(2,1))
colors <- rainbow(n=15)
plot(cum.returns[,1], ylim=range(cum.returns), col=colors[1],
     ylab="Cumulative returns")
title(main="Cumulative return of 15 stocks")
for(i in 2:15) {
  lines(cum.returns[,i], col=colors[i])
}

total.returns <- colSums(weekly.returns)
min.index <- match(min(total.returns), total.returns)
max.index <- match(max(total.returns), total.returns)
range.index <- c(min.index, max.index)
plot(cum.returns[,min.index], type="l", 
     ylim=range(cum.returns[,range.index]),
     col=colors[min.index], lwd=2)
lines(cum.returns[,max.index], col=colors[max.index], lwd=2)
title(main="Best and worst stocks")
legend("bottomleft", colnames(weekly)[range.index], 
       lwd=2, col=colors[range.index])

par(oldpar)