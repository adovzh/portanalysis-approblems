library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
weekly <- weekly[,c(1:3, 5:15)]
pointcut <- as.Date(max(time(weekly))) - 731
history <- window(weekly, start=start(weekly), end=pointcut)
investment <- window(weekly, start=pointcut+1, end=end(weekly))
means <- rbind(colMeans(history), colMeans(investment))

p <- par(cex.axis=.7, las=2, no.readonly=TRUE)
cols <- c("#9A3866", "#9BA1FC")
barplot(means, beside=TRUE, col=cols, main="Means")
legend("top", c("1st Half", "2nd Half"), 
       col=cols, pch=15, cex=.8, bty="n")
par(p)
