library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
# exclude GFC data
weekly <- window(weekly, start="2009-01-01",
                 end=Sys.timeDate())
weekly <- weekly[,c(1:3, 5:15)]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))
pointcut <- as.Date(max(time(weekly))) - 731
history.returns <- window(weekly.returns, 
                          start=start(weekly.returns), 
                          end=pointcut)
investment.returns <- window(weekly.returns, 
                             start=pointcut+1, 
                             end=end(weekly.returns))
means <- rbind(colMeans(history.returns), colMeans(investment.returns)) * 52

p <- par(cex.axis=.7, las=2, cex.lab=.8, font.lab=2, no.readonly=TRUE)
cols <- c("#9A3866", "#9BA1FC")
yrng <- pretty(c(means, max(means)*1.4)) 
barplot(means, beside=TRUE, col=cols, main="Expected returns",
        ylim=range(yrng), yaxt="n", ylab="Return")
axis(2, at=yrng, 
     labels=sprintf("%g%%", yrng*100))
legend("top", c("1st Half", "2nd Half"), 
       col=cols, pch=15, cex=.8, bty="n")
par(p)
