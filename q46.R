library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())[,1:15]
weekly.returns <- na.omit(fapply(weekly, FUN=returns))

stat.f <- function(x) c(Mean=mean(x) * 52, Stdev=sd(x) * sqrt(52))
colors <- rainbow(n=15, s=.8, v=.7)

oldpar <- par(mfrow=c(2,1), cex.axis=.7)
stats <- colStats(weekly.returns, stat.f) 
barplot(stats[1,], col=colors)
title("Annualised expected returns")
barplot(stats[2,], col=colors)
title("Annualised standard deviations")

par(oldpar)
