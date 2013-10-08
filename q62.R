library(timeSeries)
source("common.R")

bill <- "ADBR090"
bond <- "ABND10Y"
bonds <- c(bill, bond)

weekly <- toTimeSeries(loadWeekly())
weekly <- weekly[,bonds]

p <- par(las=2, cex.axis=.8)
cols <- c("darkblue", "magenta")
yrng <- range(weekly)
plot(weekly[,bill], type="l", col=cols[1], axes=FALSE,
     xlab="", ylab="Raw Yield")
lines(weekly[,bond], col=cols[2])
axis.timeDate(1, time(weekly), at=pretty(time(weekly)), 
     labels=format(pretty(time(weekly)), "%b-%y"))
axis(2, at=pretty(yrng), 
     labels=sprintf("%g%%", pretty(yrng)*100))
box()
grid()
legend("topright", c("90Day", "10Yr"), col=cols, lwd=1)
par(p)
