library(timeSeries)
source("common.R")
source("runstest.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
rt <- runs.test(diff(usb$USBD10Y))
barplot(table(rt$runs), col="#818AFC", space=.8) 
title(main="US Bond Yield run length", xlab="Run length", ylab="Frequency")
