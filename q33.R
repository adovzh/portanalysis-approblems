library(timeSeries)
source("common.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
plot(usb)
