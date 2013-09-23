library(timeSeries)
source("common.R")
source("bonds.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
bond.returns <- timeSeries(data=bond.hpr.monthly(usb[-length(usb)], usb[-1]),
                           charvec=rownames(usb)[-1])

# print statistics
library(fBasics)
print(cbind(basicStats(bond.returns), basicStats(usb)))
