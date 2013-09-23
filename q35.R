library(timeSeries)
source("common.R")
source("bonds.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
bond.returns <- timeSeries(data=bond.hpr.monthly(usb[-length(usb)], usb[-1]),
                           charvec=rownames(usb)[-1])
names(bond.returns) <- names(usb)

# print statistics
library(fBasics)
ret.stats <- cbind(basicStats(bond.returns), basicStats(usb))[c("Mean", "Stdev"), ]
print(ret.stats)