library(timeSeries)
source("common.R")
source("bonds.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
bond.returns <- timeSeries(data=bond.hpr.monthly(usb[-length(usb)], usb[-1]),
                           charvec=rownames(usb)[-1])
names(bond.returns) <- "Adj.Bond Returns"

# print statistics
library(fBasics)
stat.names <- c("Mean", "Stdev")
bond.stats <-basicStats(bond.returns)[stat.names, ] * c(12, sqrt(12))
ret.stats <- cbind(bond.stats, basicStats(usb))[stat.names, ]
print(ret.stats)
