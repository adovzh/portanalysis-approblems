library(timeSeries)
library(fBasics)
source("common.R")
source("bonds.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
bond.returns <- timeSeries(data=bond.hpr.monthly(usb[-length(usb)], usb[-1]),
                           charvec=rownames(usb)[-1])
names(bond.returns) <- names(usb)

sp.returns <- na.omit(fapply(monthly[,"S.PCOMP.RI."], FUN=returns))
ret.stats <- cbind(basicStats(bond.returns), basicStats(sp.returns))[c("Mean", "Stdev"), ]
rs.annualised <- ret.stats * c(12, sqrt(12))
print(rs.annualised)
