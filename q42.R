library(timeSeries)
source("common.R")
source("bonds.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
bond.returns <- timeSeries(data=bond.hpr.monthly(usb[-length(usb)], usb[-1]),
                           charvec=rownames(usb)[-1])
names(bond.returns) <- names(usb)

sp.returns <- na.omit(fapply(monthly[,"S.PCOMP.RI."], FUN=returns))

# bond/equity returns
be.returns <- .5*bond.returns + .5*sp.returns
colnames(be.returns) <- "Bond/Equity"

sdret <- function(series) c(sd=sd(series)*sqrt(12), ret=mean(series)*12)
stats <- colStats(cbind(sp.returns, bond.returns, be.returns), 
                  FUN=sdret)

print(stats)

plot(stats[1,], stats[2,], 
     xlim=c(0, 1.3 * max(stats[1,])), 
     ylim=c(0, 1.3 * max(stats[2,])), 
     col=c("red", "blue", "green"), 
     pch=16,
     xlab="Risk", ylab="Return")
text(stats[1,], stats[2,], colnames(stats), adj=-.2, cex=.8)
