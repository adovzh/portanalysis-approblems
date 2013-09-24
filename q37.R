library(timeSeries)
source("common.R")
source("bonds.R")

monthly <- toTimeSeries(loadMonthly())
usb <- monthly[,"USBD10Y"]
bond.returns <- timeSeries(data=bond.hpr.monthly(usb[-length(usb)], usb[-1]),
                           charvec=rownames(usb)[-1])
names(bond.returns) <- names(usb)

sp.returns <- na.omit(fapply(monthly[,"S.PCOMP.RI."], FUN=returns))
excess.returns <- sp.returns - bond.returns

x <- seq(from=-.5, to=.5, by=.01)
plot(x, dnorm(x, mean=mean(bond.returns)*12, sd=sd(bond.returns)*sqrt(12)),
     ylab="Probability", 
     col="darkgreen", type="l", lwd=2)
points(x, dnorm(x, mean=mean(sp.returns)*12, sd=sd(sp.returns)*sqrt(12)),
       col="darkred", type='l', lwd=2)
points(x, dnorm(x, mean=mean(excess.returns)*12, sd=sd(excess.returns)*sqrt(12)),
       col="darkblue", type='l', lwd=2)
legend("topright", c("USBD10Y", "S&P Accum", "Excess"), 
       col=c("darkgreen", "darkred", "darkblue"), lwd=2)
