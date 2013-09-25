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

# probability of positive excess returns ober the horizon h
pprob <- function(h) {
  m <- 12 * h * mean(excess.returns)
  s <- sqrt(12 * h) * sd(excess.returns)
  return(1 - pnorm(0, mean=m, sd=s))
}

# horizon that the excess returns are positive > 90%
sol.horizon <- uniroot(function(h) pprob(h) - .9, c(0, 200))$root
cat(sprintf("Horizon is %.2f years\n", sol.horizon))
