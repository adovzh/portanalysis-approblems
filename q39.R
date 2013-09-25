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

horizon <- c(1,5,10)
names(horizon) <- paste(horizon, "y", sep="")

# probability of positive excess returns ober the horizon h
exc.prob <- function(h) {
  m <- 12 * h * mean(excess.returns)
  s <- sqrt(12 * h) * sd(excess.returns)
  pos.prob <- 1 - pnorm(0, mean=m, sd=s)
  row <- c(m, s, pos.prob)
  names(row) <- c("Mean", "Stdev", "Pos. Prob")
  return(row)
}

report <- sapply(horizon, exc.prob)
print(report)
cat("\nSo, the probability for equities beat bonds is:\n")
print(report["Pos. Prob",])