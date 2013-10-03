library(timeSeries)
source("common.R")

weekly <- toTimeSeries(loadWeekly())
weekly.stocks <- weekly[, 1:15]
weekly.stocks <- weekly.stocks[, colnames(weekly.stocks) != "WSF"]
weekly.sp <- weekly[,"S.PCOMP"]
weekly.fx <- weekly[, "USAUSTR"]

# convert S&P into AUD
weekly.sp <- weekly.sp / weekly.fx
weekly.sp.returns <- na.omit(fapply(weekly.sp, FUN=returns))

# calculate mean and standard deviation of S&P expressed in AUD
cat("S&P in AUD statistics:\n")
print(asReport(c(mean=mean(weekly.sp.returns)*52, 
                 sd=sd(weekly.sp.returns)*sqrt(52)),
               "S&P"))
