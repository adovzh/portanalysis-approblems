# Are monthly All Ordinaries accumulation index equity returns 
# normally distributed?

source("common.R")
monthly <- loadMonthly()
aoi <- diff(log(monthly[,"AllOrdsAccum"]))
plotHistAndNorm(aoi, name="Histogram of All Ord. Acc. Index")

library(fBasics)
print(pchiTest(aoi))
cat("=================\n")
print(jarqueberaTest(aoi))
