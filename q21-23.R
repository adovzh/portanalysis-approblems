# Are monthly S&P accumulation index equity returns 
# normally distributed?

source("common.R")
monthly <- loadMonthly()
spr <- diff(log(monthly[,"S.PCOMP.RI."]))
plotHistAndNorm(spr, name="Histogram of S&P Acc. Index")

library(fBasics)
print(pchiTest(spr))
cat("=================\n")
print(jarqueberaTest(spr))
