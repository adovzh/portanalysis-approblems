source("common.R")
source("partition.R")

monthly <- loadMonthly()
sp <- diff(log(monthly[,"S.PCOMP"]))
psp <- partition.returns(sp)
hist(psp)