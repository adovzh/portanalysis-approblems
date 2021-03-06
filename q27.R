source("common.R")
source("runstest.R")

monthly <- loadMonthly()
sp <- diff(log(monthly[,"S.PCOMP"]))
# runs
rt <- runs.test(sp)

barplot(table(rt$runs), col="#818AFC", space=.8) 
title(main="S&P Run length", xlab="Run length", ylab="Frequency")
cat(sprintf("a) probability of an up month: %.2f%%\n", rt$upmonth.prob))
cat(sprintf("b) probability of a down month: %.2f%%\n", rt$downmonth.prob))
cat(sprintf("c) longest run: %d\n", rt$longest.run))