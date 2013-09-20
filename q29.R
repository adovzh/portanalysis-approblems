source("common.R")
source("runstest.R")

monthly <- loadMonthly()
sp <- diff(log(monthly[,"AUSTALL"]))
# runs
rt <- runs.test(sp)

barplot(table(rt$runs), col="#818AFC", space=.8) 
title(main="All Ordinaries Run length", xlab="Run length", ylab="Frequency")
cat(sprintf("a) probability of an up month: %.2f%%\n", rt$upmonth.prob))
cat(sprintf("b) probability of a down month: %.2f%%\n", rt$downmonth.prob))
cat(sprintf("c) longest run: %d\n", rt$longest.run))

# a <- rt$runs
# lapply(mapply(function(a,len) sp[seq(to=a, length.out=len)], cumsum(a), a), sign)
