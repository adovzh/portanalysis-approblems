source("common.R")
source("partition.R")

monthly <- loadMonthly()
sp <- diff(log(monthly[,"S.PCOMP"]))
# runs
psp <- partition.returns(sp)
barplot(table(psp), col="#818AFC", space=.8)
title(main="Run length", xlab="Run length", ylab="Frequency")

# signed runs for S&P
spsp <- psp * rep(c(1,-1), length.out=length(psp))
upmonth.prob <- 100 * sum(spsp[spsp > 0]) / sum(psp)
downmonth.prob <- 100 * sum(-spsp[spsp < 0]) / sum(psp)
longest.run <- max(psp)

cat(sprintf("a) probability of an up month: %.2f%%\n", upmonth.prob))
cat(sprintf("b) probability of a down month: %.2f%%\n", downmonth.prob))
cat(sprintf("c) longest run: %d\n", longest.run))