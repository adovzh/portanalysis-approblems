source("common.R")

monthly <- loadMonthly()
sp <- diff(log(monthly[,"S.PCOMP"]))
correl <- cor(sp[-length(sp)], sp[-1])
Z <- correl / (length(sp) ^ (-.5))

cat(sprintf("Correl = %.3f, Z = %.3f\n", correl, Z))
cat("There are no significant positive lag-1 ")
cat("autocorrelations for S&P\n")