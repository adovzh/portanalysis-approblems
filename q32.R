source("common.R")

monthly <- loadMonthly()
aoi <- diff(log(monthly[,"AUSTALL"]))
correl <- cor(aoi[-length(aoi)], aoi[-1])
Z <- correl / (length(aoi) ^ (-.5))

cat(sprintf("Correl = %.3f, Z = %.3f\n", correl, Z))
cat("There are almost significant positive lag-1 ")
cat("autocorrelations for AOI.\n")