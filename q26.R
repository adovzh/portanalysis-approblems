source("common.R")
monthly <- loadMonthly()
idx <- monthly[, c("S.PCOMP.RI.", "AllOrdsAccum", 
                   "StraitsTimesAccum", "INDIA.DS..Accum.")]
colnames(idx) <- c("S&P", "Aust", "Singapore", "India")
idx <- sapply(log(idx), diff)
outliers.ratio <- apply(idx, 2, function(x) {
  norm.outliers.prob <- 2 * pnorm(mean(aoi) - 2 * sd(aoi),
                                  mean=mean(aoi), sd=sd(aoi))  
  length(extreme.obs(x)) / length(x) / norm.outliers.prob
})

barplot(outliers.ratio, col="#818AFC", space=1.3)