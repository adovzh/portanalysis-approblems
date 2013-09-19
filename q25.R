# Leptokurtosis (too many extreme observations) is often observed 
# in financial market returns.  
# Use the monthly Australian All Ords Accumulation data to compute:  

source("common.R")
monthly <- loadMonthly()
aoi <- diff(log(monthly[,"AllOrdsAccum"]))

# (1) the number of observations that are outside the two standard 
# deviation range from the mean 
outliers <- length(extreme.obs(aoi))
cat(sprintf("a) %d\n", outliers))

# (2) the ratio of the number of observed observations 
# in these tails compared with that expected under 
# a normal distribution.
outliers.prob <- outliers / length(aoi)
norm.outliers.prob <- 2 * pnorm(mean(aoi) - 2 * sd(aoi),
                                mean=mean(aoi), sd=sd(aoi))
cat(sprintf("b) %f\n", outliers.prob / norm.outliers.prob))
