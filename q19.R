# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date), format='%d/%m/%y')

cc.return <- function(vals) {
  len <- length(vals)
  log(vals[2:len]/vals[1:(len-1)])
}

returns.stats <- function(vals, rates) {
  vals <- vals / rates
  cc.returns <- cc.return(vals)
  result <- c(12 * mean(cc.returns), sqrt(12) * sd(cc.returns))
  names(result) <- c("Annual average return", 
                     "Standard deviation of return")
  result
}

indices <- list(monthly$StraitsTimesAccum,
                monthly$S.PCOMP.RI.,
                monthly$AllOrdsAccum,
                monthly$INDIA.DS..Accum.)
rates <- list(monthly$AUD.SGD,
              monthly$AUD.USD,
              1,
              monthly$AUD.INR)
names <- c("Straits Times",
           "S&P",
           "All Ordinaries",
           "Indian")
names(indices) <- names
names(rates) <- names

stats <- mapply(returns.stats, indices, rates)
bar.cols <- c("#8782FF", "#852053")
barplot(stats, beside=TRUE, col=bar.cols,
        las=1)
legend("top", rownames(stats), col=bar.cols, pch=15)
title("Statistics of returns in AUD")

print(stats)