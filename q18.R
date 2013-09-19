# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date), format='%d/%m/%y')

cc.return <- function(vals) {
  len <- length(vals)
  log(vals[2:len]/vals[1:(len-1)])
}

returns.stats <- function(vals) {
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
names(indices) <- c("Straits Times",
                    "S&P",
                    "All Ordinaries",
                    "Indian")
stats <- sapply(indices, returns.stats)
bar.cols <- c("#8782FF", "#852053")
barplot(stats, beside=TRUE, col=bar.cols,
        las=1)
legend("top", rownames(stats), col=bar.cols, pch=15)
title("Statistics of returns in domestic currency")

print(stats)