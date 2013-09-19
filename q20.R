# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date), format='%d/%m/%y')

# print(summary(monthly$S.PCOMP.RI.))

cc.return <- function(vals) {
  len <- length(vals)
  log(vals[2:len]/vals[1:(len-1)])
}

statistics <- function(vals) {
  stats <- c(min(vals), max(vals), mean(vals), 
             sd(vals), var(vals),
             12 * mean(vals), 12 * var(vals))
  names(stats) <- c("Minimum", "Maximum", "Average", 
                    "Standard deviation", "Variance",
                    "Annual expected return",
                    "Annual variance")
  stats
}

stats <- as.matrix(statistics(cc.return(monthly$S.PCOMP.RI.)))
colnames(stats) <- "S&P Acc."
print(stats)