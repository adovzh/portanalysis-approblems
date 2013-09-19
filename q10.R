# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date), format='%d/%m/%y')

# extract columns of interest
sp <- monthly[, c('Date', 'S.PCOMP', 'AUD.USD')]
sp$S.PAUD <- sp$S.PCOMP / sp$AUD.USD

cc.returns <- function(values) {
  len <- length(values)
  log(values[2:len] / values[1:(len-1)])
}

cat(sprintf("Average annual c.c. return on S&P in USD: %.4f%%\n", 
    12 * 100 * mean(cc.returns(sp$S.PCOMP))))
cat(sprintf("Average annual c.c. return on S&P in AID: %.4f%%\n", 
    12 * 100 * mean(cc.returns(sp$S.PAUD))))
