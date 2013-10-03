# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date))

cc.return <- function(vals) {
  len <- length(vals)
  log(vals[2:len]/vals[1:(len-1)])
}

cc.returns <- cc.return(monthly$AllOrdsAccum)

alt.annual.growth <- function(vals) {
  len <- length(vals)
  log(vals[len]/vals[1]) / ((len-1)/12)
}

ans <- c(12 * mean(cc.returns),
         12 * var(cc.returns),
         2 * sqrt(3) * sd(cc.returns),
         alt.annual.growth(monthly$AllOrdsAccum))
names(ans) <- c("expected return",
                "variance of return",
                "standard deviation of return",
                "alternative expected return")
# return.alternative.mean <- log(monthly$AllOrdsAccum[len]/monthly$AllOrdsAccum[1])/((len-1)/12)
ans <- as.matrix(ans)
colnames(ans) <- "Value"

print(ans)
