loadMonthly <- function(src="Monthly.csv") {
  # read data from Monthly.csv file specially extracted
  # from the Excel data spreadsheet
  monthly <- read.table(src, header=TRUE, sep=',')
  
  # convert Date column to Date object type
#   monthly$Date <- as.Date(as.character(monthly$Date), format='%d/%m/%y')
  
  return(monthly)
}

loadWeekly <- function() loadMonthly("Weekly.csv")

plotHistAndNorm <- function(data, name) {
  h <- hist(data, breaks=40, col="blue", density=20, main=name)
  xfit <- seq(min(data), max(data), length=40)
  yfit <- dnorm(xfit, mean=mean(data), sd=sd(data))
  yfit <- yfit * diff(h$mids[1:2]) * length(data)
  lines(xfit, yfit, col="magenta", lwd=2)  
}

# those that are outside 2 sd deviations from the mean
extreme.obs <- function(data) {
  data[abs(data - mean(data)) > 2 * sd(data)]
}

toTimeSeries <- function(series) {
  require(timeSeries)
  return(timeSeries(series[,-1], as.character(series[,1])))
}
