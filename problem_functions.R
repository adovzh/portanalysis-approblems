convert.date.column <- function(series, column, format) {
  series[,column] <- as.Date(as.character(series[,column]), format=format)
}