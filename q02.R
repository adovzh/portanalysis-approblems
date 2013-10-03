# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date))

# plot the index
# TODO: decorate the graph
plot(monthly$Date, monthly$AUSTALL, type='l')

# What was the longest span of time that the Index spent 
# below a previous peak?
next.higher <- function(series, index) {
  cur.date <- series[index, 1]
  cur.price <- series[index, 2]
  j <- index + 1
  len <- dim(series)[1]
  
  while (j < len && series[j, 2] < cur.price) j <- j + 1
  
  series[j, 1] - series[index, 1]
}

retreat.bottom.ratio <- function(series, index) {
  len <- dim(series)[1]
  min(spdata[seq(index, len), 2]) / series[index, 2]
}

# slice only dates and AUSTALL index
spdata <- monthly[, c('Date', 'AUSTALL')]
spdata.size <- dim(spdata)[1]

# calculate the vector of retreats and find its maximum
retreats <- sapply(seq(from=1, to=spdata.size-1),
                   function(index) next.higher(spdata, index))
max.retreat.index <- match(max(retreats), retreats)
print(spdata[max.retreat.index, ])
cat(sprintf("Longest retreat time: %d days\n", retreats[max.retreat.index]))

# During this period, how far (in percentage terms) 
# did the index retreat?  
max.retreat.value <- retreat.bottom.ratio(spdata, max.retreat.index)
cat(sprintf("Max retreat drop: %.2f%%\n", max.retreat.value * 100))

# How many months were “winning” months (ie had a price increase)?
returns <- log(spdata[2:spdata.size, 2] / spdata[1:(spdata.size-1), 2])
spdata <- cbind(spdata, returns=c(0, returns))
cat(sprintf("Winning months: %d\n", dim(spdata[spdata$returns > 0, ])[1]))

# How many months were losing months?
cat(sprintf("Losing months: %d\n", dim(spdata[spdata$returns < 0, ])[1]))
