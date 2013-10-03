# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date))

first.date <- min(monthly$Date)
last.date <- max(monthly$Date)
columns <- c("Date", "S.PCOMP", "S.PCOMP.RI.")

first.date.row <- monthly[monthly$Date==first.date, columns]
last.date.row <- monthly[monthly$Date==last.date, columns]

dt <- julian(last.date.row$Date, 
             origin=first.date.row$Date) / 365.25
price.index <- log(last.date.row$S.PCOMP / 
                     first.date.row$S.PCOMP) / dt
accum.index <- log(last.date.row$S.PCOMP.RI. / 
                     first.date.row$S.PCOMP.RI.) / dt
cat(sprintf("Accum index: %.2f%%\n", accum.index * 100))
cat(sprintf("Price index: %.2f%%\n", price.index * 100))
cat(sprintf("Dividend: %.2f%%\n", (accum.index - price.index) * 100))