# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date))

annual.return <- function(dates, vals) {
  min.index <- match(min(dates), dates)
  max.index <- match(max(dates), dates)
  dt <- as.numeric(dates[max.index]-dates[min.index])/365.25
  log(vals[max.index]/vals[min.index])/dt*100
}

ar <- sapply(list(US=monthly$S.PCOMP, 
                  Australia=monthly$AUSTALL,
                  Singapore=monthly$SNGPORI, 
                  India=monthly$INDIA.DS..Price.),
             function(vals) annual.return(monthly$Date, vals))
barplot(ar, col='#818AFC', las=1,
        ylab='Average annual return (%)', space=1.5)
