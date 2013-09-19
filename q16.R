# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date), format='%d/%m/%y')

annual.return <- function(dates, vals, rates) {
  aud.vals <- vals / rates
  min.index <- match(min(dates), dates)
  max.index <- match(max(dates), dates)
  dt <- as.numeric(dates[max.index]-dates[min.index])/365.25
  areturn <- log(vals[max.index]/vals[min.index])/dt
  areturn.aud <- log(aud.vals[max.index]/aud.vals[min.index])/dt
  modulo.len <- function(v,i) v[(i-1) %% length(v) + 1]
  exch.return <- log(modulo.len(rates, max.index)/modulo.len(rates, min.index))/dt
  result <- c(areturn, areturn.aud, exch.return) * 100
  names(result) <- c("Domestic returns", "AUD returns", "Exchange rate return")
  result
}

ar <- mapply(function(vals, rates) annual.return(monthly$Date, vals, rates),
             list(US=monthly$S.PCOMP, 
                  Australia=monthly$AUSTALL,
                  Singapore=monthly$SNGPORI, 
                  India=monthly$INDIA.DS..Price.),
             list(US=monthly$AUD.USD,
                  Australia=1,
                  Singapore=monthly$AUD.SGD,
                  India=monthly$AUD.INR))

bar.cols <- c("#8782FF", "#852053", "#FFFFC0")
barplot(ar, beside=TRUE, col=bar.cols, ylab="Average annual return (%)",
        las=1)
