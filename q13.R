# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date), format='%d/%m/%y')

# extract columns of interest
index.name <- 'StraitsTimesAccum'
exchange.rate.name <- 'AUD.SGD'
idx <- monthly[, c('Date', index.name, exchange.rate.name)]
idx.aud <- idx[, index.name] / idx[, exchange.rate.name]
idx.growth <- idx[, index.name] / idx[1, index.name]
idx.aud.growth <- idx.aud / idx.aud[1]

px <- pretty(idx$Date, n=10)
px.lab <- format(px, '%b-%y')
pf <- pretty(c(idx.growth, idx.aud.growth), n=10)

oldpar <- par(no.readonly=TRUE)
par(cex=.8, tcl=-.3)
plot(idx$Date, idx.growth, type='l', col='magenta', lwd=2, 
     xlab='Date', ylab='Growth', axes=F)
axis(1, at=px, lab=px.lab)
axis(2, at=pf, lab=sprintf('%.1f%%', pf))
box()
abline(h=pf, col='lightgray', lwd=.5)
lines(idx$Date, idx.aud.growth, col='darkblue', lwd=2)
title('Straits Times Accumulation Index Growth')
legend('topleft', c('In SGD', 'In AUD'), col=c('magenta', 'darkblue'),
       lwd=c(2, 2))
par(oldpar)