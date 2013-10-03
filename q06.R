# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date))

# extract columns if interest
index.name <- 'S.PCOMP.RI.'
index.vname <- 'S&P 500 Accumulation Index'
sp.acc <- monthly[,c('Date', index.name)]

# $1 growth
sp.acc$growth <- sp.acc[,index.name] / sp.acc[1, index.name]
px <- pretty(sp.acc$Date, n=10)
px.lab <- format(px, '%b-%Y')
pf <- pretty(sp.acc$growth)

last.date <- max(sp.acc$Date)
last.price <- sp.acc[sp.acc$Date==last.date, index.name]
period <- julian(last.date, origin=sp.acc[1,1]) / 365.25
avg.rate <- log(last.price/sp.acc[1, index.name]) / period
avg.growth <- exp(julian(sp.acc$Date, origin=sp.acc[1,1]) / 365.25 * avg.rate)

oldpar <- par(no.readonly=TRUE)
par(cex=.8, tcl=-.3, ask=T)
plot(sp.acc$Date, sp.acc$growth, type='l', xlab='Time', 
     ylab=sprintf('Value($) of %s', index.vname), col='magenta', 
     lwd=3, axes=FALSE,
     xaxs="i", yaxs="i", ylim=range(pf))
axis(1, at=px, lab=px.lab, lwd=0, lwd.ticks=.5, col.ticks='lightgray')
axis(2, at=pf, las=1, lwd=0, lwd.ticks=.5, col='lightgray')
abline(h=pf, col='lightgray', lwd=1)
lines(sp.acc$Date, avg.growth, col='darkblue', lwd=2)
legend('topleft', c('Average growth', 'Actual growth'), 
       col=c('darkblue', 'magenta'), lwd=c(2,3))
title("S&P 500 Growth")

var.growth <- sp.acc$growth / avg.growth - 1
abslimit <- max(abs(range(var.growth)))
vpf <- pretty(c(-abslimit, abslimit), n=10)

par(mex=1.3)
plot(sp.acc$Date, var.growth, type="l", xlab="Time",
     ylab="Variation around the actual growth", col="magenta",
     lwd=3, ylim=range(vpf), yaxt="n")
axis(2, at=vpf, labels=sprintf("%.0f%%", vpf*100), las=1)
abline(h=0, lwd=.8)
title("S&P 500 Variation Around the Mean")
par(oldpar)