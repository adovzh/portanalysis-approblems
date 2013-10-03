# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')
# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date))

len <- dim(monthly)[1]
sp.growth <- log(monthly$S.PCOMP/monthly[1, 'S.PCOMP'])
sp.acc.growth <- log(monthly$S.PCOMP.RI./monthly[1, 'S.PCOMP.RI.'])

yvals <- c(sp.growth, sp.acc.growth)
ylab <- pretty(yvals)

plot(monthly$Date, sp.growth, type='l', lwd=3,
     xlab='Date',
     yaxt='n',
     ylim=range(sp.growth, sp.acc.growth),
     ylab="Growth since start")
axis(2, at=ylab, lab=sprintf("%.0f%%", ylab*100), las=1)
lines(monthly$Date, sp.acc.growth, col='blue')
abline(h=ylab, col="lightgray")
legend("topleft", c("Accumulation", "Price"), col=c("blue", "black"),
       lwd=c(3,1))