# Are monthly S&P accumulation index equity returns 
# normally distributed?

# read data from Monthly.csv file specially extracted
# from the Excel data spreadsheet
monthly <- read.table('Monthly.csv', header=TRUE, sep=',')

# convert Date column to Date object type
monthly$Date <- as.Date(as.character(monthly$Date))
spr <- diff(log(monthly[,"S.PCOMP.RI."]))
h <- hist(spr, breaks=40, col="blue", density=20)
xfit <- seq(min(spr), max(spr), length=40)
yfit <- dnorm(xfit, mean=mean(spr), sd=sd(spr))
yfit <- yfit * diff(h$mids[1:2]) * length(spr)
lines(xfit, yfit, col="magenta", lwd=2)

library(fBasics)
print(pchiTest(spr))
cat("=================\n")
print(jarqueberaTest(spr))

# right <- h$breaks[-1]
# left <- h$breaks[-length(h$breaks)]
# m <- mean(h$counts)
# s <- sd(h$counts)
# thnorm <- function(l,r) pnorm(r, m, s) - pnorm(l, m, s)
# thval <- mapply(thnorm, left, right)