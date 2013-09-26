library(timeSeries)
source("common.R")

monthly <- toTimeSeries(loadMonthly())
sp.returns <- na.omit(fapply(monthly[,"S.PCOMP.RI."], FUN=returns))
ind <- factor(rep(1:4, each=length(sp.returns)/4), labels="Column")
# ds <- data.frame(series(sp.returns), ind)

group.vars <- aggregate(series(sp.returns), by=list(ind), FUN=var)
gv <- group.vars[,2] * 12
names(gv) <- group.vars[,1]
barplot(gv, col="#818AFC", space=.8)
title("Subperiod annual variances")
fstat <- max(gv) / min(gv)

# Is it Hartley's test? If yes then:
library(SuppDists)
F.crit <- qmaxFratio(.95, df=30, k=4)
p.val <- pmaxFratio(fstat, df=30, k=4, lower.tail=F)
cat(sprintf("F.stat = %f\n", fstat))
cat(sprintf("F.crit = %f\np.val = %f", F.crit, p.val))
cat(sprintf("\nWe %s reject the null hipothesis of equal variances.",
            ifelse(p.val < .05, "can", "can't")))
