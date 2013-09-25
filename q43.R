library(timeSeries)
source("common.R")

monthly <- toTimeSeries(loadMonthly())
sp.returns <- na.omit(fapply(monthly[,"S.PCOMP.RI."], FUN=returns))
ind <- factor(rep(1:4, each=length(sp.returns)/4), labels="Column")
ds <- data.frame(series(sp.returns), ind)
print(m <- anova(lm(S.PCOMP.RI. ~ ind, data=ds)))

cat(sprintf("\nWe %s reject the null hypothesis that the groups have the same mean.\n",
            ifelse(m[1,5] < .05, "can", "can't")))
