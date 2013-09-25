library(timeSeries)
source("common.R")

monthly <- toTimeSeries(loadMonthly())
sp.returns <- na.omit(fapply(monthly[,"S.PCOMP.RI."], FUN=returns))
ind <- factor(rep(1:4, each=length(sp.returns)/4), labels="Column")
ds <- data.frame(series(sp.returns), ind)
print(m <- anova(lm(S.PCOMP.RI. ~ ind, data=ds)))

pval <- m[1,5]
cat(sprintf("\nWe %s reject the null hypothesis that the groups have the same mean.\n",
            ifelse(pval < .05, "can", "can't")))
group.means <- aggregate(series(sp.returns), by=list(ind), FUN=mean)
gm <- group.means[,2] * 12
names(gm) <- group.means[,1]
barplot(gm, col="#818AFC", space=.8)
title("Subperiod average annual returns")
