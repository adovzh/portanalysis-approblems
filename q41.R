library(timeSeries)
source("common.R")
source("bonds.R")

monthly <- toTimeSeries(loadMonthly("monthly2.csv"))
usb <- monthly[,"USBD10Y"]
bond.returns <- timeSeries(data=bond.hpr.monthly(usb[-length(usb)], usb[-1]),
                           charvec=rownames(usb)[-1])
names(bond.returns) <- names(usb)

sp.returns <- na.omit(fapply(monthly[,"S.PCOMP.RI."], FUN=returns))

# correlation obtained using function
cor.func <- cor(sp.returns, bond.returns)

# correlation obtained using formula
cor.formula <- cov(sp.returns, bond.returns) / sd(sp.returns) / sd(bond.returns)

cat(sprintf("1) Correlations using function and formula are %sequal.\n",
            ifelse(abs(cor.func - cor.formula) < 1e-9, "", "not ")))
