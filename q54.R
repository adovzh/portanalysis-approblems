source("q53.R")
source("efficientp.R")

weekly.stocks.returns <- na.omit(fapply(weekly.stocks, FUN=returns))
weekly.combined.returns <- cbind(weekly.stocks.returns, 
                                 weekly.sp.returns)

ws.means <- colMeans(weekly.stocks.returns) * 52
wc.means <- colMeans(weekly.combined.returns) * 52
ws.sigma <- cov(weekly.stocks.returns) * 52
wc.sigma <- cov(weekly.combined.returns) * 52

ws.weights <- findMVP(ws.sigma, ret=ws.means, target.return=.15)
wc.weights <- findMVP(wc.sigma, ret=wc.means, target.return=.15)

ws.rr <- mv.coords.gen(ws.sigma, ws.means)(ws.weights)
wc.rr <- mv.coords.gen(wc.sigma, wc.means)(wc.weights)

cat(sprintf("\nAmount of risk eliminated: %.2f%%\n", 
            100 * (ws.rr[1] - wc.rr[1])))
