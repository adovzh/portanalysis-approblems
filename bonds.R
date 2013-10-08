# bond holding period return
# m - maturity
# freq - coupon frequency
# w - portion of the coupon period before settlement
# prev, cur - previous and current ytm
bond.hpr.generic <- function (m, freq, w) {
  function(prev, cur) {
    log((1+cur/freq)^(-w)*((prev/freq)*(1+(1-(1+cur/freq)^(1-freq*m))/(cur/freq))+
                          (1+cur/freq)^(1-freq*m)))
  }
}

bond.hpr.monthly <- bond.hpr.generic(10, 2, 5/6)
bond.hpr.weekly <- bond.hpr.generic(10, 2, 25/26)

# bill holding period return
# m - maturity in days
# w - holding period in days
bill.hpr.generic <- function(m, w) {
  function(prev, cur) {
    log((1+prev*m/365)/(1+cur*(m-w)/365))
  }
}

bill.hpr.weekly <- bill.hpr.generic(90, 7)
