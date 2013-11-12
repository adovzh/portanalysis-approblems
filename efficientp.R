# find global minimum variance portfolio
findGMVP <- function(sigma) {
  A <- cbind(2 * sigma, 1)
  A <- rbind(A, c(rep(1, ncol(sigma)), 0))
  b <- c(rep(0, nrow(sigma)), 1)
  solve(A, b)[1:ncol(sigma)]
}

# find minimum variance portfolio with target expected return
findMVP <- function(sigma, ret, target.return) {
  A <- cbind(2 * sigma, ret, 1)
  A <- rbind(A, c(ret, 0, 0), c(rep(1, nrow(A)), 0, 0))
  b <- c(rep(0, ncol(sigma)), target.return, 1)
  w <- solve(A, b)[1:ncol(sigma)]  
}


# generate function to produce risk/return pair 
# for a vector of weights 
# given covariance matrix and vector of expected returns
mv.coords.gen <- function(a_sigma, a_means) {
  function(w) {
    risk <- sqrt(t(w) %*% a_sigma %*% w)
    ret <- a_means %*% w
    c(risk, ret)    
  }
}

# convex combination of two efficient portfolios
z.gen <- function(w1, w2) {
  function(alpha) {
    cbind(w1, w2) %*% c(alpha, 1 - alpha)
  }
}
