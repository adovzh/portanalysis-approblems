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
