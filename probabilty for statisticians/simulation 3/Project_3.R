# Project 3
# Name: GELAI DEI
# I.D. Number: 


suppressWarnings(library(comprehenr))
# Q1: Replace "return(NA)" by your code
ex3q1 <- function(n, mu, sd, r){
  sig_hat <- to_vec(for (x in seq(length(mu))) for (y in seq(length(mu))) (r^abs(x-y))*(sig^2))
  sig_hat.root <- matrix(sig_hat, length(mu), length(mu))
  sig_hat_eigen<- eigen(sig_hat.root,symmetric=TRUE)
  U <- sig_hat_eigen$vectors
  D <- diag(sqrt(sig_hat_eigen$values))
  sig_hat.root.eigen <- U %*% D %*% t(U)
  X <- matrix(rnorm(n*length(mu)),length(mu),n)
  Y <- sig_hat.root.eigen %*% X
  Y <- sweep(Y,1,mu,"+")
  return(t(Y))
}


# Q2: Replace "return(NA)" by your code
ex3q2 <- function(data, statistic, R){
  t <- rep(NA, R)
  t0 <- statistic(data)
  for (bootstrap in c(1:R)){
    real_mu <- colMeans(data)
    real_var <- var(data)
    Y_sim <- mvrnorm(10^2,real_mu,real_var)
    t[bootstrap] <-  statistic(Y_sim)
  }
  return(list("t0"=t0, "t"=t))
}