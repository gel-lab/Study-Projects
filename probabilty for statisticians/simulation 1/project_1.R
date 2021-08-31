# Project 1
# Name: GELAI DEI
# I.D. Number: 



# Q1: Replace "return(NA)" by your code
ex1q1 <- function(n, prob){
  q <- 1-prob
  U <- runif(n)
  X = ceiling(log(U)/log(q))
  return(X)
}



# Q2: Replace "return(NA)" by your code
ex1q2 <- function(n, k, th){
  hofhci_X <- (1-(1+th)^(-k))
  dens_hofchit <- ((1-runif(n)*hofhci_X)^(-1/k)-1)
  return(dens_hofchit)
}
