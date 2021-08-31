# Project 2
# Name: GELAI DEI
# I.D. Number: 

# Q1: Replace "return(NA)" by your code
ex2q1 <- function(n){
  XY <- matrix(NA,n,2)
  colnames(XY) <- c("X","Y")
  for(i in seq(n)){
    kabala <- FALSE
    while (!kabala) {
      attempt <- runif(2,0,1)
      U <- runif(1,0,1)
      kabala <- (U <= sqrt((1-sum(attempt^2))*(sum(attempt^2) < 1)))
    }
  XY[i,] <- attempt
  }
  return(data.frame(XY))
}


# Q2: Replace "return(NA)" by your code
ex2q2 <- function(alpha, n.copy){
  x  <- sapply(1:n.copy, function(i){
    dens_it <- runif(length(alpha)) 
    integr <- mean(sin(sum(fun*alpha)))})
  error <- sd(x)/sqrt(length(alpha))
  return(list("value" = mean(x), "error" = error))
}