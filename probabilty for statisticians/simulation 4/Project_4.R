# Project 4
# Name: GELAI DEI 
# I.D. Number: 

# Q1: Replace "return(NA)" by your code
ex4q1 <- function(n_sim, n_conv, a, b){
  X_sim <- Y_sim <- rep(NA, n_sim)
  f1 <- rnorm(1, mean = b[1]/a[1], sd = sqrt(1/a[1]))
  for (i in seq(n_sim)){
    X_sim[i] <- f1
    Y_sim[i] <- rnorm(1, mean = b[2]/(f1^2+a[2]), sd = 1/sqrt(f1^2+a[2]))
    f1 <- rnorm(1, mean = b[1]/(Y_sim[i]^2+a[1]), sd = 1/sqrt(Y_sim[i]^2+a[1]))
  }
    res <- cbind(X_sim,Y_sim)
  return(res)
  }


# Q2: Replace "return(NA)" by your code
ex4q2 <- function(data, mu, tau, r, lam, n_sim, n_conv){
  element <- 0
  schum <- sum(data^2)
  R_Gamma <- rgamma(1,r+length(data)/2,lam+s*0.5)
  for(i in seq(n_conv)){
    stm <-sum(data)
    ltm <-length(data)
    element <- rnorm(1,mean=(mu/tau^2+stm*R_Gamma)/(1/tau^2+ltm*R_Gamma),sd=sqrt(1/(1/tau^2+ltm*R_Gamma)))
    R_Gamma <- rgamma(1,r+ltm/2,lam+0.5*sum((data-element)^2))
  }
  s_element <- rep(NA,n_sim)
  srgam <- rep(NA,n_sim)
  element <- rnorm(1,mean=(mu/tau^2+stm*R_Gamma)/(1/tau^2+ltm*R_Gamma),sd=sqrt(1/(1/tau^2+ltm*R_Gamma)))
  for(i in seq(n_sim)){
    s_element[i] <- element
    srgam[i] <- rgamma(1,r+ltm/2,lam+0.5*sum((data-element)^2))
    element <- rnorm(1,mean=(mu/tau^2+srgam[i]*stm)/(1/tau^2+srgam[i]*ltm),sd=sqrt(1/(1/tau^2+srgam[i]*ltm)))
  }
  df <- data.frame(element=s_element,rgam=srgam)
  return(as.matrix(df))
}