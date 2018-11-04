sigmatilde <- function(x0, location, delta, sigmahat){
  m <- dim(location)[1]
  W_0 <- rep(0, m)
  K <- rep(0,m)
  for (i in 1:m){
    K[i] <- Kernelf(x0-location[i,])
  }
  W_0 <- K/sum(K)
  sigmah <- apply(location, 1, sigmahat)
  sigmat <- sum(W_0*sigmah)
  return(sigmat)
}


Kernelf <- function(x£¬ sigma){
  return(1/(2*pi*sigma^2)*exp(-(sum(x^2)/(2*sigma^2))))
}

psitilde <- function(x0, location, delta, psihat)























