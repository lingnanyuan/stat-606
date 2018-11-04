library(sp)
d = list(x=c(1,2,3),y=c(1,2,3),z=c(1,2,3))

find_Neigh <- function(d,h){
  N = length(d$x)
  d_matrix <- dist(cbind(d$x,d$y),diag = 1, upper = 1)
  d$id <- matrix(rep(0,N*N),N,N)
  d$id[d_matrix<h]=1
  return(d)
}

Raw_est <- function(d){
  n=length(d$x)
  for( i in c(1:n)){
    t <- data.frame(x=d$x[d$id[i,]],y=d$y[d$id[i,]],z=x=d$z[d$id[i,]])
    coordinates(t) = ~x+y
    vgm <- variogram(z~1,t)
    est <- fit.variogram(vgm, vgm("Mat"))
    d$est <- rbind(d$est,c(est$psill,est$range,est$kappa))
  }
  return(d)
}