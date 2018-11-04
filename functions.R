library(sp)

find_Neigh <- function(d,h){
  N = length(d$x)
  d_matrix <- as.matrix(dist(cbind(d$x,d$y),diag = 1, upper = 1))
  d$id <- matrix(rep(0,N*N),N,N)
  d$id[d_matrix<h]=1
  return(d)
}
d<-find_Neigh(d,80000)
corrplot(d$id)

Raw_est <- function(d){
  n=length(d$x)
  for( i in c(1:n)){
    t <- data.frame(x=d$x[which(d$id[i,]==1)],y=d$y[which(d$id[i,]==1)],z=d$z[which(d$id[i,]==1)])
    coordinates(t) = ~x+y
    vgm <- variogram(z~1,t)
    est <- fit.variogram(vgm, vgm("Mat"))
    d$est <- rbind(d$est,c(est$psill[2],est$range[2],est$kappa[2]))
  }
  return(d)
}
d<-Raw_est(d_list)
