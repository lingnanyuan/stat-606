library(sp)
library(sppmix)

find_Neigh <- function(d,h){
  N = length(d$x)
  d_matrix <- as.matrix(dist(cbind(d$x,d$y),diag = 1, upper = 1))
  d$id <- matrix(rep(0,N*N),N,N)
  d$id[d_matrix<h]=1
  return(d)
}
d<-find_Neigh(d,80000)

Raw_est <- function(d){
  n=length(d$x)
  d$est=matrix(rep(0,3*n),n,3)
  for( i in c(1:n)){
    t <- data.frame(x=d$x[which(d$id[i,]==1)],y=d$y[which(d$id[i,]==1)],z=d$z[which(d$id[i,]==1)])
    coordinates(t) = ~x+y
    vgm <- variogram(z~1,t)
    est <- fit.variogram(vgm, vgm("Mat"))
    d$est[i,] <- c(est$psill[2],est$range[2],est$kappa[2])
  }
  return(d)
}
d<-Raw_est(d)

pred<-function(s0,m0,par0,d){
  x = c(s0[1],d$x)
  y = c(s0[2],d$y)
  N = length(x)
  par_t = rbind(par0,d$est)
  d_matrix <- as.matrix(dist(cbind(x,y),diag = 1, upper = 1))
  Cov = matrix(rep(0,N*N),N,N)
  for (i in 1:N){
    for ( j in 1:N){
      Cov(i,j) = Matern(d_matrix[i,j],
                        range=(par_t[i,2]+par_t[j,2])/2,
                        smoothness = (par_t[i,3]+par_t[j,3])/2,
                        phi=(par_t[i,1]+par_t[j,1])/2)
    }
  }
  C = Cov[-1,-1]
  C0 = Cov[1,]
  return(m_0+C0%*%C%*%as.matrix(d$z-d$m))
}

Gamma0 <- function(x0,Nx0,theta0){
  t =data.frame(x=c(x0[1],Nx0$x),
                y=c(x0[2],Nx0$y))
  d_matrix = as.matrix(dist(cbind(t$x,t$y),diag = 1, upper = 1))
  g0 = theta0[1]*2-Matern(d_matrix,range=theta0[2],nu=theta0[3],phi=theta0[1])
  return(g0)
}
