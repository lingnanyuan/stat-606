eta_x0 <- function(Gamma_x0_hat){
  inv_Gamma_x0_hat <- solve(Gamma_x0_hat)
  dim_Gamma_x0_hat <- dim(Gamma_x0_hat)
  eta_x0 <- inv_Gamma_x0_hat %*% rep(1,dim_Gamma_x0_hat[2])/as.numeric(t(rep(1,dim_Gamma_x0_hat[1])) %*% inv_Gamma_x0_hat %*% rep(1,dim_Gamma_x0_hat[2]))
  return(eta_x0)
}