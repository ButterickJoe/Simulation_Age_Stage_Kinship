

############ Create a matrix G which projects Focal with probability of survival = 1 ##############

get_G <- function(U, no_ages, no_stages){
  sig <- t(rep(1,no_ages*no_stages))%*%U
  diag <- diag(sig[1,])
  G <- U%*%ginv(diag)
  return(G)
}

############## Dynamic projection of Focal #######################

foc_dy <- function(U, F, no_ages, no_stages, foc_init_stage){
  
  U_tilde <- U
  F_tilde <- F
  na <- no_ages
  ns <- no_stages
  
  G_tilde <- get_G(U, na, ns)
  w <- SD(U_tilde + F_tilde)
  X  <- matrix(0, nrow = na*ns, ncol = na, byrow = TRUE)
  
  IC <- rep(0, na*ns)
  IC[foc_init_stage] <- 1
  X[,1] <- IC
  
  focal_age <- matrix(0, nrow = na, ncol = na, byrow = TRUE)
  focal_stage <- matrix(0, nrow = ns, ncol = na, byrow = TRUE)
  foreach(i = 1 : (na -1) )%do%{
    X[,i+1] <- G_tilde%*%X[,i] 
    focal_age[,i] <- marg_age_dist(na, ns, X[,i])
    focal_stage[,i] <- marg_stage_dist(na, ns, X[,i])
  }
  
  
  return(list(X,focal_age,focal_stage))
}




