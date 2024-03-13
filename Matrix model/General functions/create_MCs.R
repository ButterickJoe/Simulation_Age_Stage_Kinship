
############### P_genealogical is the transition matrix : P_suv + P_fer its decomposition ###########

P_genealogical <- function(U, F, no_ages, no_stages){
  
  A <- U + F
  w <- SD(A)
  lambda <- lambda(A)
  
  P_genealogical <- matrix(0, nrow = no_ages*no_stages, ncol = no_ages*no_stages)
  
  foreach(i = 1 : (no_ages*no_stages))%do%{
    foreach(j = 1 : (no_ages*no_stages))%do%{
      
      P_genealogical[i,j] <- w[j]*A[i,j]/(w[i]*lambda)
      if(is.na(P_genealogical[i,j]) == TRUE){P_genealogical[i,j] = 0}
    }
  }
  return(P_genealogical)
}

P_gen_surv <- function(U, F, no_ages, no_stages){
  
  A <- U + F
  w <- SD(A)
  lambda <- lambda(A)
  
  P_suv <- matrix(0, nrow = no_ages*no_stages, ncol = no_ages*no_stages)
  
  foreach(i = 1 : (no_ages*no_stages))%do%{
    foreach(j = 1 : (no_ages*no_stages))%do%{
      
      P_suv[i,j] <- w[j]*U[i,j]/(w[i]*lambda)
      if(is.na(P_suv[i,j]) == TRUE){P_suv[i,j] = 0}
    }
  }
  return(P_suv)
}

P_gen_fer <- function(U, F, no_ages, no_stages){
  
  A <- U + F
  w <- SD(A)
  lambda <- lambda(A)
  
  P_fer <- matrix(0, nrow = no_ages*no_stages, ncol = no_ages*no_stages)
  
  foreach(i = 1 : (no_ages*no_stages))%do%{
    foreach(j = 1 : (no_ages*no_stages))%do%{
      
      P_fer[i,j] <- w[j]*F[i,j]/(w[i]*lambda)
      if(is.na(P_fer[i,j]) == TRUE){P_fer[i,j] = 0}
    }
  }
  return(P_fer)
}

