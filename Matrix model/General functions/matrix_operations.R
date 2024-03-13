# A file of general functions

# The growth rate -- the spectral radius of PM

lambda <- function(PM) {
  lead_eig <- (abs(eigen(PM, only.values = TRUE)$values))
  lead_eig <- lead_eig[which.max(lead_eig)]
  return(lead_eig)}


# stable age/stage distribution
# as the (right) eigenvector associated with lambda
SD <- function(PM) {
  spectral_stuff <- eigen(PM)
  spectral_stuff <- Re(spectral_stuff$vectors[,which.max(abs(spectral_stuff$values))])
  # normalise...
  vec_lambda <- spectral_stuff/sum(spectral_stuff)
  return(vec_lambda)}

# reproductive values as the (left) eigenvector -- lambda
RD <- function(PM) {
  spectral_stuff <- eigen(t(PM))
  spectral_stuff <- Re(spectral_stuff$vectors[,which.max(abs(spectral_stuff$values))])
  # normalise...
  vec_lambda <- spectral_stuff/sum(spectral_stuff)
  return(vec_lambda)}


# Age by stage structured models

# The marginal stage distribution (i.e., summing over all ages)
# ags : number of ages considered, number stages, and full dist with stage 
# embedded within age
marg_stage_dist <- function(no_ages, no_stages, full_dist){
  return(kronecker(t(rep(1, no_ages)) , diag(no_stages))%*%full_dist)}

# The marginal age dist (i.e., summing over all stages)
marg_age_dist <- function(no_ages, no_stages, full_dist){
  return(kronecker(diag(no_ages) , t(rep(1, no_stages)) ) %*%full_dist)
}

## Matirx operations -- defining the vec permutation martix 

# Given A of dimension (n,m) the permuation K_nm : K_nm vec A = vec A'
# Here E_ij is a matrix of appropriate dimension (n,m) with 0's everywhere but at (i,j)

#

e_vector <- function(i, n){
  e <- rep(0, n)
  e[i] <- 1
  return(e)
}

E_matrix <- function(i,j,n,m){
  E <- matrix(0, nrow = n, ncol = m, byrow = TRUE)
  E[i,j] <- 1
  return(E)
  
}

K_perm_mat <- function(n,m){
  perm = matrix(0, nrow = n*m, ncol = n*m, byrow = TRUE)
  for(i in 1:n){
    for(j in 1:m){
      perm = perm + kronecker( E_matrix(i,j,n,m) ,t( E_matrix(i,j,n,m) ) )
    }
  }
  return(perm)
}

#### Matrix operations -- combining age and stage

## Let n be a w by s matirx where entry (i,j) = individuals of age w in stage j

# block diagonal stage matrix with U1 U2 U3 ... US diagonals
# Takes argument of a list.
block_diag_function <- function(mat_list){
  s = length(mat_list)
  u1 = mat_list[[1]]
  dims <- dim(u1)
  r = dims[1]
  diagmat <- matrix(0, nrow = r*s, ncol = r*s, byrow = TRUE)
  for(i in 1:s){
    diagmat = diagmat + kronecker(E_matrix(i,i,s,s), mat_list[[i]])
  }
  return(diagmat)
}


create_proj_mat <- function(no_ages, no_stages, m1, m2){
  return(t(K_perm_mat(no_stages,no_ages))%*%m1%*%K_perm_mat(no_stages,no_ages)%*%m2)
}


