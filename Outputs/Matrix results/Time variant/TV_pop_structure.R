F_0 <- F_list[[1]]
U_0 <- U_list[[1]]
A_0 <- U_0 + F_0
z_0 <- SD(A_0)
plot(z_0)
no_ages <- 19
no_stages <- 7

pop_struc <- list()
pop_struc[[1]] <- z_0
gen_suv_list1 <- list()
gen_fer_list1 <- list()
for(i in 1:length(F_list)){
  pop_struc[[(1+i)]] <- (U_list[[i]] + F_list[[i]])%*%pop_struc[[i]]
  U <- U_list[[i]]
  F <- F_list[[i]]
  v1 <- pop_struc[[(1+i)]]
  v0 <- pop_struc[[(i)]]
  P_11 <- matrix(0, nrow = no_ages*no_stages, ncol = no_ages*no_stages)
  P_22 <- matrix(0, nrow = no_ages*no_stages, ncol = no_ages*no_stages)
  for(row in 1 : (no_ages*no_stages)){
    for(col in 1 : (no_ages*no_stages)){
      P_11[row,col] <- v0[col]*U[row,col]/(v1[row])
      if(is.na(P_11[row,col]) == TRUE){P_11[row,col] = 0}
      P_22[row,col] <- v0[col]*F[row,col]/(v1[row])
      if(is.na(P_22[row,col]) == TRUE){P_22[row,col] = 0}
    }
  }
  gen_suv_list1[[i]] <- P_11
  gen_fer_list1[[i]] <- P_22
}

rowSums(gen_suv_list1[[2]])
rowSums(gen_fer_list1[[2]])

## check differences ###

(W_list[[2]]-gen_fer_list1[[2]])%>%max()
(V_list[[8]]-gen_suv_list1[[8]])%>%max()
