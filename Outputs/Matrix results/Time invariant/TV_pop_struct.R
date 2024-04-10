
tv_pop_structure <- read_rds(here::here("Data", "Time invariant", "TV_pop_structure_FULL_IC3.Rds"))


tv_pop_structure <- tv_pop_structure%>%dplyr::select(stable,starts_with("y"))

no_ages <- 19
no_stages <- 7

tv_pop_structure%>%head()
tv_pop_structure[,6]

pop_struc <- list()
pop_struc[[1]] <- tv_pop_structure[,1]
gen_suv_list1 <- list()
gen_fer_list1 <- list()
for(i in 1:8){
  
  pop_struc[[(1+i)]] <- tv_pop_structure[,(1+i)]
  
  
  v1 <- pop_struc[[(1+i)]]
  v0 <- pop_struc[[(i)]]
  
  P_11 <- matrix(0, nrow = no_ages*no_stages, ncol = no_ages*no_stages)
  P_22 <- matrix(0, nrow = no_ages*no_stages, ncol = no_ages*no_stages)
  
  for(row in 1 : (no_ages*no_stages)){
    for(col in 1 : (no_ages*no_stages)){
      
      P_11[row,col] <- v0[col]*U_proj[row,col]/(v1[row])
      if(is.na(P_11[row,col]) == TRUE | is.infinite(P_11[row,col]) == TRUE){P_11[row,col] <- 0}
      
      
      P_22[row,col] <- v0[col]*F_proj[row,col]/(v1[row])
      if(is.na(P_22[row,col]) == TRUE | is.infinite(P_22[row,col]) == TRUE){P_22[row,col] <- 0}
    }
    
  }
  
  gen_suv_list1[[i]] <- P_11
  gen_fer_list1[[i]] <- P_22
  
}


which(is.na( gen_suv_list1[1:7]%>%unlist() ))
which(is.na(gen_fer_list1[1:7]%>%unlist()))

U_proj
which(gen_suv_list1[[7]]>0)
gen_fer_list1[[7]]
rowSums(gen_fer_list1[[7]])
rowSums(gen_suv_list1[[7]])


gen_suv_list2 <- list()
gen_fer_list2 <- list()
for(j in 1:8){
  
  stoch_mat_check1 <- gen_fer_list1[[j]] 
  stoch_mat_check2 <- gen_suv_list1[[j]]
  
  for(i in 1:nrow(stoch_mat_check1)){
    
    stoch_mat_check1[i,] <- stoch_mat_check1[i,]/sum(stoch_mat_check1[i,]+stoch_mat_check2[i,])
    stoch_mat_check2[i,] <- stoch_mat_check2[i,]/sum(stoch_mat_check1[i,]+stoch_mat_check2[i,])
    
  }  
  
  gen_fer_list2[[j]] <- stoch_mat_check1
  gen_suv_list2[[j]] <- stoch_mat_check2
  
}

gen_fer_list2[[6]]
gen_fer_list2[[7]]%>%rowSums()
gen_suv_list2[[7]]%>%rowSums()
gen_suv_list1
gen_fer_list1[[10]]%>%max()
