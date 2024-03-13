
create_mort_mats1 <- function(clustered_mig_data, deaths_and_exposures_data){
########################################################################## Mortality
full_df <- deaths_and_exposures_data
mig_cluster_df <- clustered_mig_data

no_age <- length(unique(mig_cluster_df$Age_class))
no_stage <- length(unique(mig_cluster_df$cluster))             

stages <- sort(unique(mig_cluster_df$cluster))%>%as.vector()

stages
# Cluster 1 age mortality matrix 
U_list <- list()
foreach(stage = stages)%do%{
  
  n_stage <- as.numeric(stage)
  #print(n_stage)
  tmp <- full_df%>%filter(cluster == n_stage)%>%group_by(Age)%>%
    summarise(dr = sum(deaths)/sum(counts),
              prob = 1-(5*dr)/(1+5*dr*0.5))%>%ungroup()%>%as.data.frame()
  
  mort_vec <- tmp$prob
  
  U_mat <- matrix(0, nrow = no_age, ncol = no_age)
  diag(U_mat[-1, -no_age]) <- mort_vec[1:(no_age-1)]
  U_mat[no_age,no_age] <- 0
  U_mat[is.na(U_mat)] <- 0
  
  
  U_list[[(1+length(U_list))]] <- U_mat
}

return(U_list)
}
