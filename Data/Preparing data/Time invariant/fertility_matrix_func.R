
create_fert_mats <- function(clustered_mig_data, new_fert_df){
  
  no_stage <- length(unique(clustered_mig_data$cluster))
  no_age <- length(unique(clustered_mig_data$Age_class))
  stages <- sort(unique(clustered_mig_data$cluster))
  
  eng_fert <- 
    new_fert_df%>%dplyr::select(c(cluster1 , ASFR , Age))%>%
    group_by(Age, cluster1)%>%
    summarise(mean_ASFR = mean(ASFR))%>%ungroup()%>%
    mutate(Clus_num = as.factor(cluster1))
  
  mig_cluster_df <- clustered_mig_data
  
  eng_fert_k4 <- as.data.frame(eng_fert)
  
  eng_fert_K4 <- na.omit(eng_fert_k4)
  eng_fert_K4$Age_number <- gsub('.{3}$',"", eng_fert_K4$Age)
  eng_fert_K4$Age_number <- as.numeric(eng_fert_K4$Age_number)
  
  age_clus_fert <- eng_fert_K4%>%group_by(Age, Clus_num)%>%summarise(mean(mean_ASFR))
  
  #eng_fert_K4<-eng_fert_K4[-which(eng_fert_K4$cluster1=="NA"),]
  
  eng_fert_K4
  F_list1 <- list()
  for(i in 1 : length(unique(eng_fert_K4$Age))){
    age = (unique(eng_fert_K4$Age))[i]
    F_k_1 <- filter(eng_fert_K4, Age == age)%>%dplyr::select(mean_ASFR)%>%as.matrix()%>%
      unname()
    f <- matrix(0, nrow = no_stage, ncol = no_stage, byrow = T)
    diag(f) <- F_k_1
    F_list1[[i]] <- f
  }
  
  F_padd <- matrix(0, no_stage, no_stage)
  F_padd
  
  mig_cluster_df$Age_number <- gsub('.{2}$',"", mig_cluster_df$Age_class)
  mig_cluster_df$Age_number <- gsub('_' , "", mig_cluster_df$Age_number)
  mig_cluster_df$Age_number
  mig_cluster_df$Age_number <- as.numeric(mig_cluster_df$Age_number)
  mig_cluster_df$Age_number
  
  intt <- intersect( unique(eng_fert_K4$Age_number), unique(mig_cluster_df$Age_number))
  lpp <- rep(0, length(which(unique(mig_cluster_df$Age_number) < min(intt))) )
  upp <- rep(0, length(which(unique(mig_cluster_df$Age_number) > max(intt))) )
  
  lpp
  upp
  ## # F_list gives an age length list of 2by2 matrices : productions of stage i from stage j, (i,j = {1,2})
  F_list <- c( rep(list(F_padd), length(lpp)) ,  F_list1, rep(list(F_padd), length(upp)))
  F_list[[18]] <- matrix(0,no_stage,no_stage)
  F_list[[19]] <- matrix(0,no_stage,no_stage)
  
  return(F_list)
  
}
