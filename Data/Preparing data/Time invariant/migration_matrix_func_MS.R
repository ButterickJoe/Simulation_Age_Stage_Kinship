
create_mig_mats_MS <- function(clustered_mig_data, ts_LAD_data, exposures_ts_df, interp_mig){

one_year_T_list <- interp_mig
mig_cluster_df <- clustered_mig_data
time_LADS <- ts_LAD_data
DF_counts <- exposures_ts_df

##### 
no_stage <- length(unique(mig_cluster_df$cluster))
no_age <- length(unique(mig_cluster_df$Age_class))
stages <- sort(unique(mig_cluster_df$cluster))

### Define the clusters which encompass the LADs
for(stage in stages){
  assign( paste0("k_clus", stage), filter(mig_cluster_df, cluster == stage))
  assign( paste0("k_LADS", stage), unique(get(paste0("k_clus", stage) )$LAD_code))
}
####################### Group migration data according to cluster: origin and destination.

bLAD <- intersect(time_LADS$orig_CODE,DF_counts$CODE)
time_LADS1 <- filter(time_LADS , orig_CODE %in% bLAD)
time_LADS1 <- time_LADS1%>%as.data.frame()
time_LADS1 <- time_LADS1%>%filter(Age != "90_94")
i1 <- intersect(DF_counts$Year , time_LADS1$year%>%unique())
DF_counts1 <- DF_counts%>%filter(Year %in% i1)

mig_and_counts_df <- data.frame()
DF_counts1$Age <- as.factor(DF_counts1$Age)
DF_counts1$counts <- as.numeric(DF_counts1$counts)
DF_counts1 <- DF_counts1%>%mutate(orig_CODE = CODE, year = Year)%>%dplyr::select(-CODE,-Year)


for(y in i1){
  AAA<-DF_counts1%>%filter(year == y)%>%group_by(Age,orig_CODE)
  AA<-time_LADS1%>%filter(year == y)%>%group_by(Age,orig_CODE)
  t_df <- full_join(AAA, AA, by = c("Age" , "orig_CODE", "year"))
  mig_and_counts_df <- rbind(mig_and_counts_df,t_df)
}
mig_and_counts_df$counts <- as.numeric(mig_and_counts_df$counts)
mig_and_counts_df <- mig_and_counts_df%>%mutate(moves = mean_count_over_age,
                                                exposures = counts,
                                                rates = moves/exposures)
mig_and_counts_df$orig_cluster <- "NA"
for(stage in stages){
  idx <- which(mig_and_counts_df$orig_CODE %in% get(paste0("k_LADS", stage) ))
  mig_and_counts_df$orig_cluster[idx] <- paste("K = ", stage, sep="")
}

mig_and_counts_df$dest_cluster <- "NA"
for(stage in stages){
  idx <- which(mig_and_counts_df$dest_CODE %in% get(paste0("k_LADS", stage) ))
  mig_and_counts_df$dest_cluster[idx] <- paste("K = ", stage, sep="")
}
mig_and_counts_df$dest_cluster

################################################################ Transition matrices ##########################################################

# Form transition matrices for each age class (Caswell calls these the U_i for i = 1,2,...,omega ages)
age_list <- mig_and_counts_df$Age%>%unique()
age_list2 <- as.numeric( gsub('_.*','', mig_and_counts_df$Age%>%unique()) )

mig_and_counts_df <- mig_and_counts_df%>%as.data.frame()
mig_and_counts_df <- na.omit(mig_and_counts_df)

age_spec_mat_list_1_old <- list()
age_spec_mat_list_5_old <- list()
for(age in age_list){
  
  index <- which(age_list == age)
  age2 <- as.numeric(age_list2[index])
  
  temp <- mig_and_counts_df%>%filter(Age == age)
  Transfer <- matrix(0, no_stage, no_stage)
  for(stage in stages){
    stage <- as.numeric(stage)
    diag_ent <- 1
    for(stage2 in stages[-stage]){
      stage2 <- as.numeric(stage2)
      assign(paste("t_", stage, "_", stage2, sep=""), 
             mean(temp%>%
                    filter(orig_cluster == paste("K = ", stage, sep="") , 
                           dest_cluster == paste("K = ", stage2, sep=""))%>%
                    group_by(orig_CODE)%>%
                    summarise(m = sum(mean_count_over_age)/mean(exposures))%>%
                    dplyr::select(m)%>%
                    sum())/max(1,nrow(temp%>%
                                  filter(orig_cluster == paste("K = ", stage, sep="") , 
                                         dest_cluster == paste("K = ", stage2, sep=""))%>%
                                  group_by(orig_CODE)%>%
                                  summarise(m = sum(mean_count_over_age)/mean(exposures))%>%
                                  dplyr::select(m))))
      
      diag_ent <- diag_ent - get(paste("t_" , stage, "_", stage2, sep = ""))
      Transfer[stage,stage2] <- get(paste("t_" , stage, "_", stage2, sep = ""))
      
    }
    
    Transfer[stage,stage] <- diag_ent
    
  }
  
  age_spec_mat_list_1_old[[ (1+age2/5) ]] <- matrix.power( t(Transfer) , 1)
  age_spec_mat_list_5_old[[ (1+age2/5) ]] <- t(one_year_T_list[[(5+age2)]])%*%
    t(one_year_T_list[[(4+age2)]])%*%t(one_year_T_list[[(3+age2)]])%*%
    t(one_year_T_list[[(2+age2)]])%*%t(one_year_T_list[[(1+age2)]])

}


return(list(age_spec_mat_list_1_old,age_spec_mat_list_5_old,age_list2))
}
