
######## joint age*stage distribution 

kin_dists_full <- function(kin_matrix, no_ages, age_increment, no_stages){
  
  no_ages <- no_ages
  no_stages <- no_stages
  full_dist <- kin_matrix
  
  df_list <- list()
  foreach(i = 1:no_ages)%do%{
    jj <- (i-1)*age_increment  
    foc_age <- paste0("focal age", jj)  
    df <- data.frame(full_dist[,i])
    colnames(df) <- "full_dist"
    df$kin_age <- rep(0, no_ages*no_stages)
    df$kin_stage <- rep(0, no_ages*no_stages)
    for(kk in 0 : (no_ages*no_stages - 1) ){
      df$kin_age[kk+1] <- age_increment*(kk%/%no_stages)
      df$kin_stage[kk+1] <- kk%%no_stages + 1
    }
    df$kin_stage <- as.factor(df$kin_stage)
    df1 <- data.frame(matrix(nrow = no_ages*no_stages, ncol = no_stages))
    foreach(i = 1 : no_stages)%do%{
      df1[ , paste0("kin_age_stage", i)] <- rep(1, no_ages*no_stages) 
    }
    
    df1 <- df1 %>% select_if(~ !any(is.na(.)))
    df <- cbind(df,df1)
    
    foreach(kk = 0 : (no_ages*no_stages - 1) )%do%{
      k = kk%/%no_stages + 1 
      foreach(i = 1 : no_stages)%do%{
        j <- which( colnames(df) == paste0("kin_age_stage", i) )
        df[kk+1 , j] <- ifelse(df$kin_stage[kk+1] == i & df$kin_age[kk+1] == k ,df$full_dist[kk+1] , 0)
      }
    }
    
    foreach(kk = seq(1, no_ages*no_stages, by = no_stages))%do%{
      
      j_beg <- which( colnames(df) == paste0("kin_age_stage", 1) )
      j_end <- which( colnames(df) == paste0("kin_age_stage", no_stages ) )
      
      sum <- sum(df[kk:(kk+no_stages-1), j_beg:j_end])
      
      foreach(ll = 1 : no_stages)%do%{
        
        df[(kk + ll -1) , (j_beg + ll -1) ] <- df[ (kk + ll -1) , (j_beg + ll -1) ] /sum
        
      } 
      
    }
    
    df$group <- rep( foc_age , nrow(df) )
    df$age_foc <- as.numeric(gsub("focal age", "", df$group))
    df_list[[length(df_list)+1]] <- df
    
  }
  
  df_list <- do.call("rbind", df_list)
  return(df_list)
}




kin_dists_full1 <- function(kin_matrix, no_ages, age_increment, no_stages, year_start){
  
  no_ages <- no_ages
  no_stages <- no_stages
  full_dist <- kin_matrix
  
  df_list <- list()
  foreach(i = 1:no_ages)%do%{
    jj <- (i-1)*age_increment  
    foc_age <- paste0("focal age", jj)  
    df <- data.frame(full_dist[,i])
    colnames(df) <- "full_dist"
    df$kin_age <- rep(0, no_ages*no_stages)
    df$kin_stage <- rep(0, no_ages*no_stages)
    for(kk in 0 : (no_ages*no_stages - 1) ){
      df$kin_age[kk+1] <- age_increment*(kk%/%no_stages)
      df$kin_stage[kk+1] <- kk%%no_stages + 1
    }
    df$kin_stage <- as.factor(df$kin_stage)
    df1 <- data.frame(matrix(nrow = no_ages*no_stages, ncol = no_stages))
    foreach(i = 1 : no_stages)%do%{
      df1[ , paste0("kin_age_stage", i)] <- rep(1, no_ages*no_stages) 
    }
    
    df1 <- df1 %>% select_if(~ !any(is.na(.)))
    df <- cbind(df,df1)
    
    foreach(kk = 0 : (no_ages*no_stages - 1) )%do%{
      k = kk%/%no_stages + 1 
      foreach(i = 1 : no_stages)%do%{
        j <- which( colnames(df) == paste0("kin_age_stage", i) )
        df[kk+1 , j] <- ifelse(df$kin_stage[kk+1] == i & df$kin_age[kk+1] == k ,df$full_dist[kk+1] , 0)
      }
    }
    
    foreach(kk = seq(1, no_ages*no_stages, by = no_stages))%do%{
      
      j_beg <- which( colnames(df) == paste0("kin_age_stage", 1) )
      j_end <- which( colnames(df) == paste0("kin_age_stage", no_stages ) )
      
      sum <- sum(df[kk:(kk+no_stages-1), j_beg:j_end])
      
      foreach(ll = 1 : no_stages)%do%{
        
        df[(kk + ll -1) , (j_beg + ll -1) ] <- df[ (kk + ll -1) , (j_beg + ll -1) ] /sum
        
      } 
      
    }
    
    df$group <- rep( foc_age , nrow(df) )
    df$age_foc <- as.numeric(gsub("focal age", "", df$group))
    df$year <- rep( year_start , nrow(df) ) + df$age_foc
    df_list[[length(df_list)+1]] <- df
    
  }
  
  df_list <- do.call("rbind", df_list)
  return(df_list)
}



############# Marginal stage distribution ########################

kin_dists_by_stage <- function(kin_stage, no_ages, age_increment, no_stages){
  
  no_ages <- no_ages
  no_stages <- no_stages
  marg_dist <- kin_stage
  
  df <- data.frame(marg_dist)
  col_names <- rep(0, no_ages)
  for(i in 1 : no_ages){
    col_names[i] <- paste0("focal_age", (i-1)*age_increment)}
  
  colnames(df) <- col_names
  df$kin_stage <- seq(1 , no_stages, by = 1)
  df$kin_stage <- as.factor(df$kin_stage)
  df <- melt(df, id = "kin_stage")
  
  df$focal_age <- rep(0, no_ages*no_stages)
  for(kk in 0 : (no_ages*no_stages - 1) ){
    df$focal_age[kk+1] <- age_increment*kk%/%no_stages 
    
  }
  
  return(df)
}

############## Marginal age distribution #######################

kin_dists_by_age <- function(kin_stage, no_ages, no_stages){
  
  no_ages <- no_ages
  no_stages <- no_stages
  marg_dist <- kin_stage
  
  df <- data.frame(marg_dist)
  col_names <- rep(0, no_ages)
  for(i in 1 : no_ages){col_names[i] <- paste0("focal_age", i)}
  colnames(df) <- col_names
  df$kin_age <- seq(1 , no_ages, by = 1)
  df$kin_age <- as.factor(df$kin_age)
  df <- melt(df, id = "kin_age")
  
  df$focal_age <- rep(0, no_ages*no_ages)
  for(kk in 0 : (no_ages*no_ages - 1) ){
    df$focal_age[kk+1] <- kk%/%no_ages + 1
    
  }
  
  return(df)
}


