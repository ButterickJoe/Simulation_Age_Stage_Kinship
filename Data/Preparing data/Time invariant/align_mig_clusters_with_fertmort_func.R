

take_cluster_mig_align_fert <- function(fertility_data, clustered_mig_data, deaths_and_exposures_data){
  
  discrete_colours <- RColorBrewer::brewer.pal(10, "Set1")
  
  mig_cluster_df <- clustered_mig_data
  fert_clus_df <- fertility_data
  full_df <- deaths_and_exposures_data

  no_ages <- length(unique(mig_cluster_df$Age_class))
  no_stages <- length(unique(mig_cluster_df$cluster))
  
  stages <- sort(unique(mig_cluster_df$cluster))
  mig_LAD_CODE <- mig_cluster_df$LAD_code%>%unique()
  
  cluster_list <- list()
  for(i in stages){
    tmp <- mig_cluster_df%>%filter(cluster == i)%>%
      dplyr::select(LAD_code)%>%unique()%>%as.data.frame()%>%unname()
    tmp <- tmp[,1]
    
    assign(paste0('clus_',i), tmp)
    
    cluster_list[[(1+length(cluster_list))]] <- tmp
  }
  
  
  ############# My deaths LAD age, data
  
  full_df$cluster <- "NA"
  foreach(i = stages)%do%{
    i <- as.numeric(i)
    
    lads <- which(full_df$CODE %in% cluster_list[[i]])
    
    full_df$cluster[lads] <- i
    
  }
  full_df <- full_df[-which(full_df$cluster=="NA"),]
  
  log_plot <- full_df%>%mutate(death_rate = deaths/counts)%>%group_by(CODE, Age)%>%
    summarise(mean_dr = mean(death_rate),
              log_mean_dr = log(mean_dr) ,
              cluster = cluster)%>%
    mutate(num_age = gsub('.{2}$' , "" , Age),
           num_age = as.numeric(gsub('_' , "" , num_age)),
           Age = gsub('_' , "-" , Age),
           Age = fct_reorder(Age, num_age))%>%
    filter(log_mean_dr>-100)%>%
    na.omit()%>%
    ggplot(aes(x = Age, y = log(mean_dr) , color = cluster))   +
    geom_line(aes(group = factor(CODE))) + 
    theme(legend.position = "top") + theme_bw()  + 
    scale_color_manual(labels = seq(1,no_stages,1),
                       values = discrete_colours[1 : no_stages])+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                                                text = element_text(size = 14)) + 
    ggeasy::easy_center_title() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                                        text = element_text(size = 14),
                                        legend.position = "top") + ylab("Log death rate") + xlab("Age class")
  
  suv_plot <- full_df%>%mutate(death_rate = deaths/counts)%>%group_by(CODE, Age)%>%
    summarise(mean_dr = mean(death_rate),
              prob = 1 - (5*mean_dr)/(1+5*mean_dr*0.5) ,
              cluster = cluster)%>%
    na.omit()%>%
    ggplot(aes(x = Age, y = prob , color = cluster))   +
    geom_line(aes(group = factor(CODE))) + 
    theme(legend.position = "top")  + 
    scale_color_manual(labels = seq(1, no_stages, 1),
                       values = discrete_colours[1 : no_stages]) +
    ylab("suv prob") + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                                          text = element_text(size = 15)) + 
    ggeasy::easy_center_title() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                                        text = element_text(size = 15),
                                        legend.position = "top") + ylab("suv") + xlab("age class")
  
  
  ### added fertility##
  
  fert_clus_df$cluster1 <- "NA"
  foreach(i = stages)%do%{
    i <- as.numeric(i)
    
    lads <- which(fert_clus_df$Code %in% cluster_list[[i]])
    
    fert_clus_df$cluster1[lads] <- i
    
  }
  fert_clus_df <- fert_clus_df[-which(fert_clus_df$cluster1=="NA"),]
  
  fert_plot <- fert_clus_df%>%group_by(Code, Age)%>%
    summarise(ASFR = mean(ASFR),
              cluster1 = cluster1,
              Age = Age)%>%
  mutate(num_age = gsub('.{2}$' , "" , Age),
         num_age = as.numeric(gsub('_' , "" , num_age)),
         Age = gsub('_' , "-" , Age),
         Age = fct_reorder(Age, num_age),
         cluster = cluster1)%>%
    ggplot(aes(x = Age, y = ASFR, color = cluster)) + 
    geom_line(aes(group = factor(Code)))  +
    scale_color_manual(labels = seq(1,no_stages,1),
                       values = discrete_colours[1 : no_stages]) +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                       text = element_text(size = 14)) + 
    ggeasy::easy_center_title() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                                        text = element_text(size = 14),
                                        legend.position = "top") + ylab("ASFR") + xlab("Age class")
  
return(list(fert_clus_df, full_df, log_plot, suv_plot, fert_plot))

}

