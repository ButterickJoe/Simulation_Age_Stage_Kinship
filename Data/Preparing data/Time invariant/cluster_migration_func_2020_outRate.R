
plan("multisession",workers=8)
## Load data times from 2011 to 2019

cluster_migration_timeSeries_outRate2020 <- function(list_of_yearly_migration_rates, number_of_clusters){

col_scal <- RColorBrewer::brewer.pal(10, "Set1")
############################################################# 2019 #########################################

############################################################# 2020 #########################################
LAD_mig_2020A <- list_of_yearly_migration_rates[[1]]
LAD_mig_2020B <- list_of_yearly_migration_rates[[2]]
##### Data comes in 2 parts, which are combined (row-wise)
LAD_mig_2020A <- as.data.frame(LAD_mig_2020A)
colnames(LAD_mig_2020A) <- c("orig_CODE" , "dest_CODE" , "Age" , "Sex" , "Count")
LAD_mig_2020A <- LAD_mig_2020A%>%filter(Sex == "F")%>%dplyr::select(-Sex)
LAD_mig_2020B <- as.data.frame(LAD_mig_2020B)
colnames(LAD_mig_2020B) <- c("orig_CODE" , "dest_CODE" , "Age" , "Sex" , "Count")
LAD_mig_2020B <- LAD_mig_2020B%>%filter(Sex == "F")%>%dplyr::select(-Sex)
LAD_mig_2020 <- rbind(LAD_mig_2020A,LAD_mig_2020B)
LAD_mig_2020 <- LAD_mig_2020 %>% 
  filter(!substr(orig_CODE, 1, 1) == "S" , !substr(dest_CODE, 1, 1) == "S" )
LAD_mig_2020 <- LAD_mig_2020[order(LAD_mig_2020$Age, decreasing = FALSE),]
LAD_mig_2020$Age%>%unique()
LAD_mig_2020%>%head()
LAD_mig_age_class_2020 <- data.frame()
foreach(age = seq(0, 90, by = 5))%do%{
  df_temp <- LAD_mig_2020%>%filter(Age>= age & Age < age + 5)%>%
    group_by(orig_CODE, dest_CODE)%>%
    summarise(mean_count_over_age = sum(Count))%>%
    ungroup()
  #df_temp$Age <- as.numeric(age)
  df_temp$Age <- paste(age , age + 4, sep = "_")
  LAD_mig_age_class_2020 <- rbind(LAD_mig_age_class_2020, df_temp)
}

LAD_mig_age_class_2020%>%head()
LAD_mig_age_class_2020$year <- 2020
LADS_in_2020 <- LAD_mig_age_class_2020$orig_CODE%>%unique()


time_LADS <- LAD_mig_age_class_2020

k_means_df_ALL_YEARS <- time_LADS%>%group_by(year, Age, orig_CODE)%>%
  summarise(mean_Dest = sum(mean_count_over_age))%>%ungroup()%>%
  arrange(year, orig_CODE, Age)%>%ungroup()

k_means_df_ALL_YEARS$mean_Dest <- as.numeric(k_means_df_ALL_YEARS$mean_Dest)

check <- as.data.frame(k_means_df_ALL_YEARS%>%
                         pivot_wider(names_from=c("Age","year"), values_from="mean_Dest"))

bad_LADs <- check[!complete.cases(check), ][,1]
k_means_df_ALL_YEARS <- k_means_df_ALL_YEARS%>%subset(!orig_CODE %in% bad_LADs )
k_means_df_ALL_YEARS1 <- as.data.frame( k_means_df_ALL_YEARS%>%
                                          pivot_wider(names_from=c("Age","year"), values_from="mean_Dest")%>%
                                          dplyr::select(-c(orig_CODE)))
res <- data.frame(ind=1:10)
for (i in 1:10) {
  set.seed(1)
  kmeansx <- kmeans(na.omit(k_means_df_ALL_YEARS1), centers=i, nstart=20)
  res$wss[i] <- kmeansx$tot.withinss
  if (i == 1) cen <- cbind(K=i,n=factor(1:i),kmeansx$centers)
  if (i > 1)  cen <- rbind(cen, cbind(K=i,n=factor(1:i),kmeansx$centers))
  if (i == 1) clus <- kmeansx$cluster
  if (i > 1)  clus <- cbind(clus, kmeansx$cluster)
  k_means_df_ALL_YEARS[[paste0("clus",i)]] <- factor(rep(rep(kmeansx$cluster, each=19),
                                                         length(unique(k_means_df_ALL_YEARS$year))))
}
cen%>%nrow()
#Elbow plot
plot_of_elbows <- res %>% ggplot(aes(x=ind,y=wss/wss[1])) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks=seq(5,20,5),minor_breaks=1:20) +
  labs(x="K",y="Within-cluster sum of squares (scaled)") +
  theme_bw()
plot_of_wss_diff <- res %>% mutate(wss=wss/wss[1]) %>%
  mutate(First=c(NA,diff(wss)),
         Second=c(NA,diff(First))) %>%
  dplyr::select(-wss) %>%
  pivot_longer(-ind,values_to="Difference",names_to="Order") %>%
  ggplot(aes(x=ind,y=Difference,color=Order)) +
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_line(aes(group=Order)) +
  geom_point() +
  geom_vline(aes(xintercept=6)) +
  labs(x="K") +
  scale_x_continuous(breaks=1:40,minor_breaks=NULL,expand=c(0.02,0.02)) +
  coord_cartesian(ylim=c(-0.1,0.1)) +
  theme_bw() +
  theme(legend.position="bottom")
cp <- cen %>% as.data.frame() %>% filter(K==3)%>%
  pivot_longer(-c("K","n"),values_to = "cen" , names_to = "age")
plot_of_centers <- ggplot(cp, aes(x=age,y=cen,color=as.factor(n))) +
  geom_line(aes(group = factor(n))) + scale_color_manual(values = col_scal[1:number_of_clusters]) +
  theme(axis.text.x = element_text(angle = 90))

clit <- colnames(k_means_df_ALL_YEARS[,5:ncol(k_means_df_ALL_YEARS)])


ddd <- k_means_df_ALL_YEARS%>%
  dplyr::select(year , Age , orig_CODE , mean_Dest  )%>%
  cbind(k_means_df_ALL_YEARS[, paste0("clus", number_of_clusters)])

colnames(ddd) <- c(colnames(ddd[,1:(ncol(ddd)-1)]),"fucker")

k_means_df_ALL_YEARS_full <- ddd%>%mutate(dd = fucker )%>%
  group_by(Age, year, orig_CODE, dd )%>%
  summarise(mean_Dest = mean(mean_Dest))%>%ungroup()%>%
  mutate(Age_class = Age,
         no_out_moves = mean_Dest,
         cluster = dd,
         LAD_code = orig_CODE)%>%
  dplyr::select(Age_class,year,no_out_moves,cluster,LAD_code)

k_means_df_ALL_YEARS_full$num_age <- gsub('.{2}$' , "" , k_means_df_ALL_YEARS_full$Age_class)
k_means_df_ALL_YEARS_full$num_age <- as.numeric(gsub('_' , "" , k_means_df_ALL_YEARS_full$num_age))
k_means_df_ALL_YEARS_full$Age_class <- fct_reorder(k_means_df_ALL_YEARS_full$Age_class, k_means_df_ALL_YEARS_full$num_age)



plot_of_clusters_yearly <- k_means_df_ALL_YEARS_full%>%
  ggplot(aes(x = Age_class, y = no_out_moves, color = cluster  ) )+ 
  geom_line(aes(group = factor(LAD_code) )) + 
  facet_grid(~year) + scale_color_manual(labels = seq(1,number_of_clusters,1),
                                         values = col_scal[1:number_of_clusters]) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                     text = element_text(size = 15),
                     legend.position = "top") + ylab("no. emigrations from LAD") + xlab("age class")

plot_of_clusters_av <- k_means_df_ALL_YEARS_full%>%
  group_by(Age_class, LAD_code, cluster) %>%
  summarise(mean_Dest = mean(no_out_moves))%>%ungroup()%>%
  mutate(num_age = gsub('.{2}$' , "" , Age_class),
         num_age = as.numeric(gsub('_' , "" , num_age)),
         Age_class = gsub('_' , "-" , Age_class),
         Age_class = fct_reorder(Age_class, num_age))%>%
  ggplot(aes(x = Age_class, y = mean_Dest, color = cluster) ) + 
  geom_line(aes(group = factor(LAD_code) ))  +
  scale_color_manual(labels = seq(1,number_of_clusters,1),
                     values = col_scal[1:number_of_clusters]) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                     text = element_text(size = 14),
                     legend.position = "top") + ylab("Age specific migration counts out of LAD") + xlab("Age class")


mig_cluster_df <- k_means_df_ALL_YEARS_full%>%
  dplyr::select(year, Age_class, LAD_code , no_out_moves, cluster)%>%
  group_by(Age_class, LAD_code, cluster )%>%
  summarise(mean_Dest = mean(no_out_moves))%>%ungroup()%>%
  mutate(Age_class = Age_class,
         no_out_moves = mean_Dest,
         cluster = cluster,
         LAD_code = LAD_code)%>%dplyr::select(Age_class,no_out_moves,cluster,LAD_code)


mig_cluster_df$num_age <- gsub('.{2}$' , "" , mig_cluster_df$Age_class)
mig_cluster_df$num_age <- as.numeric(gsub('_' , "" , mig_cluster_df$num_age))
mig_cluster_df$Age_class <- fct_reorder(mig_cluster_df$Age_class, mig_cluster_df$num_age)

mig_cluster_df <- na.omit(mig_cluster_df)

return(list(mig_cluster_df,
            time_LADS,
            plot_of_clusters_yearly,
            plot_of_clusters_av,
            plot_of_wss_diff,
            plot_of_centers,
            plot_of_elbows))
}
