
plan("multisession",workers=8)
## Load data times for 2019

cluster_migration_timeSeries_one_year <- function(list_of_yearly_migration_rates, number_of_clusters){

discrete_colours <- RColorBrewer::brewer.pal(4, "Set1")
############################################################# 2019 #########################################

LAD_mig_A <- list_of_yearly_migration_rates[[1]]
LAD_mig_B <- list_of_yearly_migration_rates[[2]]

##### Data comes in 2 parts, which are combined (row-wise)
LAD_mig_A <- as.data.frame(LAD_mig_A)
colnames(LAD_mig_A) <- c("orig_CODE" , "dest_CODE" , "Age" , "Sex" , "Count")
LAD_mig_A <- LAD_mig_A%>%filter(Sex == "F")%>%dplyr::select(-Sex)
LAD_mig_B <- as.data.frame(LAD_mig_B)
colnames(LAD_mig_B) <- c("orig_CODE" , "dest_CODE" , "Age" , "Sex" , "Count")
LAD_mig_B <- LAD_mig_B%>%filter(Sex == "F")%>%dplyr::select(-Sex)
LAD_mig_2019 <- rbind(LAD_mig_A,LAD_mig_B)
#LAD_mig_2019 <- LAD_mig_2019 %>% 
 # filter(!substr(orig_CODE, 1, 1) == "S" , !substr(dest_CODE, 1, 1) == "S" )
LAD_mig_2019 <- LAD_mig_2019[order(LAD_mig_2019$Age, decreasing = FALSE),]

LAD_mig_2019$Age%>%unique()

LAD_mig_age_class_2019 <- data.frame()
foreach(age = seq(0, 90, by = 5))%do%{
  df_temp <- LAD_mig_2019%>%filter(Age>= age & Age < age + 5)%>%
    group_by(orig_CODE, dest_CODE)%>%
    summarise(mean_count_over_age = sum(Count))
  #df_temp$Age <- as.numeric(age)
  df_temp$Age <- paste(age , age + 4, sep = "_")
  LAD_mig_age_class_2019 <- rbind(LAD_mig_age_class_2019, df_temp)
}

########################################## Rbind to big data #####################


LAD_mig_age_class_2019$year <- 2019

time_LADS <- LAD_mig_age_class_2019

## Cluster based on emigration from LADS
k_means_df_ALL_YEARS <- time_LADS%>%group_by(Age, orig_CODE)%>%
  summarise(mean_Dest = sum(mean_count_over_age))%>%ungroup()%>%
  pivot_wider(names_from=c("Age"), values_from="mean_Dest")%>%
  dplyr::select(-c(orig_CODE))

k_means_df_ALL_YEARS <- na.omit(k_means_df_ALL_YEARS)

res <- data.frame(ind=1:10)
for (i in 1:10) {
  set.seed(1)
  kmeansx <- kmeans(k_means_df_ALL_YEARS, centers=i, nstart=20)
  res$wss[i] <- kmeansx$tot.withinss
  if (i == 1) cen <- cbind(K=i,n=factor(1:i),kmeansx$centers)
  if (i > 1)  cen <- rbind(cen, cbind(K=i,n=factor(1:i),kmeansx$centers))
  if (i == 1) clus <- kmeansx$cluster
  if (i > 1)  clus <- cbind(clus, kmeansx$cluster)
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

cp <- cen %>% as.data.frame() %>% filter(K==number_of_clusters)%>%
  pivot_longer(-c("K","n"),values_to="cen",names_to="age")


plot_of_centers <- ggplot(cp, aes(x=age,y=cen,color=as.factor(n))) +
  geom_line(aes(group = factor(n))) + scale_color_manual(values = discrete_colours[1:number_of_clusters])

codee <- time_LADS%>%group_by(Age, orig_CODE)%>%
  summarise(mean_Dest = mean(mean_count_over_age))%>%
  pivot_wider(names_from=c("Age"), values_from="mean_Dest")
codee <- codee%>%na.omit()
codee <- codee%>%dplyr::select(orig_CODE)%>%unique()%>%as.data.frame()%>%unname()
#colnames(codee) <- "code"
k_means_df_ALL_YEARS$code <- as.factor(codee[,1])
k_means_df_ALL_YEARS$clus <- as.factor(clus[, number_of_clusters])
colnames(k_means_df_ALL_YEARS)
k_means_df_ALL_YEARS <- k_means_df_ALL_YEARS[, c(1,10,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21)]

mig_cluster_df <- k_means_df_ALL_YEARS%>%melt(id = c("code","clus"))%>%
  mutate(Age_class = variable,
         no_out_moves = value,
         cluster = clus,
         LAD_code = code)%>%dplyr::select(Age_class,no_out_moves,cluster,LAD_code)

mig_cluster_df$num_age <- gsub('.{2}$' , "" , mig_cluster_df$Age_class)
mig_cluster_df$num_age <- as.numeric(gsub('_' , "" , mig_cluster_df$num_age))
mig_cluster_df$Age_class <- fct_reorder(mig_cluster_df$Age_class, mig_cluster_df$num_age)



plot_of_clusters <- mig_cluster_df%>%ggplot(aes( x = Age_class, y = no_out_moves, color = cluster)) +
  geom_line(aes(group = factor(LAD_code)))+ scale_color_manual(values = discrete_colours[1:number_of_clusters]) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                     text = element_text(size = 15),
                     legend.position = "top") + ylab("no. emigrations from LAD") + xlab("age class")

return(list(mig_cluster_df,
            time_LADS,
            plot_of_clusters,
            plot_of_wss_diff,
            plot_of_centers,
            plot_of_elbows))
}
