## data from simulation
sim_sis_df <- read_rds(here::here("Outputs", "Comparisons", "Time invariant", "data frames", "YS_IC3.Rds"))


sim_sis_df<-sim_sis_df%>%transmute(Age_foc = (year), cum_kin = expected_val, 
                                   stage = stage, kin = kin)
sim_sis_df$Age_foc
sim_sis_df$method <- "stochastic simulation"


df_my_variant <- read_rds(here::here("Outputs", "Comparisons", "Time invariant", "data frames", "YS_matrix_IC3.Rds"))


Y_SIS <- rbind(sim_sis_df,df_my_variant)

agebreaks <- seq(-1,19,1)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85-90", "90-94","95+")
Y_SIS$Age_class <- cut(Y_SIS$Age_foc, agebreaks, labels = agelabels)

Y_SIS%>%filter(method=="gen mat")

clus_lab <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")

col_scal <- RColorBrewer::brewer.pal(10, "Set1")

######## Plots to compare the approaches ########################################

plt_comparison <- Y_SIS%>%ggplot(aes(x = Age_class, y = cum_kin, color = stage, fill = stage)) +
  geom_bar(position = "stack", stat = "identity")  + facet_grid(~ method) +
  ggtitle("Focal born into cluster 3") +
  scale_color_manual(labels = clus_lab, values = col_scal[1:no_stage])+
  scale_fill_manual(labels = clus_lab, values = col_scal[1:no_stage]) + theme_bw() +
  labs( fill = "Sister's stage", color = "Sister's stage") + 
  ylab("Expected no. sisters") + xlab("Age of Focal") +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = (45+90)/3 , hjust = 0.8)) + 
  theme(legend.position = "top") + ylim(c(0,0.25)) +
  ggeasy::easy_center_title() + scale_x_discrete()

plt_differences <- Y_SIS%>%ggplot(aes(x = Age_class, y = cum_kin, color = stage, shape = method)) +
  geom_point()  + facet_wrap(~ stage, scales = "free") +
  ggtitle("Comparison given Focal born into cluster 3") +
  scale_color_manual(labels = clus_lab, values = col_scal[1:no_stage])+
  scale_fill_manual(labels = clus_lab, values = col_scal[1:no_stage]) + theme_bw() +
  labs( fill = "Sister's stage", color = "Sister's stage") + 
  ylab("Expected no. sisters") + xlab("Age of focal") +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = (45+90)/3 , hjust = 0.8)) + 
  theme(legend.position = "top")  +
  ggeasy::easy_center_title() + scale_x_discrete()

plt_comparison
plt_differences
