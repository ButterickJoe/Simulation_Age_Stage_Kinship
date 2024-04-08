

df_out <- here::here("Outputs", "Comparisons","Time invariant", "data frames")
fs::dir_create(df_out)


plt_out <- here::here("Outputs", "Comparisons","Time invariant", "Figures")
fs::dir_create(df_out)


sim_sis_df <- read_rds(here::here("Outputs", "Comparisons","Time invariant", "data frames", "SS_F_IC7.Rds"))
sim_sis_df%>%head()

nrow(sim_sis_df)

#sim_sis_df <- rbind(sim_sis_df2,sim_sis_df1)
## pad the year 0 other stages as na
y0df <- data.frame(year = 0, stage = c(1,2,3,4,5,6), IC = 7, expected_val = 0)

sim_sis_df <- rbind(y0df,sim_sis_df)
sim_sis_df<-sim_sis_df%>%transmute(Age_foc = (year), cum_kin = expected_val, 
                                   stage = stage, kin = "sisters and Focal")
sim_sis_df

sim_sis_df$method <- "stochastic simulation"
nrow(sim_sis_df)


M_COMP <- rbind(df_my_variant_poiss,sim_sis_df)
M_COMP

M_COMP%>%filter(method == "Poisson" | method == "stochastic simulation")%>%
  ggplot(aes(x = Age_foc, y = cum_kin, color = stage, fill = stage)) +
  geom_bar(position = "stack", stat = "identity")  + facet_grid(~ method) +
  ggtitle("Comparison given Focal born into cluster 7") +
  scale_color_manual(labels = clus_lab, values = col_scal[1:no_stage])+
  scale_fill_manual(labels = clus_lab, values = col_scal[1:no_stage]) + theme_bw() +
  labs( fill = "Sister & Focal's stage", color = "Sister & Focal's stage") + 
  ylab("Expected no. Focal + same age sisters") + xlab("Age of Focal") +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = (45+90)/3 , hjust = 0.8)) + 
  theme(legend.position = "top") + 
  ggeasy::easy_center_title() + scale_x_discrete()+ ylim(c(0,1.25))

agebreaks <- seq(-1,19,1)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85-90", "90-94","95+")
M_COMP$Age_class <- cut(M_COMP$Age_foc, agebreaks, labels = agelabels)


clus_lab <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")

col_scal <- RColorBrewer::brewer.pal(10, "Set1")

######## Plots by Focal's age ########################################



M_COMP$cum_kin <- as.numeric(M_COMP$cum_kin)
M_COMP%>%head()
M_COMP_wide <- M_COMP%>%pivot_wider(names_from = "method", values_from = "cum_kin")

M_COMP_wide%>%head()
M_COMP_WIDE <- M_COMP_wide%>%
  mutate(diff_simulation_truncated_Poisson = `stochastic simulation` - `Poisson`)


plot_sim_ves_mat <- M_COMP_WIDE%>%
  dplyr::select(stage,Age_class,diff_simulation_truncated_Poisson)%>%
  melt(id = c("Age_class" , "stage") )%>%ggplot(aes(x = Age_class,  y =value,  color = stage)) + geom_point() + theme_bw()+ 
  ylab("differnce") + xlab("Age class") +
  theme(axis.text.x = element_text(angle = 90)) + ylim(c(-0.1,0.1))+
  scale_color_manual(labels = clus_lab, values = col_scal[1:no_stage])+
  scale_fill_manual(labels = clus_lab, values = col_scal[1:no_stage])  +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = (45+90)/3 , hjust = 0.8)) + 
  theme(legend.position = "top") +  
  ggtitle("Differences: Matrix model vs simualtion") + 
  ggeasy::easy_center_title() + scale_x_discrete()

plot_sim_ves_mat


ggsave(plot_sim_ves_mat, file = paste0(plt_out , "/" , "SS_and_F_simulation_v_matrix.png" ), width = 6, height = 4, dpi = 300)


plt_SS1_and_Foc <-  M_COMP%>%filter(method == "Poisson" | method == "stochastic simulation")%>%
  ggplot(aes(x = Age_class, y = cum_kin, color = stage, fill = stage)) +
  geom_bar(position = "stack", stat = "identity")  + facet_grid(~ method) +
  ggtitle("Comparison given Focal born into cluster 7") +
  scale_color_manual(labels = clus_lab, values = col_scal[1:no_stage])+
  scale_fill_manual(labels = clus_lab, values = col_scal[1:no_stage]) + theme_bw() +
  labs( fill = "Sister & Focal's stage", color = "Sister & Focal's stage") + 
  ylab("Expected no. Focal + same age sisters") + xlab("Age of Focal") +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = (45+90)/3 , hjust = 0.8)) + 
  theme(legend.position = "top") + 
  ggeasy::easy_center_title() + scale_x_discrete()+ ylim(c(0,1.25))

plt_SS1_and_Foc

plt_out

ggsave(plt_SS1_and_Foc, file = paste0(plt_out , "/" , "SS_and_F_IC7.png" ), width = 8, height = 5, dpi = 300)
