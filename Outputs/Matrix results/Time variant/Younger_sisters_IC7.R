no_age <- 19
no_stage <- 7

source(here::here("Matrix model"  , "General functions" , "matrix_operations.R"))
source(here::here("Matrix model"  , "General functions" , "create_Focal_IC.R" ))
source(here::here("Matrix model"  , "General functions" ,  "create_MCs.R"))
source(here::here("Matrix model"  , "General functions" ,  "data_frame_construction.R"))
source(here::here("Matrix model"  , "General functions" ,  "matrix_prod.R"))
source(here::here("Matrix model"  , "Kin projections",  "Younger_sisters.R"))

df_out <- here::here("Outputs","Time variant", "saved dataframes")
fs::dir_create(df_out)


plt_out <-  here::here("Outputs","Time variant", "Figs")
fs::dir_create(df_out)

F_list <- read_rds(here::here("Data", "Time variant", "TV_list_fert_MS_NAT.Rds"))
U_list <- read_rds(here::here("Data", "Time variant", "TV_list_suv_MS_NAT.Rds"))
W_list <- read_rds(here::here("Data", "Time variant", "TV_list_gen_fert_MS_NAT.Rds")) ## assuming eigendecomposition
V_list <- read_rds(here::here("Data", "Time variant", "TV_list_gen_suv_MS_NAT.Rds")) ## assuming eigendecomposition -- steady demography

W_list_alt <- gen_fer_list1 ## projecting pop structure from first set of TV rates
V_list_alt <- gen_suv_list1

#W_list <- W_list_alt ## use if preferable
#V_list <- V_list_alt

F_list%>%length()
V_list%>%length()

F_list_low <- lapply(1:2, function(j) F_list[[1]]) ## pad invariant rates at beginning 
F_list_high <- lapply(1:18, function(j) F_list[[21]]) ## pad invariant rates at end
U_list_low <- lapply(1:2, function(j) U_list[[1]])
U_list_high <- lapply(1:18, function(j) U_list[[21]])
V_list_low <- lapply(1:2, function(j) V_list[[1]])
V_list_high <- lapply(1:18, function(j) V_list[[21]])
W_list_low <- lapply(1:2, function(j) W_list[[1]])
W_list_high <- lapply(1:18, function(j) W_list[[21]])

F_list_1 <- c(F_list_low, F_list, F_list_high)
U_list_1 <- c(U_list_low, U_list, U_list_high)
W_list_1 <- c(W_list_low, W_list, W_list_high)
V_list_1 <- c(V_list_low, V_list, V_list_high)

no_age <- 19
no_stage <- 7
no_age
no_stage
init_cond_kin1 <- list() ## conditioning of focal's initial stage
foreach(foc_IC = 7 )%do%{
  list_of_younger_sisters <- list()
  kin_description <- list(c("A=1","B=1")) ## check version
  foreach(kin = kin_description)%do%{
    A_relation <- kin[[1]]
    B_relation <- kin[[2]]
    A <- as.numeric(gsub(".*?([0-9]+).*", "\\1", A_relation))
    B <- as.numeric(gsub(".*?([0-9]+).*", "\\1", B_relation))
    X <- matrix(0, nrow = (no_age*no_stage), ncol = no_age)
    foreach(age_focal = 0 : (no_age-1))%do%{
      x_inx <- age_focal + 1
      X[, (x_inx)] <- 
        project_backwards_1_forwards_1_gen_over_steps_Y(no_age, no_stage, 4, foc_IC, age_focal, W_list_1, V_list_1, F_list_1, U_list_1)
    }
    XX <- kin_dists_full(X, no_age, 1, no_stage)
    XX$kin_relation <- rep( paste(kin[[1]] , kin[[2]], sep = ",") )
    list_of_younger_sisters[[ (1+length(list_of_younger_sisters)) ]] <- XX }
  ## end of loop
  list_of_younger_sisters_new <- do.call("rbind" , list_of_younger_sisters)
  list_of_younger_sisters_new$ic_state_focal <- rep(foc_IC, nrow(list_of_younger_sisters_new))
  init_cond_kin1[[(foc_IC)]] <- list_of_younger_sisters_new }

full_younger_sis <- do.call("rbind" , init_cond_kin1)
############################################# Results ###########################

## data frame of matrix results
df_my_variant <- full_younger_sis%>%dplyr::select(full_dist,age_foc,kin_age,kin_stage)%>%
  group_by(age_foc, kin_stage)%>%summarise(full_dist = sum(full_dist))%>%ungroup()

df_my_variant <- df_my_variant%>%
  transmute(Age_foc = age_foc, cum_kin = full_dist, stage = kin_stage,
            kin = "younger sisters" ,method = "matrix model")%>%
  dplyr::select(Age_foc,cum_kin,stage,kin,method)

df_my_variant <- df_my_variant%>%mutate(Foc_age_class = paste(5*Age_foc, 5*Age_foc + 4, sep = "-"),
                                        Foc_age_class = reorder(Foc_age_class, Age_foc, mean))

ic_lab <- as_labeller(c('1' = "Focal born in cluster 1",
                        '2' = "Focal born in cluster 2",
                        '3' = "Focal born in cluster 3",
                        '4' = "Focal born in cluster 4",
                        '5' = "Focal born in cluster 5",
                        '6' = "Focal born in cluster 6",
                        '7' = "Focal born in cluster 7"))  # for facet plots
clus_lab <- c("Cluster 1", "Cluster 2","Cluster 3",
              "Cluster 4", "Cluster 5",
              "Cluster 6", "Cluster 7")
col_scal <- RColorBrewer::brewer.pal(10, "Set1")

df_my_variant%>%
  ggplot(aes(x = Foc_age_class, y = cum_kin , color = stage, fill = stage)) + 
  geom_bar(position = "stack" , stat = "identity") + 
  scale_color_manual(labels = clus_lab, values = col_scal[1:no_stage])+
  scale_fill_manual(labels = clus_lab,values = col_scal[1:no_stage]) + theme_bw() +
  labs( fill = "Sister's stage", color = "Sister's stage") + 
  ylab("Expected no. sisters") + xlab("Age of Focal") +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = (45+90)/3 , hjust = 0.8)) + 
  theme(legend.position = "top") + ylim(c(0,0.25))+
  ggeasy::easy_center_title() + scale_x_discrete() 





