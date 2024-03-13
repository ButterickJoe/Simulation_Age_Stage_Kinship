no_age <- 19
no_stage <- 7

source(here::here("Matrix model"  , "General functions" , "matrix_operations.R"))
source(here::here("Matrix model"  , "General functions" , "create_Focal_IC.R" ))
source(here::here("Matrix model"  , "General functions" ,  "create_MCs.R"))
source(here::here("Matrix model"  , "General functions" ,  "data_frame_construction.R"))
source(here::here("Matrix model"  , "General functions" ,  "matrix_prod.R"))
source(here::here("Matrix model"  , "Kin projections",  "Younger_aunts.R"))

df_out <- here::here("Outputs","Time invariant", "saved dataframes")
fs::dir_create(df_out)


plt_out <-  here::here("Outputs","Time invariant", "Figs")
fs::dir_create(df_out)



UU <- read_rds(here::here("Data", "Time invariant", "U_list.Rds"))
FF <- read_rds(here::here("Data", "Time invariant", "F_list.Rds"))
TT <- read_rds(here::here("Data", "Time invariant", "T_list.Rds"))
HH <- read_rds(here::here("Data", "Time invariant", "H_list.Rds"))

UB <- block_diag_function(UU) ## BD age survival matrix (one block each stage)
FB <- block_diag_function(FF) ## BD Fertility between stage matrix (one block for each age)
TB <- block_diag_function(TT) ## BD stage transition matrix (one block each stage)
HB <- block_diag_function(HH) ## BD matrix assigning newborns to a given age (one for each stage)
# Check dimensions
dim(UB)
dim(FB)
dim(TB)
dim(HB)
## Projection matrices
U_proj <- t(K_perm_mat(no_stage, no_age))%*%UB%*%K_perm_mat(no_stage, no_age)%*%TB
F_proj <- t(K_perm_mat(no_stage, no_age))%*%HB%*%K_perm_mat(no_stage, no_age)%*%FB

####################### Genealogical models ###

# TIME VARIANT MODEL

################ TIME VARIANT NEEDS A LIST OF REP MATS
lit_UU <- list()
lit_FF <- list()
for(i in 1:50){
  lit_FF[[i]] <- F_proj
  lit_UU[[i]] <- U_proj
}
P_UU <- list()
P_FF <- list()
pf <- P_gen_fer(U_proj,F_proj,no_age,no_stage)
pu <- P_gen_surv(U_proj,F_proj,no_age,no_stage)
for(i in 1:50){
  P_UU[[i]] <- pu
  P_FF[[i]] <- pf
}

no_age
no_stage
init_cond_kin1 <- list() ## conditioning of focal's initial stage
foreach(foc_IC = 3 )%do%{
  list_of_younger_aunts <- list()
  kin_description <- list(c("A=1","B=2")) ## check version
  foreach(kin = kin_description)%do%{
    A_relation <- kin[[1]]
    B_relation <- kin[[2]]
    A <- as.numeric(gsub(".*?([0-9]+).*", "\\1", A_relation))
    B <- as.numeric(gsub(".*?([0-9]+).*", "\\1", B_relation))
    X <- matrix(0, nrow = (no_age*no_stage), ncol = no_age)
    foreach(age_focal = 0 : (no_age-1))%do%{
      x_inx <- age_focal + 1
      X[, (x_inx)] <- 
        project_backwards_2_forwards_1_gen_over_steps_Y(no_age, no_stage, 2, foc_IC, age_focal, P_FF, P_UU, lit_FF, lit_UU)
    }
    
    XX <- kin_dists_full(X, no_age, 1, no_stage)
    XX$kin_relation <- rep( paste(kin[[1]] , kin[[2]], sep = ",") )
    list_of_younger_aunts[[ (1+length(list_of_younger_aunts)) ]] <- XX }
  ## end of loop
  list_of_younger_aunts_new <- do.call("rbind" , list_of_younger_aunts)
  list_of_younger_aunts_new$ic_state_focal <- rep(foc_IC, nrow(list_of_younger_aunts_new))
  init_cond_kin1[[(foc_IC)]] <- list_of_younger_aunts_new }


full_younger_aunt <- do.call("rbind" , init_cond_kin1)
############################################# Results ###########################

## data frame of matrix results
df_my_variant <- full_younger_aunt%>%dplyr::select(full_dist,age_foc,kin_age,kin_stage)%>%
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
  theme(legend.position = "top") + 
  ggeasy::easy_center_title() + scale_x_discrete() 
