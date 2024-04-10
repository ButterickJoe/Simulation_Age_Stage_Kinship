no_age <- 19
no_stage <- 7

source(here::here("Matrix model"  , "General functions" , "matrix_operations.R"))
source(here::here("Matrix model"  , "General functions" , "create_Focal_IC.R" ))
source(here::here("Matrix model"  , "General functions" ,  "create_MCs.R"))
source(here::here("Matrix model"  , "General functions" ,  "data_frame_construction.R"))
source(here::here("Matrix model"  , "General functions" ,  "matrix_prod.R"))
source(here::here("Matrix model"  , "Kin projections",  "Older_sisters.R"))

df_out <- here::here("Outputs", "Comparisons" , "Time invariant", "data frames")
fs::dir_create(df_out)
plt_out <-  here::here("Outputs","Time invariant", "Figs")
fs::dir_create(df_out)

##### Read in the moratlity, fertility, stage transfer, and redistribution (of offspring) matrices ######
##### Mortality matrices: age*age dimension, with a unique one for each stage
##### Fertility matrices: stage*stage dimension, with one for each age
##### Transfer matrices: stage*stage dimension, with one for each age
##### Redistribution matrices: age*age dimension, with one for each stage
UU <- read_rds(here::here("Data", "Time invariant", "U_list.Rds"))
FF <- read_rds(here::here("Data", "Time invariant", "F_list.Rds"))
TT <- read_rds(here::here("Data", "Time invariant", "T_list.Rds"))
HH <- read_rds(here::here("Data", "Time invariant", "H_list.Rds"))
##### Direct sums of the above create block diagonal matrices (see Section 1.1 in paper) 
UB <- block_diag_function(UU) ## BD age survival matrix (one block each stage)
FB <- block_diag_function(FF) ## BD Fertility between stage matrix (one block for each age)
TB <- block_diag_function(TT) ## BD stage transition matrix (one block each stage)
HB <- block_diag_function(HH) ## BD matrix assigning newborns to a given age (one for each stage)
# Check dimensions -- all matrices should be of dimension age*stage by age*stage
dim(UB)
dim(FB)
dim(TB)
dim(HB)
## Projection matrices construction ########
U_proj <- t(K_perm_mat(no_stage, no_age))%*%UB%*%K_perm_mat(no_stage, no_age)%*%TB
F_proj <- t(K_perm_mat(no_stage, no_age))%*%HB%*%K_perm_mat(no_stage, no_age)%*%FB

####################### Genealogical models ###

# TIME VARIANT MODEL

################ TIME VARIANT NEEDS A LIST OF REP MATS
lit_UU <- list()
lit_FF <- list()
for(i in 1:60){
  lit_FF[[i]] <- F_proj
  lit_UU[[i]] <- U_proj
}

pf <- P_gen_fer(U_proj,F_proj,no_age,no_stage)
pu <- P_gen_surv(U_proj,F_proj,no_age,no_stage)


##################### Version 1 ###########################
P_UU <- list()
P_FF <- list()
for(i in 1:20){
  P_UU[[i]] <- pu
  P_FF[[i]] <- pf
}
for(i in 1:8){
  P_UU[[(20+i)]] <- gen_suv_list2[[i]]
  P_FF[[(20+i)]] <- gen_fer_list2[[i]]
}
for(i in 1:30){  
  P_UU[[(28+i)]] <- pu
  P_FF[[(28+i)]] <- pf
}


P_UU[[27]]
P_FF[[32]]
F_proj
############################################
no_age
no_stage
init_cond_kin1 <- list() ## conditioning of focal's initial stage
foreach(foc_IC = 3 )%do%{
  list_of_older_sisters <- list()
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
        project_backwards_1_forwards_1_gen_over_steps_O(no_age, no_stage, 28, foc_IC, age_focal, P_FF, P_UU, lit_FF, lit_UU)
    }
    
    XX <- kin_dists_full(X, no_age, 1, no_stage)
    XX$kin_relation <- rep( paste(kin[[1]] , kin[[2]], sep = ",") )
    list_of_older_sisters[[ (1+length(list_of_older_sisters)) ]] <- XX }
  ## end of loop
  list_of_older_sisters_new <- do.call("rbind" , list_of_older_sisters)
  list_of_older_sisters_new$ic_state_focal <- rep(foc_IC, nrow(list_of_older_sisters_new))
  init_cond_kin1[[(foc_IC)]] <- list_of_older_sisters_new }


full_older_sisTV <- do.call("rbind" , init_cond_kin1)



############################################# Results ###########################
full_older_sis

## data frame of matrix results
df_my_variantTV <- full_older_sisTV%>%dplyr::select(full_dist,age_foc,kin_age,kin_stage)%>%
  group_by(age_foc, kin_stage)%>%summarise(full_dist = sum(full_dist))%>%ungroup()
df_my_variantTV <- df_my_variantTV%>%
  transmute(Age_foc = age_foc, cum_kin = full_dist, stage = kin_stage,
            kin = "younger sisters" ,method = "gen mat")%>%
  dplyr::select(Age_foc,cum_kin,stage,kin,method)

df_out <- here::here("Outputs","Comparisons", "Time invariant"  , "data frames")
fs::dir_create(df_out)

saveRDS(df_my_variantTV, file = paste0(df_out , "/" , "OS_MatrixResult_IC3.Rds" ))

o_sis <- df_my_variantTV
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85-90", "90-94","95+")
o_sis$Age_class <- cut(o_sis$Age_foc, agebreaks, labels = agelabels)
o_sis%>%ggplot(aes(x = Age_class, y=cum_kin, color=stage,fill=stage))+
  geom_bar(position="stack", stat="identity") +
  scale_color_manual(labels = clus_lab, values = col_scal[1:no_stage])+
  scale_fill_manual(labels = clus_lab, values = col_scal[1:no_stage]) + theme_bw() +
  labs( fill = "Sister's stage", color = "Sister's stage") + 
  ylab("Expected no. sisters") + xlab("Age of focal") +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = (45+90)/3 , hjust = 0.8)) + 
  theme(legend.position = "top") + ylim(c(0,0.25)) +
  ggeasy::easy_center_title() + scale_x_discrete()

o_sis%>%head()
full_older_sis%>%filter(age_foc==10 & kin_age==10)


