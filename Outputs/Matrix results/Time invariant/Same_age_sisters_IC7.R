no_age <- 19
no_stage <- 7

source(here::here("Matrix model"  , "General functions" , "create_Focal_IC.R"))
source(here::here("Matrix model"  , "General functions" , "create_MCs.R" ))
source(here::here("Matrix model"  , "General functions" ,  "data_frame_construction.R"))
source(here::here("Matrix model"  , "General functions" ,  "matrix_operations.R"))
source(here::here("Matrix model"  , "General functions" ,  "matrix_prod.R"))

source(here::here("Matrix model"  , "Kin projections" , "Same_age_and_Focal_Poisson.R"))

df_out <- here::here("Outputs", "Comparisons","Time invariant", "data frames")
fs::dir_create(df_out)


UU <- read_rds(here::here("Data", "Time invariant", "U_list.Rds"))
FF <- read_rds(here::here("Data", "Time invariant", "F_list.Rds"))
TT <- read_rds(here::here("Data", "Time invariant", "T_list.Rds"))
TT <- lapply(TT, function(x){abs(x)})
TT[[18]]
min(TT[[18]])
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

W_list_1 <- P_FF
V_list_1 <- P_UU
F_list_1 <- lit_FF
U_list_1 <- lit_UU

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
        project_backwards_1_forwards_1_gen_over_steps_Y_SA_pois(no_age, no_stage, 4, foc_IC, age_focal, W_list_1, V_list_1, F_list_1, U_list_1)
    }
    
    XX <- kin_dists_full(X, no_age, 1, no_stage)
    XX$kin_relation <- rep( paste(kin[[1]] , kin[[2]], sep = ",") )
    list_of_younger_sisters[[ (1+length(list_of_younger_sisters)) ]] <- XX }
  ## end of loop
  list_of_younger_sisters_new <- do.call("rbind" , list_of_younger_sisters)
  list_of_younger_sisters_new$ic_state_focal <- rep(foc_IC, nrow(list_of_younger_sisters_new))
  init_cond_kin1[[(foc_IC)]] <- list_of_younger_sisters_new }


full_younger_sis <- do.call("rbind" , init_cond_kin1)
full_younger_sis%>%head()
full_younger_sis$ic_state_focal
############################################# Results ###########################

## data frame of matrix results
df_my_variant_poiss <- full_younger_sis%>%dplyr::select(full_dist,age_foc,kin_age,kin_stage)%>%
  group_by(age_foc, kin_stage)%>%summarise(full_dist = sum(full_dist))%>%ungroup()

df_my_variant_poiss <- df_my_variant_poiss%>%
  transmute(Age_foc = age_foc, cum_kin = full_dist, stage = kin_stage,
            kin = "sisters and Focal" , method = "matrix model")%>%
  dplyr::select(Age_foc,cum_kin,stage,kin,method)


df_my_variant_poiss%>%ggplot(aes(x = Age_foc, y = cum_kin, color = stage, fill = stage)) +
  geom_bar(position = "stack" , stat = "identity") + xlim(c(0,19)) + ylim(c(0,1.2))



