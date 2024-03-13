source(here::here("Matrix models"  , "General functions" , "Functions_matrix.R"))
source(here::here("Matrix models"  , "General functions" , "Focal_IC.R" ))
source(here::here("Matrix models"  , "General functions" ,  "Genealogical_MCs.R"))
source(here::here("Matrix models"  , "General functions" ,  "df_construction.R"))
source(here::here("Matrix models"  , "General functions" ,  "matrix_product.R"))


### inputs years to over; A=2 number of reproductive events, list of projection matrices

project_backwards_1_forwards_1_gen_over_steps_O <- function(na, ns,
                                                            foc_age_0, ## list position when focal born (t=0 in model)
                                                            foc_INIT, ## focal's initial stage
                                                            x, ## focal's age for projections 
                                                            list_mat_W, ## genealogical reproductions
                                                            list_mat_V, ## genealogical survivals
                                                            list_mat_F, ## fert projections
                                                            list_mat_U){
  
  no_age <- na
  no_stage <- ns
  mother_mat <- list_mat_W[[foc_age_0]] ## W_0 in the eqs. -- foc_0^{dag}*W_0 = mother's dist at foc birth
  time_scale_back <- na # start of matrix list to focal's birth entry
  phi_0 <- rep(0, ncol(list_mat_W[[1]])) ## focal's stage at birth
  phi_0[foc_INIT] <- 1
  
  ## A=1 and B=1 situation ###############################################
  acc_kin <- 0
  foreach(time = (foc_age_0-1) : (foc_age_0 - time_scale_back) )%do%{
    ## we travel from foc_age_0 backwards (left along list) <--t-- w_-t, w_-t+1, ..., W_0
    backwards <- t(t(phi_0)%*% ## focal's birth dist
                     mother_mat%*% ## mother's dist when focal born
                     time_ordered_matrix_prod(time, (foc_age_0-1), list_mat_V))
    
    forwards <- time_ordered_matrix_prod( (time ), (foc_age_0 + x - 1), list_mat_U)%*%list_mat_F[[(time-1)]]
    acc_kin <- acc_kin + forwards%*%backwards  }
  return(as.matrix(acc_kin))}


