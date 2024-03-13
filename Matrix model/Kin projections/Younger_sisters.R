
source(here::here("Matrix model"  , "General functions" , "matrix_operations.R"))
source(here::here("Matrix model"  , "General functions" , "create_Focal_IC.R" ))
source(here::here("Matrix model"  , "General functions" ,  "create_MCs.R"))
source(here::here("Matrix model"  , "General functions" ,  "data_frame_construction.R"))
source(here::here("Matrix model"  , "General functions" ,  "matrix_prod.R"))




project_backwards_1_forwards_1_gen_over_steps_Y <- function(na, ns,
                                                            foc_age_0, ## list position when focal born (t=0 in model)
                                                            foc_INIT, ## focal's initial stage
                                                            x, ## focal's age for projections 
                                                            list_mat_W, ## genealogical reproductions
                                                            list_mat_V, ## genealogical survivals
                                                            list_mat_F, ## fert projections
                                                            list_mat_U){
  
  no_age <- na
  no_stage <- ns
  mother_mat <- list_mat_W[[(foc_age_0-1)]] ## W_0 in the eqs. -- foc_0^{dag}*W_0 = mother's dist at foc birth
  time_scale_back <- min(na*2, foc_age_0) # start of matrix list to focal's birth entry
  phi_0 <- rep(0, ncol(list_mat_W[[1]])) ## focal's stage at birth
  phi_0[foc_INIT] <- 1
  
  acc_kin <- 0
  foreach(time = (foc_age_0) : (foc_age_0 + x - 1) )%do%{
      if(x>0){
      acc_kin <- acc_kin + time_ordered_matrix_prod(time + 1, foc_age_0 + x - 1 , list_mat_U)%*%
        list_mat_F[[time]]%*%
        time_ordered_matrix_prod(foc_age_0 - 1 , time - 1 , list_mat_U)%*%t(t(phi_0)%*% ## focal's birth dist
                                                                           mother_mat) 
      
    }
  }
  
  return(as.matrix(acc_kin) )
}


