
source(here::here("Matrix model", "General functions" , "matrix_prod.R"))
source(here::here("Matrix model", "General functions" , "create_MCs.R"))
source(here::here("Matrix model", "General functions" , "data_frame_construction.R"))

## input :  number ages (classes),  number of stages, focal's stage at birth, focal's age at present (project up to)
##          list of genealogical reproductive events: ie. prob that time minus one ancestor B-1 --> B
##          list of genealogica survival events: ie., prob that time minus one, ancestor B -- > B
#           list of fertility projection matrices
#           list of survival projection matrices



project_backwards_1_forwards_1_gen_over_steps_Y_SA_pois <- function(na, ns,
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
  phi_0 <- rep(0, ncol(list_mat_W[[(1)]])) ## focal's stage at birth
  phi_0[foc_INIT] <- 1
  
  F_same <- list_mat_F[[(foc_age_0-1)]]
  F_same <-  F_same/(1 - exp(-F_same)) 
  #F_same <- 1 + F_same
  F_same[is.na(F_same)]<-0
  
  
  acc_kin <- time_ordered_matrix_prod(foc_age_0 , foc_age_0 + x - 1 , list_mat_U)%*%F_same%*%t(t(phi_0)%*%mother_mat) 
  
  
  return(as.matrix(acc_kin) )
}


