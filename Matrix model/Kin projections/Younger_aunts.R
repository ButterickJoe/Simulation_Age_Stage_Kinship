### inputs years to over; A=2 number of reproductive events, list of projection matrices

project_backwards_2_forwards_1_gen_over_steps_Y <- function(na,ns,
                                                            foc_age_0, ## list position when focal born (t=0 in model)
                                                            foc_INIT, ## focal's initial stage
                                                            x, ## focal's age we project up to
                                                            list_mat_W, ## list of genealogical reproduction matrices
                                                            list_mat_V, ## list of genealogical survival matrices
                                                            list_mat_F, ## list of fertility matrices
                                                            list_mat_U){ ## list of mortality matrices
  
  no_age <- na
  no_stage <- ns
  mother_mat <- list_mat_W[[(foc_age_0-1)]] ## W_0 in the eqs. -- foc_0^{dag}*W_0 = mother's dist at foc birth
  time_scale_back <- min(na, foc_age_0) # start of matrix list to focal's birth entry
  phi_0 <- rep(0, ncol(list_mat_W[[1]])) ## focal's stage at birth
  phi_0[foc_INIT] <- 1
  
  acc_kin <- 0
  foreach(time = foc_age_0 : (foc_age_0 - time_scale_back) )%do%{ ## going "backwards" in time
    forwards <- 0
    foreach(j = time : (foc_age_0 + x) )%do%{
      forwards <- forwards + time_ordered_matrix_prod(j + 1, foc_age_0 + x - 1, list_mat_U)%*%
        list_mat_F[[j]]%*%
        time_ordered_matrix_prod(time  , j - 1, list_mat_U) ## different ways of one reproduction and x - 1 survivals
    }
    acc_kin <- acc_kin + forwards%*%t(t(phi_0)%*% ## focal's birth dist
                                        mother_mat%*% ## mother's dist when focal born
                                        time_ordered_matrix_prod(time + 1, foc_age_0 - 2  , list_mat_V)%*% ## back from mother giving birth, to her being born
                                        list_mat_W[[time]]) ## focal's grandmother when producing mother 
  }
  return(as.matrix(acc_kin) )
}


