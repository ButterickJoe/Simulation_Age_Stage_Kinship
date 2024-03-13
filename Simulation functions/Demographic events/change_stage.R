
################################################ Move individuals between stages ###########################

create_new_stage <- function(ind, transition_matrix_list, fertility_vector_list, mortality_vector_list){

  ### ind is a data frame of the population, with a variable "mig_probs" giving probabilities of moving from current stage to any other
  ### Use mig_probs as a vector of probabilities which define a multi-nomial distribution and draw those population members who move.
  
  ind$stage <- unlist(lapply(1:nrow(ind), function(x){
    new_stage = which(rmultinom(1, 1, prob = as.vector(ind$mig_probs[x])%>%unlist())==1)
    return(new_stage)})) 

  ### After the movers and non-movers have been accounted for, we update the population to new specific migration probs, birth rates, and death rates.... 
  tran_prob <- lapply(1:nrow(ind), function(x){
    age <- ind$Age[x] + 1
    stage <- ind$stage[x]
    return(transition_matrix_list[[age]][,stage])})
  ind$mig_probs <- tran_prob
  
  babies_prob <- unlist(lapply(1:nrow(ind), function(x){
    age <- ind$Age[x] + 1
    stage <- ind$stage[x]
    return(fertility_vector_list[[stage]][age])}) )
  ind$prob_have_sex_and_child <- babies_prob
  
  die_prob <- unlist(lapply(1:nrow(ind), function(x){
    age <- ind$Age[x] + 1
    stage <- ind$stage[x]
    return(1-mortality_vector_list[[stage]][age])}) )
  ind$prob_die <- die_prob

  #### return the population 
  return(ind)}


  
