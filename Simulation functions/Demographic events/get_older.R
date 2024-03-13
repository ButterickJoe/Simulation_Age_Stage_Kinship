
get_older <- function(ind, transition_matrix_list, fertility_vector_list, mortality_vector_list){

  ## Age by 1 class
  ind$Age <- ind$Age + 1
  
  ind <- filter(ind, Age <= 18) ## filter those who pass the last age-class
  
  if(nrow(ind)>0){ ### if the populaiton is not empty, we update everyone's migration probability, birth and death rates, according to their new age
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
  }
  return(ind)
}



