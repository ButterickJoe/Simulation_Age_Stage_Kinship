

get_older <- function(ind, transition_matrix_list, fertility_vector_list, mortality_vector_list){
  
  ind$Age <- ind$Age + 1
  
  ind <- filter(ind, Age <= 18)
  
  if(nrow(ind)>0){
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



