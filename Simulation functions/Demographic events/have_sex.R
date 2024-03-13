
### Newborns are drawn from a Poisson distribution with rate = ASFR (depending also on stage)

have_sex <- function(ind, transition_matrix_list, fertility_vector_list, mortality_vector_list){
  
  newborns <- unlist(lapply(1:nrow(ind), function(x){rpois(1,ind$prob_have_sex_and_child[x])}))
  index <- which(newborns>0)

  if(length(index)>0){
    no_newborns <- sum(newborns)
    stages_newborns <- ind$stage[index]
    mother_ID_newborns <- ind$ID[index]
    max_pop_ID <- ind$ID%>%max
    babies <- data.frame(ID = seq( (max_pop_ID+1) , max_pop_ID + no_newborns, 1), 
                     Age = 0, ## marginal stage dist
                     stage = rep(stages_newborns, times = newborns[index]), ## marginal stage dist
                     mother_ID = rep(mother_ID_newborns, times = newborns[index]), ## mother ID links Focal and other kin
                     alive = 1,
                     mig_probs = NA) 
    tran_prob <- lapply(1:nrow(babies), function(x){
      age <- 1
      stage <- babies$stage[x]
      return(transition_matrix_list[[1]][,stage])})
    babies$mig_probs <- tran_prob
    babies_prob <- unlist(lapply(1:nrow(babies), function(x){
      age <- 1
      stage <- babies$stage[x]
      return(fertility_vector_list[[stage]][1])}) )
    babies$prob_have_sex_and_child <- babies_prob
      die_prob <- unlist(lapply(1:nrow(babies), function(x){
      age <-  1
      stage <- babies$stage[x]
      return(1-mortality_vector_list[[stage]][1])}) )
    babies$prob_die <- die_prob

    ind <- rbind(ind, babies)
  }
  else{ind <- ind
  newborns <- data.frame(0)
  babies <- data.frame(0)}

return(list(ind,newborns,babies))
}


