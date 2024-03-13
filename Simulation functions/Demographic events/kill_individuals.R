
do_deaths <- function(ind){
  
dead <- unlist(lapply(1:nrow(ind), function(x){rbinom(1, 1, ind$prob_die[x])}))
dead2 <- which(dead == 1)
dead1 <- which(dead == 0)
ind <- ind[dead1,]

return(list(ind,dead2))
}


