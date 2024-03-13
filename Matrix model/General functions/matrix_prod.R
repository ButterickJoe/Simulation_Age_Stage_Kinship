
time_ordered_matrix_prod <- function(start, stop, list_mat){
  temp <- list()  
  dims <- dim(list_mat[[start]])
  n <- dims[1]
  
  if(stop < start){temp[[(1+length(temp))]] <- diag(n)}
  else if(stop == start){temp[[(1+length(temp))]] <- list_mat[[stop]]}
  else{
    temp[[1]] <- list_mat[[start]]
    for(i in start : (stop-1) ){
      temp[[(1+length(temp))]] <- list_mat[[(i+1)]]%*%temp[[(i-start+1)]]
    }
  }
  return(temp[[(length(temp))]])
}
