euclidean <- function(a, b){
  if(a < b){
    c <- b
    b <- a
    a <- c
  }
  ## a > b
    while (b != 0) {
      d <- b
      b <- a %% b
      a <- d
    }
  return(abs(a))
}
    
  
    
  
