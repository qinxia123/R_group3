#' Euclidean Algorithm for GCD
#'
#' This function calculates the Greatest Common Divisor (GCD) of two integers using the Euclidean algorithm.
#'
#' @param a A numeric value representing the first integer.
#' @param b A numeric value representing the second integer.
#'
#' @return The GCD of the two input integers.
#' 
#' @details The Euclidean algorithm works by repeatedly applying the division algorithm. The process continues
#' until the remainder is zero, at which point the divisor is the GCD of the two numbers.
#'
#' @references \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Euclidean Algorithm - Wikipedia}
#' 
#' @examples
#' euclidean(252, 105)
#' euclidean(123612, 13892347912)
#' 
#' @export
euclidean <- function(a, b) {
  # Euclidean algorithm implementation
}



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
    
  
    
  
