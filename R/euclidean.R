#' Find greatest common divisor (GCD) of two integers (numbers).
#' 
#' @param a is a numerical number
#' @param b is a numerical number
#' @return greatest common divisor from integers \code{a} and \code{b}
#' 
#' @author Chandika
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' 
#' @export euclidean
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)


euclidean<-function(a,b){
  if (! is.numeric(a) || ! is.numeric(b)) {
    stop("Both arguments must be numeric integers.")
  }
  
  
  # check a & b both are non negative
  
  a <- abs(a)
  b <- abs(b)
  
  while (b != 0) {
    temp_value <- b
    b <- a %% b
    a <- temp_value
  }
  return(a)
}



