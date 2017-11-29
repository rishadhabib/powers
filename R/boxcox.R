#' Apply a box cox power transformation
#'
#' This function carries out a box cox transformation
#'
#' @param x The vector to be transformed.
#' @param plot_it Display a plot of \code{x} vs the output? Use logical.
#' \code{FALSE} by default.
#'
#' @return
#' A vector that is the:
#' \itemize{
#'      \item boxcox (for \code{boxcox})
#' }
#'  of \code{x}.
#'
#' @details this does a box cox transformation
#' @examples
#' boxcox(c(1:10), 2)




#' @rdname boxcox
#' @export

boxcox <- function(y=1,lambda=1, add_plot=FALSE) {
  
  #checks
  if(any(y <= 0)){return("y cannot be less than or equal to zero")}
  
  if (lambda==0){return(log(y))}
  if (lambda!=0){
    return(((y^lambda)-1)/lambda)
  }
}