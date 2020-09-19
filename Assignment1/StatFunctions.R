#' @Title A function for finding the mode of a data set
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mode.stat <- function(x){
  ux = unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}