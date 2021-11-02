#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

ReduceMatrix <- function(matrix,group){
  ReM <- function(x) tapply(x,group,function(t) sum(t,na.rm=T))
  return(apply(apply(matrix,1,ReM),1,ReM))
}
