#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_lpdf_SW=function(mean = 125,from=0,to=1000){
  curve(exp(lpdf_SW(mean = 125)(x)),from,to,xlab='space distance (meters)',ylab='pdf',bty='l')
}

