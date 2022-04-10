#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_lpdf_GI=function(mean=20,sd=9,from=0,to=50){
  curve(exp(lpdf_GI(mean=mean,sd=sd)(x)),from,to,xlab='time lag (days)',ylab='pdf',bty='l')
}
