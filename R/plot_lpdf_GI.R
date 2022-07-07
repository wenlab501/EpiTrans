#' Visualizing probability density functions.
#'
#' Plot the probability distribution of generation interval
#'
#' @param mean Mean of the probability distribution of generation interval.
#' @param sd Standard deviation of the probability distribution of generation interval.
#' @param from Minimum of x of the plot.
#' @param to Maximum of x of the plot.
#'
#' @examples
#' plot_lpdf_GI(mean = 20, sd = 9 ,from = 0, to = 50)
#' @export

plot_lpdf_GI=function(mean=20,sd=9,from=0,to=50){
  curve(exp(lpdf_GI(mean=mean,sd=sd)(x)),from,to,xlab='time lag (days)',ylab='pdf',bty='l')
}
