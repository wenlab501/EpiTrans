#' Visualizing probability density functions.
#'
#' Plot the probability distribution of tranmission distance
#'
#' @param mean Mean tranmission distance of exponential distribution.
#' @param from Minimum of x of the plot.
#' @param to Maximum of x of the plot.
#'
#' @examples
#' plot_lpdf_SW(mean = 125, from = 0, to = 1000)
#' @export

plot_lpdf_SW=function(mean = 125,from=0,to=1000){
  curve(exp(lpdf_SW(mean = 125)(x)),from,to,xlab='space distance (meters)',ylab='pdf',bty='l')
}

