#' Calculate the probability distribution of generation interval
#'
#' Probability distribution (gamma distribution) of generation interval, defined as the time interval between the infection (or onset) time of infector and infectee.
#'
#' @param mean Mean of the probability distribution of generation interval.
#' @param sd Standard deviation of the probability distribution of generation interval.
#' @return
#'

#' @examples
#' lpdf_GI(mean = 20,sd = 9)
#' @export
#'
lpdf_GI <- function(mean = 20, sd = 9){
  function(x) dgamma(x, shape=(mean^2)/sd, scale=sd/mean, log = T)
}
