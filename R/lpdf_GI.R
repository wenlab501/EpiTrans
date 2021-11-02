#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

lpdf_GI <- function(mean = 20, sd = 9){
  function(x) dgamma(x, shape=(mean^2)/sd, scale=sd/mean, log = T)
}
