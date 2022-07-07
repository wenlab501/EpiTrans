#' Calculate probability distribution of tranmission distance
#'
#' Spatial weighting function; probability distribution (exponential distribution) of tranmission distance
#'
#' @param mean Mean tranmission distance of exponential distribution
#' @return
#'
#' @examples
#' lpdf_SW(mean = 125)
#' @export

lpdf_SW <- function(mean = 125){
  function(x) dexp(x, rate = 1/mean, log = T)
}
