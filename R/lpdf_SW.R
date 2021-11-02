#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

lpdf_SW <- function(mean = 125){
  function(x) dexp(x, rate = 1/mean, log = T)
}
