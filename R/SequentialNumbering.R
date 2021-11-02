#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

SequentialNumbering <- function(region){
  regions <- unique(region)
  nums <- numeric(length(regions))
  no <- c()
  for(i in seq(region)){
    id <- regions==region[i]
    no[i] <- nums[id] <- nums[id]+1
  }
  return(paste0(region,no))
}
