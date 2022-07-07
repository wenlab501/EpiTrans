#' Give sequential number by region
#'
#' Give sequential number by region
#'
#' @param region Vector of region name of each data points
#' @return
#'
#' @examples
#' SequentialNumbering(covid19$region)
#' @export

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
