#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

bnd_modify <- function(sf){
  require("sf")
  bnd <- st_bbox(sf)
  x <- bnd[1]
  y <- bnd[2]
  X <- bnd[3]
  Y <- bnd[4]
  dX <- (X-x)/2
  dY <- (Y-y)/2
  Xc <- (x+X)/2
  Yc <- (y+Y)/2
  R <- dY/dX
  bnd[1] <- Xc-dX*ifelse(R<3/2,1.2,R*3/4)
  bnd[2] <- Yc-dY*ifelse(R>2/3,1.2,3/4/R)
  bnd[3] <- Xc+dX*ifelse(R<3/2,1.2,R*3/4)
  bnd[4] <- Yc+dY*ifelse(R>2/3,1.2,3/4/R)
  return(bnd)
}
