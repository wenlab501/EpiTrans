#' Adjust the boundary of the plot map
#'
#' Auto adjust the boundary of the plot map
#'
#' @param sf Object of class 'sf'.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' bnd <- bnd_modify(Taiwan)
#' @import sf
#' @export

bnd_modify <- function(sf){
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package \"sf\" must be installed to use this function.",
      call. = FALSE
    )
  }

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
