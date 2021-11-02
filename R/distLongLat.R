#' Title
#'
#' Descriptions Here
#' @param lon mean.
#' @param lat sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

distLongLat <- function(lon,lat) {
  N <- length(lon)
  x <- as.numeric(lon)*pi/180
  y <- as.numeric(lat)*pi/180
  x <- matrix(x,N,N)
  x1 <- x [!upper.tri(x,T)]
  x2 <- t(x)[lower.tri(x)]
  y <- matrix(y,N,N)
  y1 <- y [!upper.tri(y,T)]
  y2 <- t(y)[lower.tri(y)]
  ds <- sin((y2-y1)/2)^2+ cos(y1)*cos(y2)*sin((x2-x1)/2)^2
  ds <- 2*6378137*asin(sqrt(ds))
  return(ds)
}
