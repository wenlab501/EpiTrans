#' Calculate geometry distance
#'
#' Calculate distance of longtitude and latitude
#'
#' @param lon Vector of longitude; its length stands for the number of points.
#' @param lat Vector of latitude; its length stands for the number of points.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
#' lon <- covid$x[1:100]
#' lat <- covid$y[1:100]
#'
#' distLongLat(lon,lat)
#' @export
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
