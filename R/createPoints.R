#' Create points data
#'
#' Create sf points data by data frame with coordinates
#'
#' @param XY.data Data frame of points;must (at least) contain a column of x coordinate called 'x', and a column of y coordinate called 'y';and other column if needed such as 't' (time), 'Rj' (reproductive numbers ).
#' @param crs Coordinate reference system of data points: numeric (EPSG code), object of class 'crs', or input string for st_crs; if NULL, using CRS of basemap, or using WGS84 (EPSG:4326).
#' @param basemap Object of class 'sf'; reference crs to create points on the crs of this basemap
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' points = RndPts[,2:3]
#'
#' createPoints(points,crs = 4326,basemap = Taiwan)
#' @import sf
#' @export
#'
createPoints <- function(XY.data, crs = NULL, basemap = NULL){
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package \"sf\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if(is.null(crs)){
    if(!is.null(basemap)) crs <- sf::st_crs(basemap)
    else crs <-4326
  }
  sf::st_as_sf(XY.data,coords=c("x","y"), crs = crs)
}
