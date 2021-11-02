#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

createPoints <- function(XY.data, crs = NULL, basemap = NULL){
  require(sf)
  if(is.null(crs)){
    if(!is.null(basemap)) crs <- st_crs(basemap)
    else crs <-4326
  }
  st_as_sf(XY.data,coords=c("x","y"), crs = crs)
}
