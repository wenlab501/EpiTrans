#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

CreateBaseMap  <- function(basemap, bnd, gridLonLat, interact, alphaGrid = 1) {
  BaseMap <- NULL
  if(gridLonLat & !interact) BaseMap <- tm_grid(col = "white") + tm_layout (bg.color = "#D2E9FF")
  if(!is.null(basemap)){
    basemap <- st_transform(basemap, 4326)
    if(interact){
      BaseMap <- tm_shape(basemap, bbox = bnd) +
        tm_polygons(col = "darkgreen", alpha = .1, border.col = "darkgreen")
    }else{
      BaseMap <- BaseMap +
        tm_shape(basemap, bbox = bnd) +
        tm_polygons(col = "#DFFFDF", alpha = alphaGrid, border.col = "darkgreen") +
        tm_layout (bg.color = "#D2E9FF")
    }
  }
  return(BaseMap)
}

