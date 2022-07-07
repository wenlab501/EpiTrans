#' Create base map
#'
#' Create different type of base map on plot
#'
#' @param basemap Object of class 'sf'; basemap plotting on map
#' @param bnd A 4-length numerical vector of the form c(xmin, ymin, xmax, ymax) which gives the number of x and y coordinates' ranges of the map.
#' @param gridLonLat Logical; draw latitude and longitude grid on non-interactive map.
#' @param interact Logical; if TRUE, plot thematic interactive map; if FALSE, static plotting non-interactive thematic map.
#' @param alphaGrid 0~1; transparency number between 0 (totally transparent) and 1 (not transparent).
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' CreateBaseMap(Taiwan,bnd = bnd_modify(Taiwan),gridLonLat = TRUE,interact = TRUE,alphaGrid = 1)
#' @import sf
#' @import tmap
#' @export
CreateBaseMap  <- function(basemap, bnd, gridLonLat, interact, alphaGrid = 1) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package \"sf\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("tmap", quietly = TRUE)) {
    stop(
      "Package \"tmap\" must be installed to use this function.",
      call. = FALSE
    )
  }
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

