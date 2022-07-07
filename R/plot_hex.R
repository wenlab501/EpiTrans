#' Visualizing Point pattern
#'
#' Plot the spatial pattern of cases by hexagon-bining; aggregate the point into specified hexagon grids
#'
#' @param x Vector of x coordinates of data points.
#' @param y Vector of y coordinates of data points.
#' @param Rj Vector of individual reproductive numbers of data points.
#' @param grid.n Number of grid cells in one direction (shorter side).
#' @param crs Coordinate reference system of data points: numeric (EPSG code), object of class 'crs', or input string for st_crs; if NULL, using CRS of basemap, or using WGS84 (EPSG:4326).
#' @param bnd A 4-length numerical vector of the form c(xmin, ymin, xmax, ymax) which gives the number of x and y coordinates' ranges of the map.
#' @param basemap Object of class 'sf'; basemap plotting on map.
#' @param interact Logical; if TRUE, plot thematic interactive map; if FALSE, static plotting non-interactive thematic map.
#' @param gridLonLat Logical; draw latitude and longitude grid on non-interactive map.
#' @param title Main title of plot.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' plot_hex(x = dengue$long, y= dengue$lat, grid.n=30, basemap = Taiwan)
#' @export
#'
plot_hex <- function(x, y, Rj = NULL, grid.n=50, crs = NULL, bnd = NULL, basemap = NULL, interact = TRUE, gridLonLat = TRUE, title = ""){
  plot_grid(x, y, Rj, hex = T, grid.n, crs, bnd, basemap, interact, gridLonLat, title)
}
