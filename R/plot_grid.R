#' Visualizing Point pattern
#'
#' Plot the spatial pattern of cases by grid; aggregate the point into specified grids
#'
#' @param x Vector of x coordinates of data points.
#' @param y Vector of y coordinates of data points.
#' @param Rj Vector of individual reproductive numbers of data points.
#' @param hex Logical; if'TRUE', create hexagonal grid ; if'FALSE', create square grid.
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
#' plot_grid(x = dengue$long, y= dengue$lat, grid.n=30, basemap = Taiwan)
#' @import sf
#' @import tmap
#' @import tmaptools
#' @export

plot_grid <- function(x, y, Rj = NULL, hex = FALSE, grid.n=50, crs = NULL, bnd = NULL, basemap = NULL, interact = TRUE, gridLonLat = TRUE, title = ""){
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
  if (!requireNamespace("tmaptools", quietly = TRUE)) {
    stop(
      "Package \"tmaptools\" must be installed to use this function.",
      call. = FALSE
    )
  }


  if(interact) tmap_mode("view") else tmap_mode("plot")
  if(is.null(crs)) crs <- 4326

  # create points
  XY.data <- data.frame(x, y)
  if(!is.null(Rj)) XY.data$Rj <- Rj
  points <- createPoints(XY.data, crs, basemap)

  # create grids
  box <- st_bbox(points)
  grid.length <- min(diff(box[c(1, 3)]),diff(box[c(2, 4)]))/grid.n


  grid <- st_make_grid(points, cellsize = grid.length, crs = crs,what = "polygons", square = !hex)
  grid <- st_sf(grid)
  grid.pts <- st_contains(grid, points)
  grid$Counts <- lengths(grid.pts)
  if(!is.null(Rj)) grid$Sum_Rj <- sapply(grid.pts, function(x) sum(points$Rj[x]))
  grid <- grid[grid$Counts>0,]
  if(!is.null(Rj)) grid$Mean_Rj <- grid$Sum_Rj/grid$Counts

  #change CRS
  grid <- st_transform(grid, 4326)

  # boundary
  if(is.null(bnd)) bnd <- st_bbox(grid)

  # basemap
  BaseMap <- CreateBaseMap(basemap,bnd,gridLonLat,interact)

  #plotting
  if(is.null(Rj)){
    GridMap <- tm_shape(grid, bbox = bnd) +
      tm_polygons(col = "Counts", n = 6, palette="YlOrRd", alpha = .6)+
      tm_layout(legend.outside = TRUE, legend.outside.position = "right")
  }else{
      GridMap <- tm_shape(grid, bbox = bnd) +
        tm_polygons(col = "Mean_Rj", n = 8, palette="Oranges", alpha = .8)+
        tm_layout(legend.outside = TRUE, legend.outside.position = "right")
  }

  Map <- BaseMap +
    GridMap +
    tm_layout (title = title,legend.format=list(text.separator="~")) +
    tm_scale_bar()

  if(!interact)  Map <- Map + tm_compass(position=c("left", "top"))

  return(Map)
}
