#' Visualizing Point pattern
#'
#' Plot the spatial pattern of cases by KDE method; smoothed overview of the clustering pattern of an outbreak
#'
#' @param x Vector of x coordinates of data points.
#' @param y Vector of y coordinates of data points.
#' @param grid.n Number of grid cells in one direction (shorter side).
#' @param bandwidth Specifing the band width for KDE.
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
#' plot_kde(x = dengue$long, y= dengue$lat, grid.n = 30, basemap = Taiwan)
#' @import MASS
#' @import sf
#' @import SpatialKDE
#' @import tmap
#' @import tmaptools
#' @export

plot_kde <- function(x, y, grid.n=50, bandwidth=NULL, crs = NULL, bnd = NULL, basemap = NULL, interact = TRUE, gridLonLat = TRUE, title = ""){
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
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop(
      "Package \"MASS\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("SpatialKDE", quietly = TRUE)) {
    stop(
      "Package \"SpatialKDE\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if(interact) tmap_mode("view") else tmap_mode("plot")
  if(is.null(crs)) crs <- 4326

  # create points
  XY.data <- data.frame(x, y)
  points <- createPoints(XY.data, crs = crs, basemap)
  points <- st_transform(points, 3857)

  # create grids
  box <- st_bbox(points)
  grid.length=min(diff(box[c(1, 3)]),diff(box[c(2, 4)]))/grid.n
  grid <- st_make_grid(points, cellsize = grid.length)
  grid <- st_sf(grid)

  # calculate kde
  coord=st_coordinates(points$geometry)
  if(is.null(bandwidth)) bandwidth <- (bandwidth.nrd(coord[,1])+bandwidth.nrd(coord[,2]))/2
  kde <- kde(points,bandwidth,grid=grid)
  kde <- kde[kde$kde_value>0,]
  kde$KDE <- round(kde$kde_value)

  #change CRS
  kde <- st_transform(kde, 4326)

  # boundary
  if(is.null(bnd)) bnd <- bnd_modify(points)

  # basemap
  BaseMap <- CreateBaseMap(basemap,bnd,gridLonLat,interact)

  #plotting
  KDEMap <- tm_shape(kde) +
    tm_polygons("KDE",palette=RColorBrewer::brewer.pal(9,"Reds"),alpha=.8,n=8,border.alpha=0) +
    tm_layout(legend.outside = TRUE, legend.outside.position = "right")

  Map <- BaseMap +
    KDEMap +
    tm_layout (title = title,legend.format=list(text.separator="~")) +
    tm_scale_bar()

  if(!interact)  Map <- Map + tm_compass(position=c("left", "top"))

  return(Map)
}
