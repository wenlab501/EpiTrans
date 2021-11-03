#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_kde <- function(x, y, grid.n=50, bandwidth=NULL, crs = NULL, bnd = NULL, basemap = NULL, interact = TRUE, gridLonLat = TRUE, title = ""){
  require("sf")
  require("tmap")
  require("tmaptools")
  require("SpatialKDE")
  require("MASS")

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
    tm_polygons("KDE",palette=rev(heat.colors(10)),alpha=.8,n=8,border.alpha=0) +
    tm_layout(legend.outside = TRUE, legend.outside.position = "right")

  Map <- BaseMap +
    KDEMap +
    tm_layout (title = title,legend.format=list(text.separator="~")) +
    tm_scale_bar()

  if(!interact)  Map <- Map + tm_compass(position=c("left", "top"))

  return(Map)
}
