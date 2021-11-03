#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_grid <- function(x, y, Rj = NULL, hex=F, grid.n=50, crs = NULL, bnd = NULL, basemap = NULL, interact = TRUE, gridLonLat = TRUE, title = ""){
  require("sf")
  require("tmap")
  require("tmaptools")

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
