#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

animate_grid <- function(x, y, t, Rj = NULL,hex = FALSE, grid.n=50, crs = NULL, bnd = NULL, basemap = NULL, interval=c("day","week","month"), gridLonLat = TRUE, width = 600, height = 400, delay = 50, title=""){
  require("sf")
  require("tmap")
  require("tmaptools")

  if(is.null(crs)) crs <- 4326

  if(length(interval)>2) interval <- interval[1]
  date.diff=as.numeric(max(t)-min(t))
  level.t <- min(t)+0:date.diff
  if(interval=="week"){
    t <- strftime(t,"%Y-%W")
    level.t <- unique(strftime(level.t,"%Y-%W"))
  }else if(interval=="month"){
    t <- strftime(t,"%Y-%m")
    level.t <- unique(strftime(level.t,"%Y-%m"))
  }

  # create points
  XY.data <- data.frame(x, y, t)
  if(!is.null(Rj)) XY.data$Rj <- Rj
  points <- createPoints(XY.data, crs, basemap)
  points <- st_transform(points, 4326)
  points$t <- as.factor(points$t)
  levels(points$t) <- level.t

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
  if(is.null(bnd)) bnd <- bnd_modify(points)

  # basemap
  BaseMap <- CreateBaseMap(basemap,bnd,gridLonLat,interact = FALSE, alphaGrid =.1)

  #plotting
  if(is.null(Rj)){
    GridMap <- tm_shape(grid, bbox = bnd) +
      tm_polygons(col = "Counts", n = 6, palette="YlOrRd", alpha = .6)+
      tm_layout(legend.outside = TRUE, legend.outside.position = "right")+
      tm_facets(along = "t")
  }else{
    GridMap <- tm_shape(grid, bbox = bnd) +
      tm_polygons(col = "Mean_Rj", n = 8, palette="Oranges", alpha = .8)+
      tm_layout(legend.outside = TRUE, legend.outside.position = "right")
  }

  Map <- BaseMap +
    GridMap +
    tm_layout (title = title,legend.format=list(text.separator="~")) +
    tm_scale_bar()


  tmap_animation(Map, delay = delay)
}
