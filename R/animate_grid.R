#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

animate_grid <- function(x, y, t, Rj = NULL,hex = FALSE, grid.n=50, crs = NULL, bnd = NULL, basemap = NULL, interval=c("day","week","month"), gridLonLat = TRUE, width = 600, height = 400, delay = 50, title="", n.breaks=6){
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
  points$t <- as.factor(points$t)
  levels(points$t) <- level.t

  # create grids
  box <- st_bbox(points)
  grid.length <- min(diff(box[c(1, 3)]),diff(box[c(2, 4)]))/grid.n
  grid <- st_make_grid(points, cellsize = grid.length, crs = crs,what = "polygons", square = !hex)
  grid <- st_sf(grid)

  if(is.null(Rj)){
    for(i in level.t) {
      count=lengths(st_contains(grid, points[points$t==i,]))
      count[count==0]=NA
      grid[i]=count
    }
  }else{
    for(i in level.t) {
      grid.pts=st_contains(grid, points[points$t==i,])
      count = lengths(st_contains(grid, points[points$t==i,]))
      sum = sapply(grid.pts, function(x) sum(points$Rj[x]))
      mean=sum/count
      mean[count==0]=NA
      grid[i]=mean
    }
  }

  #change CRS
  grid <- st_transform(grid, 4326)

  # boundary
  if(is.null(bnd)) bnd <- bnd_modify(points)

  # basemap
  BaseMap <- CreateBaseMap(basemap,bnd,gridLonLat,interact = FALSE, alphaGrid =.1)

  #
  grid.df = st_drop_geometry(grid)
  grid.max = max(grid.df,na.rm=T)
  breaks = ceiling(grid.max/n.breaks)*0:n.breaks

  #plotting
  Map.fun <- function(x) {
    if(is.null(Rj)){
      GridMap <- tm_shape(grid, bbox = bnd) +
        tm_polygons(col = x, title = "Counts",border.col="NA",breaks = breaks,n = n.breaks, palette="YlOrRd", alpha = .6,colorNA ="NA",showNA=F)+
        tm_layout(x,legend.format=list(text.separator="~"))
    }else{
      GridMap <- tm_shape(grid, bbox = bnd) +
        tm_polygons(col = x, title = "Mean_Rj",border.col="NA",breaks = breaks,n = n.breaks, palette="YlOrRd", alpha = .6,colorNA ="NA",showNA=F)+
        tm_layout(x,legend.format=list(text.separator="~"))
    }
    BaseMap+GridMap
  }

  map_animate  <-  lapply(level.t, Map.fun)
  tmap_animation(map_animate, width = width, height = height,delay = delay)
}

