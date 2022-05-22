#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

animate_duration <- function (x, y, t, dt = 7, Rj = NULL,crs = NULL, bnd = NULL, basemap = NULL, gridLonLat = TRUE, width = 600, height = 400, delay = 20, title = "") {
  require("sf")
  require("tmap")
  require("tmaptools")
  if (is.null(crs)) crs <- 4326
  date.diff = as.numeric(max(t) - min(t))
  level.t <- min(t) + 0:date.diff

  XY.data <- data.frame(x, y, t)
  if (!is.null(Rj)) XY.data$Rj <- Rj
  points <- createPoints(XY.data, crs, basemap)
  points <- st_transform(points, 4326)
  points$start <- points$t
  points$end <- points$t +dt

  if (is.null(bnd)) bnd <- bnd_modify(points)
  BaseMap <- CreateBaseMap(basemap, bnd, gridLonLat, interact = FALSE, alphaGrid = 0.1)

  if(!is.null(Rj)){
    Q=quantile(points$Rj,0:10*0.1)[1:10]
    points$Rv=mapply(function(x) sum(x>=Q),points$Rj)
  }

  Map.fun <- function(x) {
    if(is.null(Rj)){
      select <- points[points$start<=x&points$end>=x,]
      PointsMap <- tm_shape(select, bbox = bnd) +
        tm_dots(col = "red",size = 0.3, alpha = 0.3) +
        tm_layout(title = as.character(x), legend.format = list(text.separator = "~")) +
        tm_scale_bar()+ tm_compass(position = c("left", "top"))
    }
    else {
      select <- points[points$start<=x&points$end>=x,]
      PointsMap <- tm_shape(select, bbox = bnd) +
        tm_symbols(size = "Rv",col = "Rv", breaks=0:10, border.col = NA,border.alpha = 0.5, n = 10, palette = "YlOrRd",alpha = 0.6, legend.size.show = FALSE, legend.col.show = FALSE) +
        tm_add_legend("symbol", paste0(0:9 * 10, "%~",1:10 * 10, "%"), get_brewer_pal("YlOrRd",10, plot = FALSE), 1:10 * 0.1, border.col = NA,title = "Rj (quantile)") +
        tm_layout(legend.outside=T,title = as.character(x), legend.format = list(text.separator = "~")) +
        tm_scale_bar()+ tm_compass(position = c("right", "top"))
    }
    BaseMap+PointsMap
  }
  map_animate <- lapply(level.t, Map.fun)
  tmap_animation(map_animate, width = width, height = height,delay = delay)
}
