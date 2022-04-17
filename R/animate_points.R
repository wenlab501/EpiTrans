#' Title
#'
#' Descriptions Here
#' @param x x
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

animate_points <- function(x, y, t, Rj = NULL, crs = NULL, bnd = NULL, basemap = NULL, interval=c("day","week","month"), gridLonLat = TRUE, width = 600, height = 400, delay = 50, title=""){
  require("sf")
  require("tmap")
  require("tmaptools")
  if (is.null(crs))
    crs <- 4326
  if (length(interval) > 2)
    interval <- interval[1]
  date.diff = as.numeric(max(t) - min(t))
  level.t <- min(t) + 0:date.diff
  if (interval == "week") {
    t <- strftime(t, "%Y-%W")
    level.t <- unique(strftime(level.t, "%Y-%W"))
  }
  else if (interval == "month") {
    t <- strftime(t, "%Y-%m")
    level.t <- unique(strftime(level.t, "%Y-%m"))
  }
  XY.data <- data.frame(x, y, t)
  if (!is.null(Rj))
    XY.data$Rj <- Rj
  points <- createPoints(XY.data, crs, basemap)
  points <- st_transform(points, 4326)
  points$t <- as.factor(points$t)
  levels(points$t) <- level.t
  if (is.null(bnd))
    bnd <- bnd_modify(points)
  BaseMap <- CreateBaseMap(basemap, bnd, gridLonLat, interact = FALSE,
                           alphaGrid = 0.1)
  if (is.null(Rj)) {
    PointsMap <- tm_shape(points, bbox = bnd) + tm_dots(col = "red",
                                                        size = 0.3, alpha = 0.3) + tm_facets(along = "t")
  }
  else {
    PointsMap <- tm_shape(points, bbox = bnd) + tm_symbols(size = "Rj",
                                                           col = "Rj", style = "quantile", border.col = NA,
                                                           border.alpha = 0.5, n = 10, palette = "YlOrRd",
                                                           alpha = 0.6, legend.size.show = FALSE, legend.col.show = FALSE) +
      tm_add_legend("symbol", paste0(0:9 * 10, "%~", 1:10 *
                                       10, "%"), get_brewer_pal("YlOrRd", 10, plot = FALSE),
                    1:10 * 0.1, border.col = NA, title = "Rj (quantile)") +
      tm_facets(along = "t")
  }
  Map <- BaseMap + PointsMap + tm_layout(title = title, legend.format = list(text.separator = "~")) +
    tm_scale_bar()
  if (is.null(Rj)) { Map <- Map + tm_compass(position = c("left", "top"))}
  else{ Map <- Map + tm_compass(position = c("right", "top"))}
  tmap_animation(Map, delay = delay)
}
