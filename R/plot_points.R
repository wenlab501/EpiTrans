#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_points <- function(x, y, t = NULL, Rj = NULL, crs = NULL, bnd = NULL, basemap = NULL, interact = TRUE, gridLonLat = TRUE, title = ""){
  require("sf")
  require("tmap")
  require("tmaptools")

  if(interact) tmap_mode("view") else tmap_mode("plot")
  if(is.null(crs)) crs <- 4326

  # create points
  XY.data <- data.frame(x, y)
  if(!is.null(t)) XY.data$t <- as.numeric(t)
  if(!is.null(Rj)) XY.data$Rj <- Rj
  points <- createPoints(XY.data, crs, basemap)
  points <- st_transform(points, 4326)

  # boundary
  if(is.null(bnd)) bnd <- bnd_modify(points)

  # basemap
  BaseMap <- CreateBaseMap(basemap,bnd,gridLonLat,interact)

  #plotting
  if(is.null(t) & is.null(Rj)){
    PointsMap <- tm_shape(points, bbox = bnd) +
      tm_dots(col = "red", size = ifelse(interact, .05, .3), alpha = .3)
  }else if(!is.null(Rj)){
    if(interact){
      PointsMap <- tm_shape(points, bbox = bnd) +
        tm_dots(col = "Rj", size = "Rj", style="quantile", n = 10, palette="YlOrRd", alpha = .6, scale = .8, legend.size.show = FALSE, legend.show = FALSE)+
        tm_add_legend("fill", paste0(0:9*10, "%~", 1:10*10, "%"), get_brewer_pal("YlOrRd", 10, plot = FALSE), title ="Rj (quantile)")
    }else{
      PointsMap <- tm_shape(points, bbox = bnd) +
        tm_symbols(size = "Rj", col = "Rj", style="quantile", border.col = NA, border.alpha = .5, n = 10, palette="YlOrRd", alpha = .6, legend.size.show = FALSE, legend.col.show = FALSE)+
        tm_add_legend("symbol", paste0(0:9*10, "%~", 1:10*10, "%"), get_brewer_pal("YlOrRd", 10, plot = FALSE), 1:10*0.1, border.col = NA, title ="Rj (quantile)")+
        tm_layout(outer.margins = .05, legend.outside = TRUE, legend.outside.position = "right")
    }
  }else{
    ts=seq(min(t),max(t),length.out = 11)
    if(interact){
      PointsMap <- tm_shape(points, bbox = bnd) +
        tm_dots(col = "t", size = .05, n = 10, palette = rainbow(10), alpha = .6, legend.show = FALSE) +
        tm_add_legend("fill", paste0(ts[1:10], "~", ts[2:11]), rainbow(10), title = "Date",alpha = .6)
    }else{
      PointsMap <- tm_shape(points, bbox = bnd) +
        tm_dots(col = "t", size = .3, alpha = .3, n = 10, palette = rainbow(10), legend.show = FALSE) +
        tm_add_legend("symbol", paste0(ts[1:10], "~", ts[2:11]), rainbow(10), title = "Date",alpha = .6)+
        tm_layout(outer.margins = .05, legend.outside = TRUE, legend.outside.position = "right")
    }
  }


  Map <- BaseMap +
    PointsMap +
    tm_layout (title = title,legend.format=list(text.separator="~")) +
    tm_scale_bar()

  if(!interact)  Map <- Map + tm_compass(position=c("left", "top"))

  return(Map)
}
