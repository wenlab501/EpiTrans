#' Visualizing the spatio-temporal pattern of choropleth map
#'
#' Generate animation of chorolepth map of cases by grouping region
#'
#' @param region Vector of region (polygon) name of data points.
#' @param t Vector of time of data points.
#' @param sf Object of class 'sf'; polygon layer of statistical data (region data of these data points).
#' @param interval Character; time resolution(day, week, month) of animation ; aggregrate data through day, week or month.
#' @param breaks Numeric vector to tell how chorolepth classify.
#' @param gridLonLat Logical; draw latitude and longitude grid on non-interactive map.
#' @param width Width of the animation file (in pixels).
#' @param height Height of the animation file (in pixels).
#' @param delay Delay time between images (in 1/100th of a second).
#' @return
#'

#' @examples
#' data("EpiTrans")
#' covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
#'
#' animate_polygons(region = covid$region,t = covid$date,sf = Taipei)
#' @import dplyr
#' @import reshape2
#' @import sf
#' @import tmap
#' @import tmaptools
#' @export
#'

animate_polygons <- function(region, t, sf, interval=c("day","week","month"), breaks = NULL, gridLonLat = TRUE, width = 600, height = 400, delay = 50){

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
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package \"dplyr\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop(
      "Package \"reshape2\" must be installed to use this function.",
      call. = FALSE
    )
  }


  if(length(interval)>2) interval <- interval[1]
  date.diff=as.numeric(max(t)-min(t))
  level.t <- min(t)+0:date.diff
  if(interval=="week"){
    t <- strftime(t,"%Y-%W")
    level.t <- unique(strftime(level.t,"%Y-%W"))
  }else if(interval=="mXonth"){
    t <- strftime(t,"%Y-%m")
    level.t <- unique(strftime(level.t,"%Y-%m"))
  }
  t <- as.factor(t)
  levels(t) <- level.t

  region <- factor(region,levels=sf$region)
  df <- as.data.frame.matrix(xtabs(~region+t))
  maxdf <- max(df)
  time <- colnames(df)
  df$region <- rownames(df)
  sf <- left_join(sf,df,by="region")
  sf <- st_transform(sf,4326)
  if(is.null(breaks)) breaks <- c(0,round(exp(seq(0,log(maxdf),length.out = 10)))[c(-1:-3)])

  Map.fun <- function(x) {
    tm_layout (bg.color = "#D2E9FF") +
      tm_grid(col = "white") +
      tm_shape(sf) +
      tm_polygons(col = x, breaks = breaks, palette = "OrRd",n = length(breaks)-1)+
      tm_layout(legend.outside = TRUE, legend.outside.position = "right",legend.format=list(text.separator="~"))
  }

  map_animate  <-  lapply(time, Map.fun)
  tmap_animation(map_animate, width = width, height = height, delay = delay)
}
