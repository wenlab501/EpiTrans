#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

animate_polygons <- function(region, t, sf, interval=c("day","week","month"), breaks = NULL, gridLatLong = TRUE, width = 600, height = 400, delay = 50){
  require("sf")
  require("tmap")
  require("tmaptools")
  require("dplyr")
  require("reshape2")

  if(length(interval)>2) interval <- interval[1]
  if(interval=="week") t=strftime(t,"%Y-%W")
  else if(interval=="month") t=strftime(t,"%Y-%m")

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
