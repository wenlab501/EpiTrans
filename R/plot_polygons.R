#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_polygons=function(region, sf, interact = TRUE, gridLonLat = TRUE){
  require("sf")
  require("tmap")
  require("tmaptools")
  require("dplyr")

  if(interact) tmap_mode("view") else tmap_mode("plot")

  sf <- left_join(sf,data.frame(xtabs(~region)),by="region")
  sf <- st_transform(sf,4326)
  sf$Counts <- sf$Freq

  Map <- tm_layout (bg.color = "#D2E9FF")
  if(gridLonLat & !interact) Map <- Map + tm_grid(col = "white")

  Map <- Map+
    tm_shape(sf) +
    tm_polygons(col = "Counts",palette =rev(heat.colors(10)), n=7, alpha = ifelse(interact,.7,1), colorNA="white", textNA="No Case")+
    tm_layout(legend.outside = TRUE, legend.outside.position = "right", legend.format=list(text.separator="~"))

  return(Map)
}
