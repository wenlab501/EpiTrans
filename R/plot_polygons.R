#' Draw Choropleth map
#'
#' Plot chorolepth map of cases by grouping region
#'
#' @param region Vector of region (polygon) name of data points.
#' @param sf Object of class 'sf'; polygon layer of statistical data (region data of these data points).
#' @param interact Logical; if TRUE, plot thematic interactive map; if FALSE, static plotting non-interactive thematic map.
#' @param gridLonLat Logical; draw latitude and longitude grid on non-interactive map.
#'
#' @examples
#' data("EpiTrans")
#'
#' covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
#'
#' plot_polygons(region = covid$region,sf = Taipei)
#' @import dplyr
#' @import sf
#' @import tmap
#' @import tmaptools
#' @export

plot_polygons=function(region, sf, interact = TRUE, gridLonLat = TRUE){
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
