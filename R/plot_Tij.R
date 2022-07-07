#' Visualizing Town Level Reproductive Numbers
#'
#' Plot regional transmission relationship map by transmission probablity matrix
#'
#' @param Tij Matrix of transmission probablity between regions
#' @param sf Object of class 'sf'; polygons of region data
#' @return
#'

#' @examples
#' data("EpiTrans")
#'
#' covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
#' covid.R_adj=Rj(t = covid$date, x = covid$x, y = covid$y, GI.pdf = lpdf_GI(5.2,1.5), unit_coord = "meter")
#'
#' Tij = ReduceMatrix(covid.R_adj$Pij,covid$region)
#'
#' plot_Tij(Tij = Tij, sf = Taipei)
#' @import dplyr
#' @import reshape2
#' @import sf
#' @import stplanr
#' @import tmap
#' @import tmaptools
#' @export

plot_Tij <- function(Tij,sf) {
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
  if (!requireNamespace("stplanr", quietly = TRUE)) {
    stop(
      "Package \"stplanr\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop(
      "Package \"reshape2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  OD=melt(Tij,c("from","to"),value.name = "Tij")
  OD=subset(OD,from!=to & Tij>1)
  OD=od2line(OD,sf,zone_code='region')
  OD$NAME=paste0(OD$from,"to",OD$to)
  OD=OD[,c("NAME",names(OD))]

  center=st_centroid(sf)
  center=inner_join(center,data.frame(Tij=diag(Tij),region=rownames(Tij)),by="region")

  tmap_mode("view")
  tm_shape(sf)+tm_polygons()+
    tm_shape(OD)+tm_lines(lwd="Tij",col ="Tij",legend.col.show = F,breaks = c(0:6)*10,scale = max(OD$Tij)/3)+
    tm_shape(center)+tm_symbols("Tij",col ="Tij",palette="Reds",legend.col.show = F)
}
