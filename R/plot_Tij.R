#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_Tij <- function(Tij,sf) {
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
