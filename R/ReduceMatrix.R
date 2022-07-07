#' Reduce individual transmission probablity matrix to region transmission matrix
#'
#' Reduce individual transmission probablity matrix to region transmission matrix
#'
#' @param matrix Matrix of individual transmission probablity from case i to case j
#' @param group Vector of grouop, or region(polygon) of cases
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
#' covid.R_adj=Rj(t = covid$date, x = covid$x, y = covid$y, GI.pdf = lpdf_GI(5.2,1.5), unit_coord = "meter")
#'
#' Tij = ReduceMatrix(covid.R_adj$Pij,covid$region)
#' @export

ReduceMatrix <- function(matrix,group){
  ReM <- function(x) tapply(x,group,function(t) sum(t,na.rm=T))
  return(apply(apply(matrix,1,ReM),1,ReM))
}
