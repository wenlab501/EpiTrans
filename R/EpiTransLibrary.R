#' Pre-run package dependencies
#'
#' Pre-run package dependencies of EpiTrans
#' 
#' @export
#' @examples
#' EpiTransLibrary()

EpiTransLibrary <- function() {
  library("Matrix")
  library("ggplot2")
  library("sf")
  library("gganimate")
  library("gifski")
  library("png")
  library("transformr")
  library("dplyr")
  library("tmap")
  library("reshape2")
  library("stplanr")
  library("MASS")
  library("tmaptools")
  library("SpatialKDE")
}
