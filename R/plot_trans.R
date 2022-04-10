#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_trans=function(Rj, from = 1:200, to = 1:200,title=NULL){
  require("raster")
  require("Matrix")
  Mat = Matrix(Rj$Pij[from,to])
  col = c("white",hcl.colors(19, "YlOrRd", rev = TRUE))
  image(Mat, colorkey = T, col = col, lwd=0,main=title)
}
