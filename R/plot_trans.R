#' Visualize pair-wise transmission likelihood
#'
#' Plot the individual transmission probablity matrix
#'
#' @param Rj Input the result of 'Rj'; a list must contain a matrix of individual transmission probablity from case i to case j called 'Pij'.
#' @param from Origin of transmission, y-axis on the plot.
#' @param to Destination of transmission, x-axis on the plot.
#' @param title Main title of plot.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' res_adj = Rj(t = dengue$date, x = dengue$long, y = dengue$lat,GI.pdf = lpdf_GI(),SW.pdf = lpdf_SW(), adjSP = TRUE)
#' res = Rj(t = dengue$date, GI.pdf = lpdf_GI(), adjSP = FALSE)
#'
#' plot_trans(res, from = 1:200, to = 1:200)
#' plot_trans(res_adj, from = 1:200, to = 1:200)
#' @import Matrix
#' @import RColorBrewer
#' @export

plot_trans=function(Rj, from = 1:200, to = 1:200,title=NULL){
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop(
      "Package \"Matrix\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop(
      "Package \"RColorBrewer\" must be installed to use this function.",
      call. = FALSE
    )
  }

  Mat = Matrix(Rj$Pij[from,to])
  col = c("white",hcl.colors(19, "YlOrRd", rev = TRUE))
  image(Mat, colorkey = T, col = col, lwd=0,main=title)
}
