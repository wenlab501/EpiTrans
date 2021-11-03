#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_hex <- function(x, y, Rj = NULL, grid.n=50, crs = NULL, bnd = NULL, basemap = NULL, interact = TRUE, gridLonLat = TRUE, title = ""){
  plot_grid(x, y, Rj, hex = T, grid.n, crs, bnd, basemap, interact, gridLonLat, title)
}
