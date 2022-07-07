#' Visualizing the spatio-temporal pattern of an outbreak’s transmision potential
#'
#' Generate animation of the spatial points of cases; point pattern is the most direct and exact way to visualize spatial pattern of an outbreak.
#'
#' @param x Vector of x coordinates of data points.
#' @param y Vector of y coordinates of data points.
#' @param t Vector of time of data points.
#' @param Rj Vector of individual reproductive numbers of data points.
#' @param crs Coordinate reference system of data points: numeric (EPSG code), object of class 'crs', or input string for st_crs; if NULL, using CRS of basemap, or using WGS84 (EPSG:4326).
#' @param bnd A 4-length numerical vector of the form c(xmin, ymin, xmax, ymax) which gives the number of x and y coordinates' ranges of the map.
#' @param basemap Object of class 'sf'; basemap plotting on map.
#' @param interval Character; time resolution(day, week, month) of animation ; aggregrate data through day, week or month.
#' @param gridLonLat Logical; draw latitude and longitude grid on non-interactive map.
#' @param width Width of the animation file (in pixels).
#' @param height Height of the animation file (in pixels).
#' @param delay Delay time between images (in 1/100th of a second).
#' @param title Main title of animation.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' res_adj = Rj(t = dengue$date, x = dengue$long, y = dengue$lat,GI.pdf = lpdf_GI(),SW.pdf = lpdf_SW(), adjSP = TRUE)
#' dengue$Rj = res_adj$Rj
#' dengue = dengue[dengue$date > as.Date("2007-09-15") & dengue$date < as.Date("2007-10-15"),]
#'
#' animate_points(t = dengue$date, x = dengue$long, y= dengue$lat, Rj = dengue$Rj, interval = "week",basemap = Taiwan)

#' @import sf
#' @import tmap
#' @import tmaptools
#' @export

animate_points <-
  function(x,
           y,
           t,
           Rj = NULL,
           crs = NULL,
           bnd = NULL,
           basemap = NULL,
           interval = c("day", "week", "month"),
           gridLonLat = TRUE,
           width = 600,
           height = 400,
           delay = 50,
           title = "") {
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
    if (is.null(crs))
      crs <- 4326
    if (length(interval) > 2)
      interval <- interval[1]
    date.diff = as.numeric(max(t) - min(t))
    level.t <- min(t) + 0:date.diff
    if (interval == "week") {
      t <- strftime(t, "%Y-%W")
      level.t <- unique(strftime(level.t, "%Y-%W"))
    }
    else if (interval == "month") {
      t <- strftime(t, "%Y-%m")
      level.t <- unique(strftime(level.t, "%Y-%m"))
    }
    XY.data <- data.frame(x, y, t)
    if (!is.null(Rj))
      XY.data$Rj <- Rj
    points <- createPoints(XY.data, crs, basemap)
    points <- st_transform(points, 4326)
    points$t <- as.factor(points$t)
    levels(points$t) <- level.t
    if (is.null(bnd))
      bnd <- bnd_modify(points)
    BaseMap <-
      CreateBaseMap(basemap,
                    bnd,
                    gridLonLat,
                    interact = FALSE,
                    alphaGrid = 0.1)
    if (is.null(Rj)) {
      PointsMap <- tm_shape(points, bbox = bnd) + tm_dots(col = "red",
                                                          size = 0.3,
                                                          alpha = 0.3) + tm_facets(along = "t")
    }
    else {
      PointsMap <- tm_shape(points, bbox = bnd) + tm_symbols(
        size = "Rj",
        col = "Rj",
        style = "quantile",
        border.col = NA,
        border.alpha = 0.5,
        n = 10,
        palette = "YlOrRd",
        alpha = 0.6,
        legend.size.show = FALSE,
        legend.col.show = FALSE
      ) +
        tm_add_legend(
          "symbol",
          paste0(0:9 * 10, "%~", 1:10 *
                   10, "%"),
          get_brewer_pal("YlOrRd", 10, plot = FALSE),
          1:10 * 0.1,
          border.col = NA,
          title = "Rj (quantile)"
        ) +
        tm_facets(along = "t")
    }
    Map <-
      BaseMap + PointsMap + tm_layout(title = title,
                                      legend.format = list(text.separator = "~")) +
      tm_scale_bar()
    if (is.null(Rj)) {
      Map <- Map + tm_compass(position = c("left", "top"))
    }
    else{
      Map <- Map + tm_compass(position = c("right", "top"))
    }
    tmap_animation(Map, delay = delay)
  }
