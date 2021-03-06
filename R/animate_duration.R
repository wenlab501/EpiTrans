#' Visualizing the spatio-temporal pattern of an outbreak’s transmision potential
#'
#' Generate animation of the spatial points of cases by day with a specific duration time
#'
#' @param x Vector of x coordinates of data points.
#' @param y Vector of y coordinates of data points.
#' @param t Vector of time of data points.
#' @param dt Day of the duration time.
#' @param Rj Vector of individual reproductive numbers of data points.
#' @param crs Coordinate reference system of data points: numeric (EPSG code), object of class 'crs', or input string for st_crs; if NULL, using CRS of basemap, or using WGS84 (EPSG:4326).
#' @param bnd A 4-length numerical vector of the form c(xmin, ymin, xmax, ymax) which gives the number of x and y coordinates' ranges of the map.
#' @param basemap Object of class 'sf'; basemap plotting on map.
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
#' animate_duration(x = dengue$long, y = dengue$lat,t = dengue$date,dt = 7,Rj = dengue$Rj, basemap = Taiwan)
#'
#' @import sf
#' @import tmap
#' @import tmaptools
#' @export

animate_duration <-
  function (x,
            y,
            t,
            dt = 7,
            Rj = NULL,
            crs = NULL,
            bnd = NULL,
            basemap = NULL,
            gridLonLat = TRUE,
            width = 600,
            height = 400,
            delay = 20,
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
    date.diff = as.numeric(max(t) - min(t))
    level.t <- min(t) + 0:date.diff

    XY.data <- data.frame(x, y, t)
    if (!is.null(Rj))
      XY.data$Rj <- Rj
    points <- createPoints(XY.data, crs, basemap)
    points <- st_transform(points, 4326)
    points$start <- points$t
    points$end <- points$t + dt

    if (is.null(bnd))
      bnd <- bnd_modify(points)
    BaseMap <-
      CreateBaseMap(basemap,
                    bnd,
                    gridLonLat,
                    interact = FALSE,
                    alphaGrid = 0.1)

    if (!is.null(Rj)) {
      Q = quantile(points$Rj, 0:10 * 0.1)[1:10]
      points$Rv = mapply(function(x)
        sum(x >= Q), points$Rj)
    }

    Map.fun <- function(x) {
      if (is.null(Rj)) {
        select <- points[points$start <= x & points$end >= x, ]
        PointsMap <- tm_shape(select, bbox = bnd) +
          tm_dots(col = "red",
                  size = 0.3,
                  alpha = 0.3) +
          tm_layout(title = as.character(x),
                    legend.format = list(text.separator = "~")) +
          tm_scale_bar() + tm_compass(position = c("left", "top"))
      }
      else {
        select <- points[points$start <= x & points$end >= x, ]
        PointsMap <- tm_shape(select, bbox = bnd) +
          tm_symbols(
            size = "Rv",
            col = "Rv",
            breaks = 0:10,
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
            paste0(0:9 * 10, "%~", 1:10 * 10, "%"),
            get_brewer_pal("YlOrRd", 10, plot = FALSE),
            1:10 * 0.1,
            border.col = NA,
            title = "Rj (quantile)"
          ) +
          tm_layout(
            legend.outside = T,
            title = as.character(x),
            legend.format = list(text.separator = "~")
          ) +
          tm_scale_bar() + tm_compass(position = c("right", "top"))
      }
      BaseMap + PointsMap
    }
    map_animate <- lapply(level.t, Map.fun)
    tmap_animation(map_animate,
                   width = width,
                   height = height,
                   delay = delay)
  }
