#' Visualizing the spatio-temporal pattern of an outbreak’s transmision potential
#'
#' Generate animation of the spatial pattern of cases by grid; aggregate the point into specified grids
#'
#' @param x Vector of x coordinates of data points.
#' @param y Vector of y coordinates of data points.
#' @param t Vector of time of data points.
#' @param Rj Vector of individual reproductive numbers of data points.
#' @param hex Logical; if'TRUE', create hexagonal grid ; if'FALSE', create square grid.
#' @param grid.n Number of grid cells in one direction (shorter side).
#' @param crs Coordinate reference system of data points: numeric (EPSG code), object of class 'crs', or input string for st_crs; if NULL, using CRS of basemap, or using WGS84 (EPSG:4326).
#' @param bnd A 4-length numerical vector of the form c(xmin, ymin, xmax, ymax) which gives the number of x and y coordinates' ranges of the map.
#' @param basemap Object of class 'sf'; basemap plotting on map.
#' @param interval Character; time resolution(day, week, month) of animation ; aggregrate data through day, week or month.
#' @param gridLonLat Logical; draw latitude and longitude grid on non-interactive map.
#' @param width Width of the animation file (in pixels).
#' @param height Height of the animation file (in pixels).
#' @param delay Delay time between images (in 1/100th of a second).
#' @param title Main title of animation.
#' @param n.breaks Number of classify classes.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' res_adj = Rj(t = dengue$date, x = dengue$long, y = dengue$lat,GI.pdf = lpdf_GI(),SW.pdf = lpdf_SW(), adjSP = TRUE)
#' dengue$Rj = res_adj$Rj
#' dengue = dengue[dengue$date > as.Date("2007-09-15") & dengue$date < as.Date("2007-10-15"),]
#'
#'
#' animate_grid(x=dengue$long, y=dengue$lat, t=dengue$date, Rj = dengue$Rj, grid.n=20, hex = TRUE, basemap = Taiwan, interval="week")
#' @import sf
#' @import tmap
#' @import tmaptools
#' @export

animate_grid <-
  function(x,
           y,
           t,
           Rj = NULL,
           hex = FALSE,
           grid.n = 50,
           crs = NULL,
           bnd = NULL,
           basemap = NULL,
           interval = c("day", "week", "month"),
           gridLonLat = TRUE,
           width = 600,
           height = 400,
           delay = 50,
           title = "",
           n.breaks = 6) {
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
    } else if (interval == "month") {
      t <- strftime(t, "%Y-%m")
      level.t <- unique(strftime(level.t, "%Y-%m"))
    }

    # create points
    XY.data <- data.frame(x, y, t)
    if (!is.null(Rj))
      XY.data$Rj <- Rj
    points <- createPoints(XY.data, crs, basemap)
    points$t <- as.factor(points$t)
    levels(points$t) <- level.t

    # create grids
    box <- st_bbox(points)
    grid.length <- min(diff(box[c(1, 3)]), diff(box[c(2, 4)])) / grid.n
    grid <-
      st_make_grid(
        points,
        cellsize = grid.length,
        crs = crs,
        what = "polygons",
        square = !hex
      )
    grid <- st_sf(grid)

    if (is.null(Rj)) {
      for (i in level.t) {
        count = lengths(st_contains(grid, points[points$t == i, ]))
        count[count == 0] = NA
        grid[i] = count
      }
    } else{
      for (i in level.t) {
        grid.pts = st_contains(grid, points[points$t == i, ])
        count = lengths(st_contains(grid, points[points$t == i, ]))
        sum = sapply(grid.pts, function(x)
          sum(points$Rj[x]))
        mean = sum / count
        mean[count == 0] = NA
        grid[i] = mean
      }
    }

    #change CRS
    grid <- st_transform(grid, 4326)

    # boundary
    if (is.null(bnd))
      bnd <- bnd_modify(points)

    # basemap
    BaseMap <-
      CreateBaseMap(basemap,
                    bnd,
                    gridLonLat,
                    interact = FALSE,
                    alphaGrid = .1)

    #
    grid.df = st_drop_geometry(grid)
    grid.max = max(grid.df, na.rm = T)
    breaks = ceiling(grid.max / n.breaks) * 0:n.breaks

    #plotting
    Map.fun <- function(x) {
      if (is.null(Rj)) {
        GridMap <- tm_shape(grid, bbox = bnd) +
          tm_polygons(
            col = x,
            title = "Counts",
            border.col = "NA",
            breaks = breaks,
            n = n.breaks,
            palette = "YlOrRd",
            alpha = .6,
            colorNA = "NA",
            showNA = F
          ) +
          tm_layout(x, legend.format = list(text.separator = "~"))
      } else{
        GridMap <- tm_shape(grid, bbox = bnd) +
          tm_polygons(
            col = x,
            title = "Mean_Rj",
            border.col = "NA",
            breaks = breaks,
            n = n.breaks,
            palette = "YlOrRd",
            alpha = .6,
            colorNA = "NA",
            showNA = F
          ) +
          tm_layout(x, legend.format = list(text.separator = "~"))
      }
      BaseMap + GridMap
    }

    map_animate  <-  lapply(level.t, Map.fun)
    tmap_animation(map_animate,
                   width = width,
                   height = height,
                   delay = delay)
  }
