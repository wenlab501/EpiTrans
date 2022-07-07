#' Calculate the number of cases.
#'
#' Convert polygon data to point coordinates, random sampling and locating to get the cases points' coordinates
#'
#' @param TimeDF Data frame of points regarding time;must contain a column of place (region, polygon name) called 'region', and a column of time called 'time'.
#' @param PointsDF Data frame (regarding space) of population points or random points in polygons; must contain a column of place (region, polygon name) called 'region', and a column of x coordinate called 'x', and a column of y coordinate called 'y'.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' GeoLocater(TimeDF = covid19,PointsDF = RndPts)
#' @import dplyr
#' @export

GeoLocater <- function(TimeDF,PointsDF) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package \"dplyr\" must be installed to use this function.",
      call. = FALSE
    )
  }

  names(TimeDF)[1] <- "region"
  names(PointsDF)[1] <- "region"
  TimeDF$TID <- SequentialNumbering(TimeDF$region)
  PointsDF$PID <- SequentialNumbering(PointsDF$region)
  region.df <- data.frame(xtabs(~region,TimeDF))
  region.df <- left_join(region.df,data.frame(xtabs(~region,PointsDF)),by="region")
  RndID <- unlist(apply(region.df, 1, function(x) paste0(x[1],sample(as.numeric(x[3]),as.numeric(x[2])))))
  Locate <- data.frame(TID=sort(TimeDF$TID),PID=RndID)
  Locate <- left_join(Locate,PointsDF,by="PID")
  Locate <- left_join(TimeDF,Locate,by=c("TID","region"))
  delcol  <-  names(Locate) %in% c("TID","PID")
  return(Locate[,!delcol])
}
