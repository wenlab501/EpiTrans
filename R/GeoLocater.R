#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

GeoLocater <- function(TimeDF,PointsDF) {
  require("dplyr")
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
