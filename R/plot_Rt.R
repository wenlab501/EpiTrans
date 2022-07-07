#' Visualizing Time-varying reproductive numbers
#'
#' Plot line chart of individual reproductive numbers by time
#'
#' @param t Vector of time of data points.
#' @param Rt Vector of individual reproductive numbers of data points, stand for Rt1.
#' @param Rt2 Another vector of individual reproductive numbers of data points to compare, stand for Rt2.
#' @param percentile A 2-length numerical vector, draw the area of value of percentile range; if c(0,1) means min and max will be drawn, c(0.05,0.95) means the middle 90 percent value will be drawn.
#' @param interval Character; time resolution(day, week, month) of animation ; aggregrate data through day, week or month.
#' @param col Color of line Rt1.
#' @param col2 Color of line Rt2.
#' @param label Label of line Rt1.
#' @param label2 Label of line Rt2.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
#' covid.R_adj=Rj(t = covid$date, x = covid$x, y = covid$y, GI.pdf = lpdf_GI(5.2,1.5), unit_coord = "meter")
#'
#' plot_Rt(t = covid$date, Rt=covid.R_adj$Rj, Rt2=covid.R_adj$Rj,percentile = c(.025,.975))
#' @import ggplot2
#' @export

plot_Rt <- function(t, Rt, Rt2=NULL, percentile=c(0,1), interval = c("day","week","month"), col="orange",col2="darkgreen",label="Rt",label2="Rt2"){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }


  if(length(interval)>2) interval <- interval[1]
  date.diff=as.numeric(max(t)-min(t))
  level.t <- min(t)+0:date.diff
  if(interval=="week"){
    t <- strftime(t,"%Y-%W")
    level.t <- unique(strftime(level.t,"%Y-%W"))
  }else if(interval=="month"){
    t <- strftime(t,"%Y-%m")
    level.t <- unique(strftime(level.t,"%Y-%m"))
  }

  t <- as.factor(t)
  levels(t) <- level.t

  meanR <- tapply(Rt, t, FUN=function(.) mean(.,na.rm = T))
  minR <- tapply(Rt, t, FUN=function(.) quantile(.,percentile[1],na.rm = T))
  maxR <- tapply(Rt, t, FUN=function(.) quantile(.,percentile[2],na.rm = T))

  meanR[is.na(meanR)]<-0
  minR[is.na(minR)]<-0
  maxR[is.na(maxR)]<-0

  data <- data.frame(t = level.t,meanR,minR,maxR)
  data$date <- 1:nrow(data)
  xbrk <- round(seq(1,nrow(data),length.out = 6))

  if(!is.null(Rt2)){
    meanR2 <- tapply(Rt2, t, FUN=function(.) mean(.,na.rm = T))
    minR2 <- tapply(Rt2, t, FUN=function(.) quantile(.,percentile[1],na.rm = T))
    maxR2 <- tapply(Rt2, t, FUN=function(.) quantile(.,percentile[2],na.rm = T))


    meanR2[is.na(meanR2)]<-0
    minR2[is.na(minR2)]<-0
    maxR2[is.na(maxR2)]<-0

    data <- data.frame(data,meanR2,minR2,maxR2)
  }

  if(is.null(Rt2)){
    res <- ggplot(data) + geom_line(aes(x = date, y = meanR, group = 1),lwd=1,col=col) +
      geom_line(aes(x = date, y = minR, group = 1),lty=2,col=col)+
      geom_line(aes(x = date, y = maxR, group = 1),lty=2,col=col)+
      geom_ribbon(aes(x = date, ymin = minR, ymax = maxR), fill=col, alpha = 0.2)
  }else{
    res <- ggplot(data) + geom_line(aes(x = date, y = meanR, group = 1,col="Rt"),lwd=1) +
      geom_line(aes(x = date, y = minR, group = 1),lty=2,col=col)+
      geom_line(aes(x = date, y = maxR, group = 1),lty=2,col=col)+
      geom_ribbon(aes(x = date, ymin = minR, ymax = maxR), fill=col, alpha = 0.2)+
      geom_line(aes(x = date, y = meanR2, group = 2,col="Rt2"),lwd=1) +
      geom_line(aes(x = date, y = minR2, group = 2),lty=2,col=col2)+
      geom_line(aes(x = date, y = maxR2, group = 2),lty=2,col=col2)+
      geom_ribbon(aes(x = date, ymin = minR2, ymax = maxR2), fill=col, alpha = 0.2)+
      scale_color_manual(NULL,breaks = c("Rt","Rt2"),values = c("Rt"=col,"Rt2"=col2),labels = c("Rt"=label,"Rt2"=label2))
  }

  res<-res + geom_hline(yintercept=1, linetype="dashed", color = "red")


  res <- res + scale_x_continuous(breaks=xbrk, labels = data$t[xbrk])+
    xlab(paste("by",interval))+ylab(paste0("Rt (percentile=",percentile[1]*100,"%~",percentile[2]*100,"%)"))+
    theme_minimal()

  if(!is.null(Rt2)){res<-res+theme(legend.position = c(.9, .9))}

  return(res)
}
