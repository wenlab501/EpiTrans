#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_Rt <- function(t, Rt, Rt2=NULL, interval = c("day","week","month")){
  require("ggplot2")

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

  meanR <- tapply(Rt, t, FUN=mean)
  minR <- tapply(Rt, t, FUN=min)
  maxR <- tapply(Rt, t, FUN=max)

  meanR[is.na(meanR)]<-0
  minR[is.na(minR)]<-0
  maxR[is.na(maxR)]<-0

  data <- data.frame(t = level.t,meanR,minR,maxR)
  data$date <- 1:nrow(data)
  xbrk <- round(seq(1,nrow(data),length.out = 6))

  if(!is.null(Rt2)){
    meanR2 <- tapply(Rt2, t, FUN=mean)
    minR2 <- tapply(Rt2, t, FUN=min)
    maxR2 <- tapply(Rt2, t, FUN=max)

    meanR2[is.na(meanR2)]<-0
    minR2[is.na(minR2)]<-0
    maxR2[is.na(maxR2)]<-0

    data <- data.frame(data,meanR2,minR2,maxR2)
  }

  res <- ggplot(data) + geom_line(aes(x = date, y = meanR, group = 1),lwd=1,col="orange") +
    geom_line(aes(x = date, y = minR, group = 1),lty=2,col="orange")+
    geom_line(aes(x = date, y = maxR, group = 1),lty=2,col="orange")

  if(!is.null(Rt2)){
  res <- res+ geom_line(aes(x = date, y = meanR2, group = 2),lwd=1,col="darkgreen") +
    geom_line(aes(x = date, y = minR2, group = 2),lty=2,col="darkgreen")+
    geom_line(aes(x = date, y = maxR2, group = 2),lty=2,col="darkgreen")
  }

  res <- res + scale_x_continuous(breaks=xbrk, labels = data$t[xbrk])+
    xlab(paste("by",interval))+ylab("cases")+
    theme_minimal()

  return(res)
}
