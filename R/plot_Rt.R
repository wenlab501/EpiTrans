#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_Rt <- function(t, Rt, Rt2=NULL, conf.level=1, interval = c("day","week","month"), col="orange",col2="darkgreen",label="Rt",label2="Rt2"){
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

  meanR <- tapply(Rt, t, FUN=function(.) mean(.,na.rm = T))
  minR <- tapply(Rt, t, FUN=function(.) quantile(.,0.5-conf.level/2,na.rm = T))
  maxR <- tapply(Rt, t, FUN=function(.) quantile(.,0.5+conf.level/2,na.rm = T))

  meanR[is.na(meanR)]<-0
  minR[is.na(minR)]<-0
  maxR[is.na(maxR)]<-0

  data <- data.frame(t = level.t,meanR,minR,maxR)
  data$date <- 1:nrow(data)
  xbrk <- round(seq(1,nrow(data),length.out = 6))

  if(!is.null(Rt2)){
    meanR2 <- tapply(Rt2, t, FUN=function(.) mean(.,na.rm = T))
    minR2 <- tapply(Rt2, t, FUN=function(.) quantile(.,0.5-conf.level/2,na.rm = T))
    maxR2 <- tapply(Rt2, t, FUN=function(.) quantile(.,0.5+conf.level/2,na.rm = T))


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
    xlab(paste("by",interval))+ylab(paste0("Rt (conf.lvl=",conf.level*100,"%)"))+
    theme_minimal()

  if(!is.null(Rt2)){res<-res+theme(legend.position = c(.9, .9))}

  return(res)
}
