#' Title
#'
#' Descriptions Here
#' @param mean mean.
#' @param sd sd
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

plot_epi <- function(t, interval = c("day","week","month")){
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
  data <- data.frame(xtabs(~t))
  data$date <- as.numeric(data$t)

  xbrk <- round(seq(1,nrow(data),length.out = 6))


  res <- ggplot(data) + geom_line(aes(x = date, y = Freq, group = 1),lwd=1) +
    scale_x_continuous(breaks=xbrk, labels = data$t[xbrk])+
    xlab(paste("by",interval))+ylab("cases")+
    theme_minimal()

  return(res)
}
