#' Draw the epidemic curve
#'
#' Plot epidemic curve (time series of incident cases); epidemic curve (time series of incident cases) is the most common way to characterize temporal pattern of an outbreak.
#'
#' @param t Vector of time of data points.
#' @param interval Character; time resolution(day, week, month) of animation ; aggregrate data through day, week or month.
#' @param plot.points Logical; if TRUE points of line chart will be marked.
#' @return
#'
#' @examples
#' data("EpiTrans")
#'
#' plot_epi(t = dengue$date, interval ="week")
#' @import ggplot2
#' @export

plot_epi <- function(t, interval = c("day","week","month"), plot.points=NULL){
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

  if(is.null(plot.points)){
    if(interval == "day") plot.points=F else plot.points=T
  }

  t <- as.factor(t)
  levels(t) <- level.t
  data <- data.frame(xtabs(~t))
  data$date <- as.numeric(data$t)

  xbrk <- round(seq(1,nrow(data),length.out = 6))


  res <- ggplot(data) + geom_line(aes(x = date, y = Freq, group = 1),lwd=1)
  if(plot.points) res <- res + geom_point(aes(x = date, y = Freq, group = 1))
  res <- res +scale_x_continuous(breaks=xbrk, labels = data$t[xbrk])+
    xlab(paste("by",interval))+ylab("cases")+
    theme_minimal()

  return(res)
}
