% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_Rt.R
\name{plot_Rt}
\alias{plot_Rt}
\title{Visualizing Time-varying reproductive numbers}
\usage{
plot_Rt(
  t,
  Rt,
  Rt2 = NULL,
  percentile = c(0, 1),
  interval = c("day", "week", "month"),
  col = "orange",
  col2 = "darkgreen",
  label = "Rt",
  label2 = "Rt2"
)
}
\arguments{
\item{t}{Vector of time of data points.}

\item{Rt}{Vector of individual reproductive numbers of data points, stand for Rt1.}

\item{Rt2}{Another vector of individual reproductive numbers of data points to compare, stand for Rt2.}

\item{percentile}{A 2-length numerical vector, draw the area of value of percentile range; if c(0,1) means min and max will be drawn, c(0.05,0.95) means the middle 90 percent value will be drawn.}

\item{interval}{Character; time resolution(day, week, month) of animation ; aggregrate data through day, week or month.}

\item{col}{Color of line Rt1.}

\item{col2}{Color of line Rt2.}

\item{label}{Label of line Rt1.}

\item{label2}{Label of line Rt2.}
}
\value{

}
\description{
Plot line chart of individual reproductive numbers by time
}
\examples{
data("EpiTrans")

covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
covid.R_adj=Rj(t = covid$date, x = covid$x, y = covid$y, GI.pdf = lpdf_GI(5.2,1.5), unit_coord = "meter")

plot_Rt(t = covid$date, Rt=covid.R_adj$Rj, Rt2=covid.R_adj$Rj,percentile = c(.025,.975))
}
