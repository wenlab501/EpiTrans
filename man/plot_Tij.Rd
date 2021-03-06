% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_Tij.R
\name{plot_Tij}
\alias{plot_Tij}
\title{Visualizing Town Level Reproductive Numbers}
\usage{
plot_Tij(Tij, sf)
}
\arguments{
\item{Tij}{Matrix of transmission probablity between regions}

\item{sf}{Object of class 'sf'; polygons of region data}
}
\value{

}
\description{
Plot regional transmission relationship map by transmission probablity matrix
}
\examples{
data("EpiTrans")

covid = GeoLocater(TimeDF = covid19,PointsDF = RndPts)
covid.R_adj=Rj(t = covid$date, x = covid$x, y = covid$y, GI.pdf = lpdf_GI(5.2,1.5), unit_coord = "meter")

Tij = ReduceMatrix(covid.R_adj$Pij,covid$region)

plot_Tij(Tij = Tij, sf = Taipei)
}
