% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Comparisons.R
\name{Get_Apps_Exp}
\alias{Get_Apps_Exp}
\title{Calculate the Experimental Apps for an NBA matchup for a particular nba Season}
\usage{
Get_Apps_Exp(HomeTeam, VisitorTeam, Seasondata, nbins = 25)
}
\arguments{
\item{HomeTeam}{Home Team}

\item{VisitorTeam}{Visitor Team}

\item{Seasondata}{The information of shots, it can be downloaded with function
read_season}

\item{nbins}{The number of bins the hexplot for the shot charts are made
(default is 25)}
}
\value{
a dataframe with the offensive apps, defensive apps and home spread
}
\description{
This function takes an NBA season object and calculates de Apps for a
particular matchup, this is the development version that will become the GetApps.
}
\examples{
data("season2017")
Get_Apps_Exp(HomeTeam = "Bos", VisitorTeam = "Was", Seasondata = season2017)
Get_Apps_Exp(HomeTeam = "GSW", VisitorTeam = "Cle", Seasondata = season2017)
Get_Apps_Exp(HomeTeam = "Cle", VisitorTeam = "GSW", Seasondata = season2017)
}
\seealso{
\code{\link[SpatialBall]{DefShotSeasonGraphTeam}}

\code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
}
\author{
Derek Corcoran <derek.corcoran.barrios@gmail.com>
}
