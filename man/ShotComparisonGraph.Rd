% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Comparisons.R
\name{ShotComparisonGraph}
\alias{ShotComparisonGraph}
\title{plot the comparative shot chart of the matchup of two teams for an NBA
Season}
\usage{
ShotComparisonGraph(HomeTeam, VisitorTeam, Seasondata, nbins = 25,
  quant = 0.4, focus = "all")
}
\arguments{
\item{HomeTeam}{The home team in the match up}

\item{VisitorTeam}{The visitor team in the match up}

\item{Seasondata}{The information of shots, it can be downloaded with function
read_season}

\item{nbins}{The number of bins the hexplot for the shot charts are made
(default is 25)}

\item{quant}{A number between 0 and 1, it determines quantile of shots used
to plot the shot chart, (default is 0.4)}

\item{focus}{A character to specify where the shot chart will focus on, if
the character "all" (default) is specified, all shots will be ploted, if
"plus" the shots where the offense has the advantage, if minus, where the
defense has the advantage}
}
\value{
a ggplot object plotting the offensive shot chart of a given team on
an NBA season
}
\description{
This function takes an NBA season object and makes a comparative shot chart of
all the matchup of two teams through that regular season.
}
\examples{
data("season2017")
#Examples with several teams
ShotComparisonGraph(HomeTeam = "GSW", VisitorTeam = "Sas", Seasondata = season2017)
ShotComparisonGraph(HomeTeam = "GSW", VisitorTeam = "Sas", Seasondata = season2017,
focus = "plus")
}
\seealso{
\code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
}
\author{
Derek Corcoran <derek.corcoran.barrios@gmail.com>
}
