## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ------------------------------------------------------------------------
library(SpatialBall2)

## ------------------------------------------------------------------------
SeasonData <- Seasonscrape(season = "2017-18", Start = "12/05/2017", End = "12/07/2017")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 1. Shot chart of Stephen Curry"----
data("season2017")
ShotSeasonGraphPlayer(season2017, player = "Stephen Curry")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 2. Shot chart of Stephen Curry showing the percentage of shots made"----
ShotSeasonGraphPlayer(season2017, player = "Stephen Curry", type = "PCT")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 3. Shot chart of Kyle Singler"----
ShotSeasonGraphPlayer(season2017, player = "Kyle Singler")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 4. Shot chart of Kyle Singler, point and kernel"----
PointShotSeasonGraphPlayer(season2017, player = "Kyle Singler")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 5. Shot chart of Kyle Singler, point and kernel, only made shots"----
PointShotSeasonGraphPlayer(season2017, player = "Kyle Singler", Type = "Made")

## ---- fig.height= 6, fig.width=6, fig.cap="Figure 6. Shot chart of Kyle Singler, points only, only made shots"----
PointShotSeasonGraphPlayer(season2017, player = "Kyle Singler", Type = "Made", kernel = FALSE)

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 7. Offensive Shot chart of the Golden State Warriors"----
data("season2017")
OffShotSeasonGraphTeam(season2017, team = "GSW")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 8. Offensive Shot chart of the Golden State Warriors, percentage of shots made"----
data("season2017")
OffShotSeasonGraphTeam(season2017, team = "GSW", type = "PCT")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 9. Offensive Shot chart of the Golden State Warriors, percentage of shots made"----
data("season2017")
DefShotSeasonGraphTeam(season2017, team = "Sas", type = "PCT")

## ---- fig.height= 5, fig.width=10, fig.cap="Figure 10. Comparative shot chart of 2016-17 Miami Heat against Houston Rockets"----
ShotComparisonGraph(HomeTeam = "Hou", VisitorTeam = "Mia", Seasondata = season2017)

## ---- fig.height= 5, fig.width=10, fig.cap="Figure 11. Comparative shot chart of 2016-17 Miami Heat against Houston Rockets, focusing in the areas where the attack has the advantage"----
ShotComparisonGraph(HomeTeam = "Hou", VisitorTeam = "Mia", Seasondata = season2017, focus = "plus")

## ---- fig.height= 5, fig.width=6, fig.cap="Figure 12. Shot chart of the Whole 2017 Season"----
data("season2017")
ShotSeasonGraph(season2017, quant = 0.4)

## ------------------------------------------------------------------------
data("season2017")
Stats2017 <- TeamStats(Seasondata = season2017)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(Stats2017[[1]], caption = "Offensive team stats")

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(Stats2017[[2]], caption = "Defensive team stats")

## ------------------------------------------------------------------------
data("season2017")
Get_Apps(HomeTeam = "Cha", VisitorTeam = "Bos", Seasondata = season2017)

## ------------------------------------------------------------------------
Get_Apps(HomeTeam = "Bos", VisitorTeam = "Cha", Seasondata = season2017)

## ------------------------------------------------------------------------
knitr::kable(SpatialRating(Seasondata = season2017))

