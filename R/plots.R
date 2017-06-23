#' plot the shot chart of a whole NBA Season
#'
#' This function takes an NBA season object and makes a shot chart of all the
#' shots takes through that regular season.
#' You can choose to either plot the results based on Points per Shot or on
#' Shooting Percentage
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 30)
#' @param quant A number between 0 and 1, it determines quantile of shots used
#' to plot the shot chart, (default is 0.4)
#' @param type A character to specify if the shot chart is based on Points per
#' Shot ("PPS") or percentage ("PCT")
#' @param MAX_Y a numeric that limits the y axis of the shot chart, defaults at
#' 280
#' @return a ggplot object plotting the shot chart of a given NBA season
#' @examples
#' data("season2017")
#' ShotSeasonGraph(season2017, quant = 0.4)
#' ShotSeasonGraph(season2017, quant = 0.4, type = "PCT")
#' @seealso \code{\link[SpatialBall]{DefShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export


ShotSeasonGraph <- function(Seasondata, nbins = 25, quant = 0.4, type = "PPS", MAX_Y = 280) {
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250)) + theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle("Points per Shot")
  }  else {GRAPH <- GRAPH +  ggtitle("Shooting percentage")}


  return(GRAPH)
}

#' plot the offensive shot chart of a team for an NBA Season
#'
#' This function takes an NBA season object and makes a shot chart of all the
#' shots takes through that regular season.
#' You can choose to either plot the results based on Points per Shot or on
#' Shooting Percentage
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param team the team you which to plot the shot charts of
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 30)
#' @param quant A number between 0 and 1, it determines quantile of shots used
#' to plot the shot chart, (default is 0.4)
#' @param type A character to specify if the shot chart is based on Points per
#' Shot ("PPS") or percentage ("PCT")
#' @param MAX_Y a numeric that limits the y axis of the shot chart, defaults at
#' 270
#' @return a ggplot object plotting the offensive shot chart of a given team on
#' an NBA season
#' @examples
#' data("season2017")
#' #Examples with several teams
#' OffShotSeasonGraphTeam(season2017, team = "GSW",quant = 0.4)
#' OffShotSeasonGraphTeam(season2017, team = "Hou",quant = 0.4)
#' OffShotSeasonGraphTeam(season2017, team = "ORL",quant = 0.4)
#' #Examples with shooting percentage instead of Points per Shot
#' OffShotSeasonGraphTeam(season2017, team = "ORL",quant = 0.4, type = "PCT")
#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{ShotSeasonGraph}}#' @importFrom dplyr filter
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export

OffShotSeasonGraphTeam <- function(Seasondata, team, nbins = 25, quant = 0.4, type = "PPS", MAX_Y = 280) {
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Seasondata <- dplyr::filter(Seasondata, TEAM_NAME == team)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")}
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", team, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", team, sep =" ")
  )}


  return(GRAPH)
}

#' plot the defensive shot chart of a team for an NBA Season
#'
#' This function takes an NBA season object and makes a shot chart of all the
#' shots takes through that regular season.
#' You can choose to either plot the results based on Points per Shot or on
#' Shooting Percentage
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param team the team you which to plot the defensive shot charts of
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 25)
#' @param quant A number between 0 and 1, it determines quantile of shots used
#' to plot the shot chart, (default is 0.4)
#' @param type A character to specify if the shot chart is based on Points per
#' Shot ("PPS") or percentage ("PCT")
#' @param MAX_Y a numeric that limits the y axis of the shot chart, defaults at
#' 280
#' @return a ggplot object plotting the defensive shot chart of a given team on
#' an NBA season
#' @examples
#' data("season2017")
#' #Examples with several teams
#' DefShotSeasonGraphTeam(season2017, team = "GSW")
#' DefShotSeasonGraphTeam(season2017, team = "Hou")
#' DefShotSeasonGraphTeam(season2017, team = "ORL")
#' #Examples with shooting percentage instead of Points per Shot
#' DefShotSeasonGraphTeam(season2017, team = "ORL", type = "PCT")
#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{ShotSeasonGraph}}
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export

DefShotSeasonGraphTeam <- function(Seasondata, team, nbins = 25, quant = 0.4, type = "PPS", MAX_Y = 280) {
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Seasondata <- dplyr::filter(Seasondata, HTM == team | VTM == team & TEAM_NAME != team)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(name = "PPS", midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(name = "Pct", midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")}
    GRAPH <- GRAPH +  ggtitle(paste("Defensive shot chart of", team, sep =" "))


  return(GRAPH)
}

#' plot the shot chart of a player for an NBA Season
#'
#' This function takes an NBA season object and makes a shot chart of all the
#' shots takes through that regular season.
#' You can choose to either plot the results based on Points per Shot or on
#' Shooting Percentage
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param player the player you which to plot the shot charts of
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 25)
#' @param quant A number between 0 and 1, it determines quantile of shots used
#' to plot the shot chart, (default is 0.4)
#' @param type A character to specify if the shot chart is based on Points per
#' Shot ("PPS") or percentage ("PCT")
#' @param MAX_Y a numeric that limits the y axis of the shot chart, defaults at
#' 280
#' @return a ggplot object plotting the defensive shot chart of a given team on
#' an NBA season
#' @examples
#' data("season2017")
#' #Examples with several players
#' OffShotSeasonGraphPlayer(season2017, player = "Stephen Curry")
#' OffShotSeasonGraphPlayer(season2017, player = "DeAndre Jordan")
#' OffShotSeasonGraphPlayer(season2017, player = "DeMar DeRozan")
#' OffShotSeasonGraphPlayer(season2017, player = "Isaiah Thomas")
#'
#'  #Examples with percentage instead of points per shot
#' OffShotSeasonGraphPlayer(season2017, player = "Stephen Curry", type = "PCT")
#' OffShotSeasonGraphPlayer(season2017, player = "DeAndre Jordan", type = "PCT")

#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{ShotSeasonGraph}}
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'
OffShotSeasonGraphPlayer <- function(Seasondata, player, nbins = 25, quant = 0.4, type = "PPS", MAX_Y = 280) {
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
Seasondata <- dplyr::filter(Seasondata, PLAYER_NAME == player)
#Get the maximum and minumum values for x and y
xbnds <- range(Seasondata$LOC_X)
ybnds <- range(Seasondata$LOC_Y)
#Make hexbin dataframes out of the players

if (type == "PPS"){
  makeHexData <- function(df) {
    h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
    data.frame(hcell2xy(h),
               PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
               ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
               cid = h@cell)
  }
}

if (type == "PCT"){
  makeHexData <- function(df) {
    h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
    data.frame(hcell2xy(h),
               PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
               ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
               cid = h@cell)
  }
}


##Total NBA data
Totalhex <- makeHexData(Seasondata)
Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                             ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                    ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                           1))))


#Function to transform hexbins into polygons
hex_coord_df <- function(x, y, width, height, size = 1) {
  # like hex_coord but returns a dataframe of vertices grouped by an id variable
  dx <- size * width / 6
  dy <- size * height / 2 / sqrt(3)

  hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
  hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
  id    <- rep(1:length(x), each=6)

  data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
}

#Transform Hexbins into polygons

Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
Total$PPS <- rep(Totalhex$PPS, each = 6)

#Make Graph
if(type == "PPS"){
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
}else{
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
}
if(type == "PPS"){
  GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", player, sep =" "))
}  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", player, sep =" ")
)}


return(GRAPH)
}


#' plot all the shots taken on an NBA Season separating makes and misses
#'
#' This function takes an NBA season object and plots as circles all the shots
#' taken on an nba season, ploting makes and misses as different colors
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @return a ggplot object plotting all the shots taken on an NBA season
#' @examples
#' data("season2017")
#' PointShotSeasonGraph(season2017)
#' @seealso \code{\link[SpatialBall]{ShotSeasonGraph}}
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'


PointShotSeasonGraph <- function(Seasondata) {
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < 280)
  Seasondata$SHOT_MADE_FLAG <- ifelse(Seasondata$SHOT_MADE_FLAG == "1", "Made", "Missed")
  Seasondata$SHOT_MADE_FLAG <- as.factor(Seasondata$SHOT_MADE_FLAG)
  #Make Graph
  GRAPH <- ggplot(Seasondata, aes(x=LOC_X, y = LOC_Y))+ annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(color = SHOT_MADE_FLAG), alpha = 0.2) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280))+ theme(legend.position="bottom")
  return(GRAPH)
}


#' plot all the shots taken on an NBA Season separating makes and misses
#'
#' This function takes an NBA season object and plots as circles all the shots
#' taken on an nba season, ploting makes and misses as different colors
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param team The team the shots should be graphed
#' @return a ggplot object plotting all the shots taken by a team on an NBA
#' season
#' @examples
#' data("season2017")
#' PointShotSeasonGraphTeam(season2017, "Hou")
#' PointShotSeasonGraphTeam(season2017, "Orl")
#' @seealso \code{\link[SpatialBall]{ShotSeasonGraph}}
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'


PointShotSeasonGraphTeam <- function(Seasondata, team) {
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < 280)
  Seasondata <- dplyr::filter(Seasondata, TEAM_NAME == team)
  Seasondata$SHOT_MADE_FLAG <- ifelse(Seasondata$SHOT_MADE_FLAG == "1", "Made", "Missed")
  Seasondata$SHOT_MADE_FLAG <- as.factor(Seasondata$SHOT_MADE_FLAG)
  #Make Graph
  GRAPH <- ggplot(Seasondata, aes(x=LOC_X, y = LOC_Y))+ annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(color = SHOT_MADE_FLAG), alpha = 0.2) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280))+ theme(legend.position="bottom")
  return(GRAPH)
}


#' plot the shot chart of a player for an NBA Season with eight bins
#'
#' This function takes an NBA season object and makes a shot chart of all the
#' shots takes through that regular season.
#' You can choose to either plot the results based on Points per Shot or on
#' Shooting Percentage
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param player the player you which to plot the shot charts of
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 8)
#' @param quant A number between 0 and 1, it determines quantile of shots used
#' to plot the shot chart, (default is 0.4)
#' @param type A character to specify if the shot chart is based on Points per
#' Shot ("PPS") or percentage ("PCT")
#' @param MAX_Y a numeric that limits the y axis of the shot chart, defaults at
#' 280
#' @return a ggplot object plotting the defensive shot chart of a given team on
#' an NBA season
#' @examples
#' data("season2017")
#' #Examples with several players
#' OffShotSeasonGraphPlayer(season2017, player = "Stephen Curry")
#' OffShotSeasonGraphPlayer(season2017, player = "DeAndre Jordan")
#' OffShotSeasonGraphPlayer(season2017, player = "DeMar DeRozan")
#' OffShotSeasonGraphPlayer(season2017, player = "Isaiah Thomas")
#'
#'  #Examples with percentage instead of points per shot
#' OffShotSeasonGraphPlayerEight(season2017, player = "Stephen Curry", type = "PCT")
#' OffShotSeasonGraphPlayerEight(season2017, player = "DeAndre Jordan", type = "PCT")

#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{ShotSeasonGraph}}
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'
OffShotSeasonGraphPlayerEight <- function(Seasondata, player, nbins = 8, quant = 0.4, type = "PPS", MAX_Y = 418) {
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Seasondata <- dplyr::filter(Seasondata, PLAYER_NAME == player)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the players

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 170*Totalhex$ST, 50*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 320))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 320))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", player, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", player, sep =" ")
  )}


  return(GRAPH)
}
