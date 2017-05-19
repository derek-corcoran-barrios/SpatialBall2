#' Calculate the Apps for an NBA matchup for a particular nba Season
#'
#' This function takes an NBA season object and calculates de Apps for a
#' particular matchup.
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 25)
#' @param HomeTeam Home Team
#' @param VisitorTeam Visitor Team
#' @param MAX_Y a numeric that limits the y axis of the shot chart
#' @return a dataframe with the offensive apps, defensive apps and home spread
#' @examples
#' data("season2017")
#' Get_Apps(HomeTeam = "Bos", VisitorTeam = "Was", Seasondata = season2017)
#' Get_Apps(HomeTeam = "GSW", VisitorTeam = "Cle", Seasondata = season2017)
#' Get_Apps(HomeTeam = "Cle", VisitorTeam = "GSW", Seasondata = season2017)
#' @seealso \code{\link[SpatialBall]{DefShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @importFrom caret predict.train
#' @importFrom dplyr filter
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
Get_Apps <- function(HomeTeam, VisitorTeam, Seasondata, nbins = 25, MAX_Y = 280){
  ComparisonPPS <- function(OffTeam, DefTeam, Seasondata, nbins = 25, MAX_Y = 280) {
  #Filter the offensive data of the Offensive Team
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Off <- filter(Seasondata, TEAM_NAME == OffTeam)
  #Filter the Deffensive data of the Defensive team
  deff <- dplyr::filter(Seasondata, HTM == DefTeam | VTM == DefTeam & TEAM_NAME != DefTeam)
  #Get the maximum and minumum values for x and y
  #xbnds <- range(c(Seasondata$LOC_X, deff$LOC_X), na.rm = TRUE)
  #ybnds <- range(c(Seasondata$LOC_Y, deff$LOC_Y), na.rm = TRUE)
  #Make hexbin dataframes out of the teams
  makeHexData <- function(df) {
    h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = c(-250, 250), ybnds = c(-51, MAX_Y), IDs = TRUE)
    data.frame(hcell2xy(h),
               PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
               ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
               cid = h@cell)
  }
  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  ##Defensive team data
  Defhex <- makeHexData(deff)
  ##Offensive team data
  Offhex <- makeHexData(Off)
  #Merge offensive and deffensive data with total data by Cell id
  DeffbyCell <- merge(Totalhex, Defhex, by = "cid", all = T)
  OffByCell <- merge(Totalhex, Offhex, by = "cid", all = T)
  #  make a "difference" data.frame
  DiffDeff <- data.frame(x = ifelse(is.na(DeffbyCell$x.x), DeffbyCell$x.y, DeffbyCell$x.x),
                         y = ifelse(is.na(DeffbyCell$y.x), DeffbyCell$y.y, DeffbyCell$y.x),
                         PPS= DeffbyCell$PPS.y - DeffbyCell$PPS.x,
                         cid= DeffbyCell$cid,
                         ST = DeffbyCell$ST.y)

  DiffOff <- data.frame(x = ifelse(is.na(OffByCell$x.x), OffByCell$x.y, OffByCell$x.x),
                        y = ifelse(is.na(OffByCell$y.x), OffByCell$y.y, OffByCell$y.x),
                        PPS= OffByCell$PPS.y - OffByCell$PPS.x,
                        ST = OffByCell$ST.x,
                        cid = OffByCell$cid,
                        ST = OffByCell$ST.y)
  #make team comparisons
  Comparison <- merge(DiffOff, DiffDeff, by = "cid", all = T)
  Comparison <- Comparison[,-c(6:7)]
  Comparison$Diff <- c(Comparison$PPS.x + Comparison$PPS.y)


  PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), Comparison$ST.x, na.rm = TRUE)
  Offa <- dplyr::filter(Seasondata, HTM == OffTeam | VTM == OffTeam)
  OffCorrection <- nrow(dplyr::filter(Offa, TEAM_NAME == OffTeam))/nrow(dplyr::filter(Offa, TEAM_NAME != OffTeam))
  Defa <- dplyr::filter(Seasondata, HTM == DefTeam | VTM == DefTeam)
  DefCorrection <- nrow(dplyr::filter(Defa, TEAM_NAME != DefTeam))/nrow(dplyr::filter(Defa, TEAM_NAME == DefTeam))
  PPSAAc = PPSAA*((OffCorrection*DefCorrection)/2)


  return(PPSAAc)
  }
  data("BRT")
  defAPPS <- ComparisonPPS(OffTeam = HomeTeam, DefTeam = VisitorTeam, Seasondata = Seasondata, nbins = nbins)
  offAPPS <- ComparisonPPS(OffTeam = VisitorTeam, DefTeam = HomeTeam, Seasondata = Seasondata, nbins = nbins)
  spread <- predict(BRT, data.frame(defAPPS = defAPPS, offAPPS = offAPPS))
  return(data.frame(defAPPS = defAPPS, offAPPS= offAPPS, spread = spread))
}

#####################################################

#' Calculate the Offensive, Defensive, and Net Spatial Rating for a particular
#' NBA Season
#'
#' This function takes an NBA season object and calculates the Offensive,
#' Defensive, and Net Spatial Rating
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 25)
#' @param MAX_Y a numeric that limits the y axis of the shot chart
#' @return a dataframe with the Offensive, Defensive, and Net Spatial Rating for
#' an NBA Season
#' @examples
#' data("season2017")
#' SpatialRating(Seasondata = season2017)
#' @seealso \code{\link[SpatialBall]{DefShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
SpatialRating <- function(Seasondata, nbins = 25, MAX_Y = 280){
  ComparisonPPS <- function(OffTeam, DefTeam, Seasondata, nbins = nbins, MAX_Y = MAX_Y) {
    #Filter the offensive data of the Offensive Team
    Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
    Off <- filter(Seasondata, TEAM_NAME == OffTeam)
    #Filter the Deffensive data of the Defensive team
    deff <- dplyr::filter(Seasondata, HTM == DefTeam | VTM == DefTeam & TEAM_NAME != DefTeam)
    #Get the maximum and minumum values for x and y
    #xbnds <- range(c(Seasondata$LOC_X, deff$LOC_X), na.rm = TRUE)
    #ybnds <- range(c(Seasondata$LOC_Y, deff$LOC_Y), na.rm = TRUE)
    #Make hexbin dataframes out of the teams
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = c(-250, 250), ybnds = c(-51, MAX_Y), IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
    ##Total NBA data
    Totalhex <- makeHexData(Seasondata)
    ##Defensive team data
    Defhex <- makeHexData(deff)
    ##Offensive team data
    Offhex <- makeHexData(Off)
    #Merge offensive and deffensive data with total data by Cell id
    DeffbyCell <- merge(Totalhex, Defhex, by = "cid", all = T)
    OffByCell <- merge(Totalhex, Offhex, by = "cid", all = T)
    #  make a "difference" data.frame
    DiffDeff <- data.frame(x = ifelse(is.na(DeffbyCell$x.x), DeffbyCell$x.y, DeffbyCell$x.x),
                           y = ifelse(is.na(DeffbyCell$y.x), DeffbyCell$y.y, DeffbyCell$y.x),
                           PPS= DeffbyCell$PPS.y - DeffbyCell$PPS.x,
                           cid= DeffbyCell$cid,
                           ST = DeffbyCell$ST.y)

    DiffOff <- data.frame(x = ifelse(is.na(OffByCell$x.x), OffByCell$x.y, OffByCell$x.x),
                          y = ifelse(is.na(OffByCell$y.x), OffByCell$y.y, OffByCell$y.x),
                          PPS= OffByCell$PPS.y - OffByCell$PPS.x,
                          ST = OffByCell$ST.x,
                          cid = OffByCell$cid,
                          ST = OffByCell$ST.y)
    #make team comparisons
    Comparison <- merge(DiffOff, DiffDeff, by = "cid", all = T)
    Comparison <- Comparison[,-c(6:7)]
    Comparison$Diff <- c(Comparison$PPS.x + Comparison$PPS.y)


    PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), Comparison$ST.x, na.rm = TRUE)
    Offa <- dplyr::filter(Seasondata, HTM == OffTeam | VTM == OffTeam)
    OffCorrection <- nrow(dplyr::filter(Offa, TEAM_NAME == OffTeam))/nrow(dplyr::filter(Offa, TEAM_NAME != OffTeam))
    Defa <- dplyr::filter(Seasondata, HTM == DefTeam | VTM == DefTeam)
    DefCorrection <- nrow(dplyr::filter(Defa, TEAM_NAME != DefTeam))/nrow(dplyr::filter(Defa, TEAM_NAME == DefTeam))
    PPSAAc = PPSAA*((OffCorrection*DefCorrection)/2)


    return(PPSAAc)
  }
  df <- data.frame(matrix(ncol = 30, nrow = 30))
  colnames(df) <- as.character(unique(Seasondata$TEAM_NAME))
  rownames(df) <- as.character(unique(Seasondata$TEAM_NAME))

  Offensive_teams <- as.character(unique(Seasondata$TEAM_NAME))
  defenseve_names <- Offensive_teams

  for (i in 1:length(Offensive_teams)) {
    for (j in 1:length(defenseve_names)){
      df[rownames(df) == defenseve_names[j],colnames(df) == Offensive_teams[i]] <- ComparisonPPS(OffTeam = Offensive_teams[i], DefTeam = defenseve_names[j], Seasondata = Seasondata, nbins = nbins, MAX_Y = MAX_Y)
    }
    print(paste(i, "of", length(Offensive_teams)))
  }

  ROWS <- sort(rownames(df))
  COLS <- sort(colnames(df))
  df2 <- df
  for (i in 1:length(ROWS)) {
    df[rownames(df) == COLS[i], colnames(df) == ROWS[i]] <- NA
  }

  offrating <- colMeans(df, na.rm = TRUE)*50
  defrating <- rowMeans(df, na.rm = TRUE)*-50

  offratingDF <- data.frame(Team = colnames(df), offrating = offrating)
  defratingDF <- data.frame(Team = rownames(df), defrating = defrating)

  netDF <- merge.data.frame(offratingDF, defratingDF)
  netDF$netrating <- netDF$offrating + netDF$defrating
  netDF <- arrange(netDF, desc(netrating))
  return(netDF)
}

#' plot the comparative shot chart of the matchup of two teams for an NBA
#' Season
#'
#' This function takes an NBA season object and makes a comparative shot chart of
#' all the matchup of two teams through that regular season.
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param HomeTeam The home team in the match up
#' @param VisitorTeam The visitor team in the match up
#' @param nbins The number of bins the hexplot for the shot charts are made
#' (default is 25)
#' @param quant A number between 0 and 1, it determines quantile of shots used
#' to plot the shot chart, (default is 0.4)
#' @param focus A character to specify where the shot chart will focus on, if
#' the character "all" (default) is specified, all shots will be ploted, if
#' "plus" the shots where the offense has the advantage, if minus, where the
#' defense has the advantage
#' @param MAX_Y a numeric that limits the y axis of the shot chart, defaults at
#' 280
#' @return a ggplot object plotting the offensive shot chart of a given team on
#' an NBA season
#' @examples
#' data("season2017")
#' #Examples with several teams
#' ShotComparisonGraph(HomeTeam = "GSW", VisitorTeam = "Sas", Seasondata = season2017)
#' ShotComparisonGraph(HomeTeam = "GSW", VisitorTeam = "Sas", Seasondata = season2017,
#' focus = "plus")
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
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom gridExtra arrangeGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export

ShotComparisonGraph <-function(HomeTeam, VisitorTeam, Seasondata, nbins = 25, quant = 0.4, focus = "all", MAX_Y = 280){
  ShotComparisonGraph2 <- function(OffTeam, DefTeam, Seasondata, nbins = nbins, quant = 0.4, focus = focus, MAX_Y = MAX_Y) {
  #Filter the offensive data of the Offensive Team
  data("court")
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Off <- filter(Seasondata, TEAM_NAME == OffTeam)
  #Filter the Deffensive data of the Defensive team
  deff <- dplyr::filter(Seasondata, HTM == DefTeam | VTM == DefTeam & TEAM_NAME != DefTeam)
  #Get the maximum and minumum values for x and y
  xbnds <- range(c(Seasondata$LOC_X, deff$LOC_X))
  ybnds <- range(c(Seasondata$LOC_Y, deff$LOC_Y))
  #Make hexbin dataframes out of the teams
  makeHexData <- function(df) {
    h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
    data.frame(hcell2xy(h),
               PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
               ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
               cid = h@cell)
  }
  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  ##Defensive team data
  Defhex <- makeHexData(deff)
  ##Offensive team data
  Offhex <- makeHexData(Off)
  #Merge offensive and deffensive data with total data by Cell id
  DeffbyCell <- merge(Totalhex, Defhex, by = "cid", all = T)
  OffByCell <- merge(Totalhex, Offhex, by = "cid", all = T)
  ##  when calculating the difference empty cells should count as 0
  DeffbyCell$PPS.x[is.na(DeffbyCell$PPS.x)] <- 0
  DeffbyCell$PPS.y[is.na(DeffbyCell$PPS.y)] <- 0
  DeffbyCell$ST.y[is.na(DeffbyCell$ST.y)] <- 0

  OffByCell$PPS.x[is.na(OffByCell$PPS.x)] <- 0
  OffByCell$PPS.y[is.na(OffByCell$PPS.y)] <- 0
  OffByCell$ST.y[is.na(OffByCell$ST.y)] <- 0
  #  make a "difference" data.frame
  DiffDeff <- data.frame(x = ifelse(is.na(DeffbyCell$x.x), DeffbyCell$x.y, DeffbyCell$x.x),
                         y = ifelse(is.na(DeffbyCell$y.x), DeffbyCell$y.y, DeffbyCell$y.x),
                         PPS= DeffbyCell$PPS.y - DeffbyCell$PPS.x,
                         cid= DeffbyCell$cid,
                         ST = DeffbyCell$ST.y)

  DiffOff <- data.frame(x = ifelse(is.na(OffByCell$x.x), OffByCell$x.y, OffByCell$x.x),
                        y = ifelse(is.na(OffByCell$y.x), OffByCell$y.y, OffByCell$y.x),
                        PPS= OffByCell$PPS.y - OffByCell$PPS.x,
                        ST = OffByCell$ST.x,
                        cid = OffByCell$cid,
                        ST = OffByCell$ST.y)
  #make team comparisons
  Comparison <- merge(DiffOff, DiffDeff, by = "cid", all = T)
  Comparison <- Comparison[,-c(6:7)]
  Comparison$Diff <- c(Comparison$PPS.x + Comparison$PPS.y)


  PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), Comparison$ST.x)


  #Legend extractor
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- log(size * width / 6)
    dy <- log(size * height / 2 / sqrt(3))

    hex_x <- rbind(x - 2 * dx, x - dx, x + dx, x + 2 * dx, x + dx, x - dx)
    hex_y <- rbind(y, y + dy, y + dy, y, y - dy, y - dy)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Filter by quantile and focus
  if (focus == "all") {
    DiffOff <- filter(DiffOff, ST > quantile(DiffOff$ST, probs = quant))
    DiffDeff <- filter(DiffDeff, ST > quantile(DiffDeff$ST, probs = quant))
    Comparison <- filter(Comparison, ST.x > quantile(Comparison$ST.x, probs = quant))
  }
  if (focus == "plus"){
    DiffOff <- filter(DiffOff, ST > quantile(DiffOff$ST, probs = quant))
    DiffDeff <- filter(DiffDeff, ST > quantile(DiffDeff$ST, probs = quant))
    Comparison <- filter(Comparison, ST.x > quantile(Comparison$ST.x, probs = quant))
    Comparison <- filter(Comparison, Diff >= 0)
  }

  if (focus == "minus") {
    DiffOff <- filter(DiffOff, ST > quantile(DiffOff$ST, probs = quant))
    DiffDeff <- filter(DiffDeff, ST > quantile(DiffDeff$ST, probs = quant))
    Comparison <- filter(Comparison, ST.x > quantile(Comparison$ST.x, probs = quant))
    Comparison <- filter(Comparison, Diff <= 0)
  }
  #Transform Hexbins into polygons

  DFOFF <- hex_coord_df(DiffOff$x, DiffOff$y, (0.05*DiffOff$ST), (0.05*DiffOff$ST), size =1)
  DFOFF$PPS <- rep(DiffOff$PPS, each = 6)

  DFDEF <- hex_coord_df(DiffDeff$x, DiffDeff$y, DiffDeff$ST, DiffDeff$ST, size =1)
  DFDEF$PPS <- rep(DiffDeff$PPS, each = 6)

  DFDIF <- hex_coord_df(Comparison$x.x, Comparison$y.x, (0.05*Comparison$ST.x),(0.05*Comparison$ST.x), size =1)
  DFDIF$Dif <- rep(Comparison$Diff, each = 6)

  #Create Legend
  OFFLEG <- ggplot(DFOFF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 270)) + xlim(-250, 250)+ theme(legend.position="bottom") +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  leg<-g_legend(OFFLEG)

  OFF <- ggplot(DFOFF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 270))+ theme(legend.position="none") + xlim(c(-250, 250)) +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  DEF <- ggplot(DFDEF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS))+ scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 270))+ xlim(c(-250, 250))+ theme(legend.position="none") + ggtitle(paste(DefTeam, "defensive\n Shot Chart", sep = " "))

  COMP <- ggplot(DFDIF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = Dif)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) +  ylim(c(-40, 270))+ xlim(c(-250, 250))+ theme(legend.position="none") + ggtitle("Comparison\n Shot Chart")
  charts <- arrangeGrob(DEF,OFF, COMP, ncol = 3)
  return(charts)
}


Com1 <- ShotComparisonGraph2(OffTeam = HomeTeam, DefTeam = VisitorTeam, Seasondata = Seasondata , nbins = nbins, quant = quant, focus = focus)
Com2 <- ShotComparisonGraph2(OffTeam = VisitorTeam, DefTeam = HomeTeam, Seasondata = Seasondata , nbins = nbins, quant = quant, focus = focus)
return(grid.arrange(Com1, Com2))
}
