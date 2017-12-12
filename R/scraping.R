#' Gets a season dataset from the NBA stats page
#'
#' Creates a season dataset
#' @param season the season of which you want to get the data, in a character format,
#' defaults to "2017-18"
#' @param type One of: "Regular Season", "Pre Season", "Playoffs", "All-Star",
#' "All Star", "Preseason", defaults to "Regular Season"
#' @param Start a character in a month/day/year format, if NULL (default)
#' it starts at the beginning of the season
#' @param End a character in a month/day/year format, if NULL (default)
#' it ends scraping at the end of the season
#'
#' @return A season database
#' @examples
#'
#' \dontrun{
#'
#' #Without date limits
#'
#' Seasonscrape(season = "2017-18")
#'
#' #With date limits
#'
#' Seasonscrape(season = "2017-18", Start = "12/05/2017", End = "12/07/2017")
#'
#' #Scraping playoff games with date limits
#'
#' Seasonscrape(season = "2016-17", type = "Playoffs", Start = "03/25/2017", End = "03/30/2017")
#'
#' }
#' @importFrom rjson fromJSON
#' @importFrom lubridate ymd
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'

Seasonscrape <- function(season = "2017-18", type = "Regular Season", Start = NULL, End = NULL){

  type <-sub(x = type, " ", "+")

  Season <- list()

  if(is.null(Start) & is.null(End)){
    shotURLtotal <- paste0("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",season ,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=",season,"&SeasonSegment=&SeasonType=", type,"&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&PlayerPosition=")
  }

  if(!is.null(Start) & is.null(End)){
    shotURLtotal <- paste0("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",season ,"&ContextFilter=&ContextMeasure=FGA&DateFrom=", Start ,"&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=",season,"&SeasonSegment=&SeasonType=", type,"&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&PlayerPosition=")
  }

  if(is.null(Start) & !is.null(End)){
    shotURLtotal <- paste0("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",season ,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=",End,"&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=",season,"&SeasonSegment=&SeasonType=", type,"&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&PlayerPosition=")
  }

  if(!is.null(Start) & !is.null(End)){
    shotURLtotal <- paste0("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",season ,"&ContextFilter=&ContextMeasure=FGA&DateFrom=", Start ,"&DateTo=",End,"&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=",season,"&SeasonSegment=&SeasonType=", type,"&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&PlayerPosition=")
  }

  # import from JSON
  Season <- fromJSON(file = shotURLtotal, method="C")
  Names <- Season$resultSets[[1]][[2]]
  # unlist shot data, save into a data frame
  Season <- data.frame(matrix(unlist(Season$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE))

  colnames(Season) <- Names


  # covert x and y coordinates into numeric
  Season$LOC_X <- as.numeric(as.character(Season$LOC_X))
  Season$LOC_Y <- as.numeric(as.character(Season$LOC_Y))


  Season$TEAM_NAME <- gsub("Detroit Pistons", "Det", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Atlanta Hawks", "Atl", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Chicago Bulls", "Chi", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Boston Celtics", "Bos", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Cleveland Cavaliers", "Cle", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("New Orleans Pelicans", "NO", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Golden State Warriors", "GSW", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Orlando Magic", "ORL", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Washington Wizards", "Was", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Philadelphia 76ers", "Phi", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Brooklyn Nets", "Bkn", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Utah Jazz", "Uta", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Miami Heat", "Mia", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Charlotte Hornets", "Cha", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Toronto Raptors", "Tor", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Indiana Pacers", "Ind", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Houston Rockets", "Hou", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Denver Nuggets", "Den", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Memphis Grizzlies", "Mem", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("New York Knicks", "NY", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Milwaukee Bucks", "Mil", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Oklahoma City Thunder", "Okc", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("San Antonio Spurs", "Sas", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Dallas Mavericks", "Dal", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Phoenix Suns", "Pho", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Portland Trail Blazers", "Por", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("LA Clippers", "Lac", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Sacramento Kings", "Sac", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Los Angeles Lakers", "Lal", Season$TEAM_NAME)
  Season$TEAM_NAME <- gsub("Minnesota Timberwolves", "Min", Season$TEAM_NAME)


  ####HOME VISITOR


  Season$HTM <- gsub("DET", "Det", Season$HTM)
  Season$HTM <- gsub("ATL", "Atl", Season$HTM)
  Season$HTM <- gsub("CHI", "Chi", Season$HTM)
  Season$HTM <- gsub("BOS", "Bos", Season$HTM)
  Season$HTM <- gsub("CLE", "Cle", Season$HTM)
  Season$HTM <- gsub("NOP", "NO", Season$HTM)
  Season$HTM <- gsub("GSW", "GSW", Season$HTM)
  Season$HTM <- gsub("ORL", "ORL", Season$HTM)
  Season$HTM <- gsub("WAS", "Was", Season$HTM)
  Season$HTM <- gsub("PHI", "Phi", Season$HTM)
  Season$HTM <- gsub("BKN", "Bkn", Season$HTM)
  Season$HTM <- gsub("UTA", "Uta", Season$HTM)
  Season$HTM <- gsub("MIA", "Mia", Season$HTM)
  Season$HTM <- gsub("CHA", "Cha", Season$HTM)
  Season$HTM <- gsub("TOR", "Tor", Season$HTM)
  Season$HTM <- gsub("IND", "Ind", Season$HTM)
  Season$HTM <- gsub("HOU", "Hou", Season$HTM)
  Season$HTM <- gsub("DEN", "Den", Season$HTM)
  Season$HTM <- gsub("MEM", "Mem", Season$HTM)
  Season$HTM <- gsub("NYK", "NY", Season$HTM)
  Season$HTM <- gsub("MIL", "Mil", Season$HTM)
  Season$HTM <- gsub("OKC", "Okc", Season$HTM)
  Season$HTM <- gsub("SAS", "Sas", Season$HTM)
  Season$HTM <- gsub("DAL", "Dal", Season$HTM)
  Season$HTM <- gsub("PHX", "Pho", Season$HTM)
  Season$HTM <- gsub("POR", "Por", Season$HTM)
  Season$HTM <- gsub("LAC", "Lac", Season$HTM)
  Season$HTM <- gsub("SAC", "Sac", Season$HTM)
  Season$HTM <- gsub("LAL", "Lal", Season$HTM)
  Season$HTM <- gsub("MIN", "Min", Season$HTM)

  ###Visitor


  Season$VTM <- gsub("DET", "Det", Season$VTM)
  Season$VTM <- gsub("ATL", "Atl", Season$VTM)
  Season$VTM <- gsub("CHI", "Chi", Season$VTM)
  Season$VTM <- gsub("BOS", "Bos", Season$VTM)
  Season$VTM <- gsub("CLE", "Cle", Season$VTM)
  Season$VTM <- gsub("NOP", "NO", Season$VTM)
  Season$VTM <- gsub("GSW", "GSW", Season$VTM)
  Season$VTM <- gsub("ORL", "ORL", Season$VTM)
  Season$VTM <- gsub("WAS", "Was", Season$VTM)
  Season$VTM <- gsub("PHI", "Phi", Season$VTM)
  Season$VTM <- gsub("BKN", "Bkn", Season$VTM)
  Season$VTM <- gsub("UTA", "Uta", Season$VTM)
  Season$VTM <- gsub("MIA", "Mia", Season$VTM)
  Season$VTM <- gsub("CHA", "Cha", Season$VTM)
  Season$VTM <- gsub("TOR", "Tor", Season$VTM)
  Season$VTM <- gsub("IND", "Ind", Season$VTM)
  Season$VTM <- gsub("HOU", "Hou", Season$VTM)
  Season$VTM <- gsub("DEN", "Den", Season$VTM)
  Season$VTM <- gsub("MEM", "Mem", Season$VTM)
  Season$VTM <- gsub("NYK", "NY", Season$VTM)
  Season$VTM <- gsub("MIL", "Mil", Season$VTM)
  Season$VTM <- gsub("OKC", "Okc", Season$VTM)
  Season$VTM <- gsub("SAS", "Sas", Season$VTM)
  Season$VTM <- gsub("DAL", "Dal", Season$VTM)
  Season$VTM <- gsub("PHX", "Pho", Season$VTM)
  Season$VTM <- gsub("POR", "Por", Season$VTM)
  Season$VTM <- gsub("LAC", "Lac", Season$VTM)
  Season$VTM <- gsub("SAC", "Sac", Season$VTM)
  Season$VTM <- gsub("LAL", "Lal", Season$VTM)
  Season$VTM <- gsub("MIN", "Min", Season$VTM)
  Season$GAME_DATE <- ymd(Season$GAME_DATE)
  return(Season)
}
