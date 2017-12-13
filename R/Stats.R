#' Calculate Offensive and Defensive stats for teams
#'
#' This function takes an NBA season object and calculates the Offensive and
#' Defensive stats for teams
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @return a list of two dataframes, one with the offensive team stats and the
#' other with defensive team stats
#' @examples
#' \dontrun{
#' data("season2017")
#' TeamStats(Seasondata = season2017)
#' }
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_replace
#' @importFrom tidyr spread
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export

TeamStats <- function(Seasondata){
  offstats <- Seasondata %>% mutate(EVENT_TYPE = make.names(EVENT_TYPE), SHOT_TYPE = str_replace(SHOT_TYPE, " Field Goal", ""))  %>% group_by(TEAM_NAME, SHOT_TYPE, EVENT_TYPE) %>% summarise(N = n()) %>% spread(key = EVENT_TYPE, value = N) %>% split(.$SHOT_TYPE) %>%  map(~mutate(.x, ShotPct = round(Made.Shot/(Made.Shot +Missed.Shot), 3), Total = Made.Shot + Missed.Shot))
  Points <- c("2Pts", "3Pts")
  for(i in 1:length(offstats)){
    colnames(offstats[[i]])[2:6] <- paste0(colnames(offstats[[i]])[2:6], Points[i])
  }
  offstats <- offstats %>% reduce(merge) %>% select(-SHOT_TYPE2Pts, -SHOT_TYPE3Pts, -Made.Shot2Pts, -Made.Shot3Pts, -Missed.Shot2Pts, -Missed.Shot3Pts) %>% mutate(TotalShots = Total2Pts + Total3Pts) %>% mutate(PercentageOf2s = round(Total2Pts/TotalShots,2),PercentageOf3s = round(Total3Pts/TotalShots, 2)) %>% select(-TotalShots, -Total2Pts, -Total3Pts)
  Teams <- unique(Seasondata$TEAM_NAME)

  Prop <- data.frame(TEAM_NAME = Teams , PropShot = NA)

  for(i in 1:30){
    Offa <- filter(Seasondata, HTM == Teams[i] | VTM == Teams[i])
    Prop$PropShot[i] <- round(nrow(filter(Offa, TEAM_NAME == Teams[i]))/nrow(filter(Offa,TEAM_NAME != Teams[i])),3)
  }
  offstats <- full_join(offstats, Prop) %>% mutate(PPS = round((ShotPct2Pts*2*PercentageOf2s)+(ShotPct3Pts*3*PercentageOf3s),3)) %>% mutate(AdjPPS = round((PPS * PropShot),3)) %>% arrange(desc(AdjPPS))
  defstats <- Seasondata %>% mutate(EVENT_TYPE = make.names(EVENT_TYPE), SHOT_TYPE = str_replace(SHOT_TYPE, " Field Goal", ""))

  defstats$DefTeam <-NA

  for (i in 1:nrow(defstats)){
    defstats$DefTeam[i] <- c(defstats$HTM[i],defstats$VTM[i])[c(defstats$HTM[i],defstats$VTM[i]) != defstats$TEAM_NAME[i]]
  }

  Prop <- data.frame(DefTeam = Teams , PropShot = NA)

  for(i in 1:30){
    Defa <- dplyr::filter(defstats, HTM == Teams[i] | VTM == Teams[i])
    Prop$PropShot[i] <- round(nrow(dplyr::filter(Defa, DefTeam == Teams[i]))/nrow(dplyr::filter(Defa,DefTeam != Teams[i])),3)
  }


  defstats <- defstats %>% group_by(DefTeam, SHOT_TYPE, EVENT_TYPE) %>% dplyr::summarise(N = n()) %>% spread(key = EVENT_TYPE, value = N) %>% split(.$SHOT_TYPE) %>%  map(~mutate(.x, ShotPct = round(Made.Shot/(Made.Shot +Missed.Shot), 3), Total = Made.Shot + Missed.Shot))

  Points <- c("2Pts", "3Pts")

  for(i in 1:length(defstats)){
    colnames(defstats[[i]])[2:6] <- paste0(colnames(defstats[[i]])[2:6], Points[i])
  }

  defstats <- defstats %>% reduce(merge) %>% select(-SHOT_TYPE2Pts, -SHOT_TYPE3Pts, -Made.Shot2Pts, -Made.Shot3Pts, -Missed.Shot2Pts, -Missed.Shot3Pts) %>% mutate(TotalShots = Total2Pts + Total3Pts) %>% mutate(PercentageOf2s = round(Total2Pts/TotalShots,2),PercentageOf3s = round(Total3Pts/TotalShots, 2)) %>% select(-TotalShots, -Total2Pts, -Total3Pts)
  Teams <- unique(Seasondata$TEAM_NAME)

  defstats <- full_join(defstats, Prop) %>% mutate(PPS = round((ShotPct2Pts*2*PercentageOf2s)+(ShotPct3Pts*3*PercentageOf3s),3)) %>% mutate(AdjPPS = round((PPS * PropShot),3)) %>% arrange(AdjPPS)

  return(list(offstats = offstats, defstats = defstats))
}




