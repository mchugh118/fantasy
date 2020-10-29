# Dynasty Fantasy Football Predictions
# Willis Day and Pat McHugh
# May 7, 2020

library(httr)
library(jsonlite)
#library(dplyr)
#library(tidyverse)
library(devtools)
library(tidyverse)



lids <- c("515645615373778944")
years <- c(2020)
lid <- lids[1]
year <- years[1]
league <- GET(paste("https://api.sleeper.app/v1/league/", lids[1], sep=""))
leagueList <- fromJSON(content(league,as="text"))

getOwnerList <- function(lid){
  owners = GET(paste("https://api.sleeper.app/v1/league/", lid, "/users", sep=""))
  ownerList <- fromJSON(content(owners,as="text"))
  return (ownerList)
}

getRosterList <- function(lid){
  rosters = GET(paste("https://api.sleeper.app/v1/league/", lid, "/rosters", sep=""))
  rosterList <- fromJSON(content(rosters,as="text"))
  return (rosterList)
}

getPlayerList <- function(lid){
  players = GET("https://api.sleeper.app/v1/players/nfl")
  playerList <- fromJSON(content(players,as="text"))
  return (playerList)
}

getRosterDf <- function(rosterList, playerDf){
  rosterDf <- data.frame(owner_id=rosterList$owner_id)
  starters <- do.call(rbind, rosterList$starters)
  nStarters <- ncol(starters)
  fullRosters <- do.call(rbind, rosterList$players)
  rosterSize <- ncol(fullRosters)
  nReserves <- rosterSize - nStarters
  rostersWithNames <- matrix(nrow=nrow(rosterDf), ncol=rosterSize*2)
  for (i in 1:nrow(rosterDf)){
    reserves <- setdiff(fullRosters[i,], starters[i,])
    roster <- c(starters[i,], reserves)
    length(roster) <- rosterSize
    rosterNames <- unname(unlist(sapply(roster, function(x){playerVec[which(names(playerVec) == x)]})))
    length(rosterNames) <- rosterSize
    
    newRow <- c(rbind(roster, rosterNames))
    rostersWithNames[i,] <- newRow
  }
  rosterDf <- cbind(rosterDf, rostersWithNames)
  idColNames <- c(paste("starter_", 1:nStarters, sep=""), paste("reserve_", 1:nReserves, sep=""))
  nameColNames <- c(paste("starter_", 1:nStarters, "_name", sep=""), paste("reserve_", 1:nReserves, "_name", sep=""))
  
  colnames(rosterDf) <- c("owner_id", c(rbind(idColNames, nameColNames)))
  return (rosterDf)
}


getMatchupsList <- function(lid, matchup_week){
  matchups = GET(paste("https://api.sleeper.app/v1/league/", lid, "/matchups/", matchup_week, sep=""))
  matchupsList <- fromJSON(content(matchups,as="text")) %>% 
    mutate(matchup_week = matchup_week)
  return (matchupsList)
}

ownerList <- getOwnerList(lid)
rosterList <- getRosterList(lid)
playerList <- getPlayerList(lid)
ownerDf <- data.frame(owner_id=ownerList$user_id, owner_name=ownerList$display_name, 
                      owner_index=1:length(ownerList$user_id), stringsAsFactors = F)
playerVec <- sapply(playerList, function(x){if (exists("full_name", where=x)){return (x[["full_name"]])} else {return (x[["player_id"]])}})
playerDf <- data.frame(player_id=names(playerList), player_name=unlist(unname(playerVec)))

rosterDf <- getRosterDf(rosterList, playerDf)

ownersAndRosters <- merge(ownerDf, rosterDf, by="owner_id")

seasonMatchups <- map_dfr(.x = 1:12, .f = ~getMatchupsList(lid, .x))
seasonMatchupsDf <- rosterList %>% 
  select(owner_id, roster_id) %>% 
  left_join(seasonMatchups, by = c("roster_id")) %>% 
  arrange(matchup_week, roster_id) %>% 
  left_join(ownerList %>% select(user_id, display_name),
            by = c("owner_id" = "user_id")) %>% 
  group_by(matchup_week, matchup_id) %>% 
  mutate(winning_matchup_score = max(points, na.rm = T),
         losing_matchup_score = min(points, na.rm = T),
         win = case_when(
           points == 0 ~ 0,
           winning_matchup_score == points ~ 1,
           losing_matchup_score == points ~ 0),
         loss = case_when(
           points == 0 ~ 0,
           losing_matchup_score == points ~ 1,
           winning_matchup_score == points ~ 0),
         opponent_points = case_when(
           win == 1 ~ losing_matchup_score,
           loss == 1 ~ winning_matchup_score),
         opponent_display_name = str_c(display_name, collapse = "")) %>%
  ungroup() %>% 
  mutate(opponent_display_name = map2_chr(.x = opponent_display_name,
                                          .y = display_name,
                                          .f = ~sub(.y, "", .x))) %>% 
  group_by(matchup_week) %>% 
  mutate(week_max_points = max(points, na.rm = T),
         team_strength = case_when(
          points > 0 ~ points / week_max_points)) %>% 
  ungroup()
  
season_table <- seasonMatchupsDf %>% 
  group_by(owner_id, display_name) %>% 
  summarise(wins = sum(win, na.rm = T),
            losses = sum(loss, na.rm = T),
            win_pct = wins / (wins + losses),
            season_high_score = max(points, na.rm = T),
            total_season_points_scored = sum(points, na.rm = T),
            total_season_points_against = sum(opponent_points, na.rm = T),
            avg_ppg = mean(points, na.rm = T),
            avg_opponent_ppg = mean(opponent_points, na.rm = T),
            team_strength = mean(team_strength, na.rm = T),
            .groups = "drop")

