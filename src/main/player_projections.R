#devtools::install_github("FantasyFootballAnalytics/ffanalytics")
#install.packages("nflfastR")
#install.packages('reclin')
library(httr)
library(jsonlite)
library(tidyverse)
library(devtools)
library(ffanalytics)
library(nflfastR)
library(janitor)
library(lubridate)
library(reclin)
library(reactable)

#Scrape projections, commenting out sites that don't have them yet
week_8_scrape <- ffanalytics::scrape_data(
  src = c(
    "CBS",
    "ESPN",
    "FantasyData",
    "FantasyPros",
    "FantasySharks",
    "FFToday",
    "NumberFire",
    "Yahoo",
    "FantasyFootballNerd",
    "NFL"
  ),
  pos = c("QB", "RB", "WR", "TE", "K", "DST"),
  season = 2020,
  week = 8)

#Calculate projections and add player info
week_8_projections <- projections_table(data_result = week_8_scrape) %>%
  add_player_info() %>% 
  filter(avg_type == "robust")

#Get all players in the ffanalytics player database (to be used for matching)
ff_player_data <- ffanalytics::ff_player_data


#Get all players on current rosters
playersOnRosters <- rosterList %>% select(players) %>% pull() %>% unlist()

#Create Df of all players and relevant information
playerMappingDf <- tibble(
  sleeperPlayerID = playerList %>% names(),
  sleeperPlayerName = paste0(map_chr(playerList, c("first_name"), .default = NA),
                             " ", map_chr(playerList, c("last_name"), .default = NA)),
  sleeperPlayerPosition = map_chr(playerList, c("position"), .default = NA),
  sleeperPlayerTeam = map_chr(playerList, c("team"), .default = NA),
  birthdate = map_chr(playerList, c("birth_date"), .default = NA) %>% 
    lubridate::ymd(),
  espn_id = map_dbl(playerList, c("espn_id"), .default = NA)) %>% 
  group_by(espn_id) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(xor(sleeperPlayerPosition == "DEF", n < 2)) %>% 
  select(-n) %>% 
  filter(sleeperPlayerPosition %in% c("QB", "RB", "WR", "TE", "K", "DEF")) %>% 
  filter(sleeperPlayerID %in% playersOnRosters)

#Get team abbreviations from sleeper
sleeper_team_abbrev <- playerMappingDf %>%
  filter(!is.na(sleeperPlayerTeam)) %>%
  distinct(sleeperPlayerTeam) %>%
  filter(sleeperPlayerTeam != "OAK") %>% 
  arrange(sleeperPlayerTeam)

#Get team abbreviations from ffanalytics
ff_analytics_team_abbrev <- ffanalytics::scrape_data(
  src = c(
    "FantasySharks"),
  pos = c("DST"),
  season = 2020,
  week = 0) %>% 
  projections_table() %>%
  add_player_info() %>%
  distinct(team) %>% 
  arrange(team)

#Bind columns for a lookup table (same number of rows)
team_abbrev_mapping <- sleeper_team_abbrev %>% bind_cols(ff_analytics_team_abbrev)
sleeperPositionVec <- playerMappingDf %>% distinct(sleeperPlayerPosition) %>% dplyr::pull()

#Gather and prepare ffanalytics table for matching
proj_player_table <- week_8_projections %>% 
  select(id, first_name, last_name, team, position) %>% 
  distinct(.keep_all = T) %>% 
  mutate(name = paste(first_name, last_name)) %>% 
  left_join(team_abbrev_mapping, by = "team") %>% 
  mutate(position = case_when(
           position == "DST" ~ "DEF",
           TRUE ~ position)
         ) %>% 
  filter(position %in% sleeperPositionVec) %>% 
  select(id, sleeperPlayerName = name, sleeperPlayerPosition = position,
         sleeperPlayerTeam)


#Run probabilistic matching on the two sets and gather one result for each sleeperID
fullPlayerMappingDF <- reclin::pair_blocking(playerMappingDf, proj_player_table,
                               c("sleeperPlayerTeam", "sleeperPlayerPosition")) %>% 
  compare_pairs(by = c("sleeperPlayerName"), default_comparator = jaro_winkler(0.9)) %>% 
  score_problink(var = "weight") %>% 
  select_threshold(threshold = 5) %>% 
  select_n_to_m() %>% 
  link() %>% 
  filter(!is.na(sleeperPlayerID)) %>% 
  select(sleeperPlayerID, ffanalyticsPlayerID = id,
         sleeperPlayerName = sleeperPlayerName.x,
         sleeperPlayerPosition = sleeperPlayerPosition.x,
         sleeperPlayerTeam = sleeperPlayerTeam.x,
         ffanalyticsPlayerName = sleeperPlayerName.y)


#Join projections Df with playerMappings
sleeperWeek8_projections <- fullPlayerMappingDF %>% 
  left_join(week_8_projections, by = c("ffanalyticsPlayerID" = "id"))

##To do: 
### - add our specific rules for projections calculations
### - deal with NA values for inactive players (adding in 0's)
### - add in player stuff into the app





