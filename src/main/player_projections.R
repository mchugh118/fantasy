library(httr)
library(jsonlite)
#library(dplyr)
library(tidyverse)
library(devtools)
#devtools::install_github("FantasyFootballAnalytics/ffanalytics")
library(ffanalytics)

#Scrape projections, commenting out sites that don't have them yet
season_scrape <- ffanalytics::scrape_data(src = c(  
  "CBS",
  "ESPN",
  "FantasyData",
  "FantasyPros",
  #"FantasySharks",
  #"FleaFlicker",
  #"NumberFire",
  #"Yahoo"
  "FantasyFootballNerd",
  #"NFL",
  #"RTSports",
  "Walterfootball"
),
pos = c("QB", "RB", "WR", "TE", "K", "DST"),
season = year,
week = 0)

season_projections <- projections_table(data_result = season_scrape) %>%
  add_player_info()

positions <- playerList %>% 
  pluck("position") %>%
  unlist() %>%
  enframe() %>%
  select(player_id = name, position = value)

player_mapping_db <- playerDf %>%
  as_tibble() %>%
  inner_join(positions, by = "player_id") %>%
  rename(sleeper_player_id = player_id) %>%
  separate(player_name, c("first_name", "last_name"), " ", remove = F) %>%
  mutate_at(c("first_name", "last_name"), ~gsub('[[:punct:] ]+', "", .)) %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "K", "DST"))

player_table <- player_table %>%
  mutate_at(c("first_name", "last_name"), ~gsub('[[:punct:] ]+', "", .))

player_mapping_db <- player_mapping_db %>%
  left_join(player_table, by = c("first_name", "last_name", "position")) %>%
  select(sleeper_player_id, first_name, last_name, position, id, team)

test <- season_projections %>%
  left_join(player_mapping_db, by = c("first_name", "last_name", "position", "id", "team"))



