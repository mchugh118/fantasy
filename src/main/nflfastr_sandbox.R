library(tidyverse)
library(nflfastR)
library(lubridate)

#Load data
seasons <- 2018:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

DK_Metcalf_54_yd_rec <- pbp %>% 
  filter(play_id == 1920, game_id == "2019_03_NO_SEA")

All_WR <- pbp %>% 
  filter(play_type == "pass", lubridate::year(game_date) == 2020,
         !is.na(receiver_id)) %>% 
  group_by(receiver_id) %>% 
  summarise(name = first(receiver_player_name),
            team = last(posteam),
            targets = n(),
            receptions = sum(complete_pass, na.rm = T),
            air_yards = sum(air_yards, na.rm = T),
            yac = sum(yards_after_catch, na.rm = T),
            air_yards_per_target = air_yards/targets,
            yac_per_target = yac/targets,
            cum_epa = sum(epa, na.rm = T),
            epa_per_target = cum_epa/targets,
            xyac_median_per_target = sum(xyac_median_yardage, na.rm = T)/targets,
            yac_over_expected = yac_per_target - xyac_median_per_target) %>% 
  arrange(-cum_epa) %>% 
  group_by(team) %>% 
  mutate(team_target_share = targets / sum(targets),
         team_air_yard_share = air_yards/ sum(air_yards),
         team_yac_share = yac / sum(yac)) %>% 
  filter(targets > 20)




