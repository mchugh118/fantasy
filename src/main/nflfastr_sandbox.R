library(tidyverse)
library(nflfastR)
library(lubridate)
library(reactable)
library(magick)
library(htmltools)
library(httr)

#Load data
seasons <- 2018:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

roster_2020 <- nflfastR::fast_scraper_roster(c(2020))


#Analysis
DK_Metcalf_54_yd_rec <- pbp %>% 
  filter(play_id == 1920, game_id == "2019_03_NO_SEA")

All_WR <- pbp %>% 
  filter(play_type == "pass", lubridate::year(game_date) == 2020,
         !is.na(receiver_id)) %>% 
  group_by(receiver_id) %>% 
  summarise(targets = n(),
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
  nflfastR::decode_player_ids() %>% 
  inner_join(roster_2020, by = c("receiver_id" = "gsis_id")) %>% 
  group_by(team) %>% 
  mutate(team_target_share = targets / sum(targets),
         team_air_yard_share = air_yards/ sum(air_yards),
         team_yac_share = yac / sum(yac)) %>% 
  ungroup() %>% 
  filter(targets > 50)

receiver_prod <- All_WR %>% 
  mutate(image_src = paste0("player_headshot_images/", receiver_id)) %>% 
  select(full_name, image_src, targets, receptions)

#htmltools::renderTags(img(test$image[[1]]))
##htmltools::ren
#test_tbl <- reactable(
#  test,
#  columns = list(
#    image = colDef(
##      cell = function(value, index){
 #       div(
#          class = 'team',
#          img(class = 'flag', src = test[index, "image"])
#        )
#      }
#    )
#  )
#)
i <- image_read(All_WR$headshot_url[[1]])
i
All_WR %>% 
  mutate(img = image_read(headshot_url))

reactable(All_WR)
i <- magick::image_read(All_WR$headshot_url[[1]])
magick::image_write(i, path = "player_headshot_images/test.png")
img_test <- GET(All_WR$headshot_url[[1]])
img_test %>% 
  content()

