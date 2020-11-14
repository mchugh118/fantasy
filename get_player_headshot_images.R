library(magick)
library(tidyverse)
library(tictoc)

getPlayerHeadshot <- function(gsis_id, headshot_url){
  i <- magick::image_read(headshot_url)
  i %>% 
    magick::image_write(path = paste0("player_headshot_images/", gsis_id, ".png"))
}

safe_getPlayerHeadshot <- safely(getPlayerHeadshot)

tic()
roster_2020 %>% 
  filter(!is.na(gsis_id) & !is.na(headshot_url)) %>% 
  purrr::walk2(.x = .$gsis_id,
               .y = .$headshot_url,
               .f = ~safe_getPlayerHeadshot(gsis_id = .x, headshot_url = .y))
  
toc()
