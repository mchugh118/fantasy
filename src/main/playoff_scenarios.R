library(jsonlite)
library(tictoc)

appendSeasonMatchups <- function(seasonMatchupsDf_raw_input){
  seasonMatchupsDf <- seasonMatchupsDf_raw_input %>% 
    group_by(matchup_week, matchup_id) %>% 
    mutate(winning_matchup_score = case_when(
      !is.na(points) ~ max(points, na.rm = T)),
      losing_matchup_score = case_when(
        !is.na(points) ~ min(points, na.rm = T)),
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
    mutate(week_max_points = case_when(
      !is.na(points) ~ max(points, na.rm = T)),
      team_strength = case_when(
        !is.na(points) & points > 0 ~ points / week_max_points)) %>% 
    ungroup()
  return(seasonMatchupsDf)
}

getSeasonTable <- function(seasonMatchupsDf_input){
  season_table_output <- seasonMatchupsDf_input %>% 
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
              .groups = "drop") %>% 
    arrange(-win_pct, -total_season_points_scored)
  return(season_table_output)
}

computeTiebreakers <- function(seasonTable_input, seasonMatchupsDf_input){
  complete_season_table_output <- seasonTable_input %>% 
    group_by(win_pct) %>% 
    distinct(display_name) %>% 
    mutate(opponent_display_name = display_name) %>% 
    expand(display_name, opponent_display_name) %>% 
    semi_join(x = seasonMatchupsDf_input, y = ., by = c("display_name", "opponent_display_name")) %>% 
    getSeasonTable() %>% 
    select(display_name, tiebreak_win_pct = win_pct, 
           tiebreak_total_points_scored = total_season_points_scored) %>% 
    left_join(x = seasonTable_input, y = .,
              by = "display_name") %>% 
    arrange(-win_pct, -tiebreak_win_pct) %>% 
    mutate(final_ranking = row_number())
  return(complete_season_table_output)
}

tic()
completed_weeks <- c(1:11)

seasonMatchups <- map_dfr(.x = 1:13, .f = ~getMatchupsList(lid, .x))
seasonMatchupsDf_raw <- rosterList %>% 
  as_tibble() %>% 
  select(owner_id, roster_id) %>% 
  left_join(seasonMatchups, by = c("roster_id")) %>% 
  arrange(matchup_week, roster_id) %>% 
  left_join(ownerList %>% select(user_id, display_name),
            by = c("owner_id" = "user_id")) %>% 
  mutate(matchup_name_id = paste0("week", matchup_week, "matchup", matchup_id)) %>% 
  ungroup() %>% 
  mutate(points = case_when(
    matchup_week %in% completed_weeks ~ points,
    !(matchup_week %in% completed_weeks) ~ NA_real_
  ))

completed_games <- seasonMatchupsDf_raw %>% 
  as_tibble() %>% 
  filter(!is.na(points))

incomplete_games <- seasonMatchupsDf_raw %>% 
  as_tibble() %>% 
  filter(is.na(points)) 

all_scenarios_week12 <- incomplete_games %>% 
  arrange(matchup_week, matchup_id) %>% 
  select(matchup_name_id, display_name) %>% 
  pivot_wider(names_from = matchup_name_id, values_from = display_name,
              values_fn = list) %>%
  unnest(cols = c(week12matchup1, week12matchup2, week12matchup3,
                  week12matchup4, week12matchup5, week12matchup6)) %>%
  expand(week12matchup1, week12matchup2, week12matchup3,
         week12matchup4, week12matchup5, week12matchup6) %>% 
  mutate(scenario = row_number()) %>% 
  group_by(scenario) %>% 
  nest() %>% 
  mutate(sim_season = map(.x = data,
                          .f = ~pivot_longer(data = .x, names_to = "matchup_name_id",
                                             values_to = "predicted_winner",
                                             cols = c(week12matchup1, week12matchup2, week12matchup3,
                                                      week12matchup4, week12matchup5, week12matchup6)) %>% 
                            right_join(incomplete_games, by = "matchup_name_id") %>% 
                            select(owner_id:display_name, matchup_name_id:predicted_winner) %>% 
                            mutate(points = case_when(
                              display_name == predicted_winner ~ 120,
                              display_name != predicted_winner ~ 100
                            )) %>% 
                            select(-predicted_winner) %>% 
                            bind_rows(completed_games) %>% 
                            appendSeasonMatchups),
         sim_table = map(.x = sim_season, .f = getSeasonTable),
         sim_table = map2(.x = sim_table, .y = sim_season,
                          .f = ~computeTiebreakers(.x, .y)))


final_table <- all_scenarios_week12 %>% 
  ungroup() %>% 
  unnest(sim_table) %>% 
  select(final_ranking, display_name) %>% 
  group_by(display_name) %>% 
  summarise(`1st` = length(final_ranking[final_ranking == 1]),
            `2nd` = length(final_ranking[final_ranking == 2]),
            `3rd` = length(final_ranking[final_ranking == 3]),
            `4th` = length(final_ranking[final_ranking == 4]),
            `5th` = length(final_ranking[final_ranking == 5]),
            `6th` = length(final_ranking[final_ranking == 6]),
            `7th` = length(final_ranking[final_ranking == 7]),
            `8th` = length(final_ranking[final_ranking == 8]),
            `9th` = length(final_ranking[final_ranking == 9]),
            `10th` = length(final_ranking[final_ranking == 10]),
            `11th` = length(final_ranking[final_ranking == 11]),
            `12th` = length(final_ranking[final_ranking == 12]),
            .groups = "keep"
  ) %>% 
  summarise_if(is.numeric, .funs = ~ ./4096, .groups = "keep") %>% 
  mutate(playoff_odds = pmap_dbl(.l = list(`1st`, `2nd`, `3rd`, `4th`, `5th`, `6th`),
                                 .f = sum))

final_table %>% 
  write_csv("output_files/final_table_raw.csv", na="")
