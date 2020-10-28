library(shiny)
library(httr)
library(jsonlite)
library(devtools)
library(tidyverse)
library(reactable)
library(htmltools)

##Should eventually source this but having issues
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

current_standings_cols <- c("wins", "losses")
rating_cols <- c("team_strength")
playoff_cols <- c("make_playoffs", "win_rd_1", "win_rd_2", "win_rd_3")

current_standings_column <- function(maxWidth = 100, ...){
    colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

rating_column <- function(maxWidth = 100, class = NULL, ...){
    colDef(class = "cell number", maxWidth = maxWidth, align = "center", ...)
}

playoff_column <- function(maxWidth = 100, class = NULL, ...){
    colDef(
        cell = format_pct,
        maxWidth = maxWidth,
        class = paste("cell number", class),
        style = function(value){
            if(value < 0.01){
                list(color = "#aaa")
            } else {
                list(color = "#111", background = knockout_pct_color(value))
            }
        }, ...
    )
}

format_pct <- function(value){
    if (value == 0)"  \u2013 "
    else if (value == 1) "\u2713"
    else if (value <= .01) "<1%"
    else if (value >= .99) ">99%"
    else formatC(paste0(round(value * 100), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1){
    get_color <- colorRamp(colors = colors, bias = bias)
    function(x) rgb(get_color(x), maxColorValue = 255)
}

knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
                                     bias = 2)
strength_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)


forecasts <- season_table %>% 
    mutate(make_playoffs = (team_strength - min(team_strength)) / (max(team_strength)-min(team_strength)),
           win_rd_1 = (team_strength - min(team_strength)) / (max(team_strength)-min(team_strength))/2,
           win_rd_2 = (team_strength - min(team_strength)) / (max(team_strength)-min(team_strength))/4,
           win_rd_3 = (team_strength - min(team_strength)) / (max(team_strength)-min(team_strength))/8,
           team_strength = team_strength * 100) %>% 
    select(display_name, all_of(current_standings_cols), all_of(rating_cols), all_of(playoff_cols))




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #add style elements
    tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback",
                  rel = "stylesheet"),
        tags$style(
            HTML("
                .standings {
                    font-family: Karla, 'Helvetica Neue', Helvetica, Arial, sans-serif;
                    font-size: 14px;
                }
                
                .header:hover,
                .header[aria-sort='ascending'],
                .header[aria-sort='descending'] {
                  background-color: #eee;
                }
            ")
        )
    ),

    # Application title
    titlePanel("Fantasy Football"),
    
    div(class = "standings",
        div(class = "title",
            h2("2020 Grandview Dynasty League"),
            "Current standings and playoff projections"),
        reactable::reactableOutput("season_table"),
        "Standings as of Week 6"
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    tbl <- reactable(
        forecasts,
        pagination = FALSE,
        defaultSorted = "team_strength",
        defaultSortOrder = "desc",
        defaultColDef = colDef(class = "cell", headerClass = "header"),
        columnGroups = list(
            colGroup(name = "Current Standings", columns = current_standings_cols),
            colGroup(name = "Rating", columns = rating_cols),
            colGroup(name = "Chances of ...", columns = playoff_cols)
        ),
        columns = list(
            display_name = colDef(name = "Team"),
            wins = current_standings_column(name = "Wins",
                                            format = colFormat(digits = 0)),
            losses = current_standings_column(name = "Losses",
                                              format = colFormat(digits = 0)),
            team_strength = rating_column(name = "Strength",
                                          format = colFormat(digits = 1)),
            make_playoffs = playoff_column(name = "Make Playoffs"),
            win_rd_1 = playoff_column(name = "Make Semi-finals",
                                      format = colFormat(digits = 1)),
            win_rd_2 = playoff_column(name = "Make Finals",
                                      format = colFormat(digits = 1)),
            win_rd_3 = playoff_column(name = "Win Finals",
                                      format = colFormat(digits = 1))
        )
    )
    
    
    output$season_table <- reactable::renderReactable(
        tbl
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
