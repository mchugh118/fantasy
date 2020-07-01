library(shiny)
library(httr)
library(jsonlite)
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


ownerList <- getOwnerList(lid)
rosterList <- getRosterList(lid)
playerList <- getPlayerList(lid)
ownerDf <- data.frame(owner_id=ownerList$user_id, owner_name=ownerList$display_name, 
                      owner_index=1:length(ownerList$user_id), stringsAsFactors = F)
playerVec <- sapply(playerList, function(x){if (exists("full_name", where=x)){return (x[["full_name"]])} else {return (x[["player_id"]])}})
playerDf <- data.frame(player_id=names(playerList), player_name=unlist(unname(playerVec)))

rosterDf <- getRosterDf(rosterList, playerDf)

ownersAndRosters <- merge(ownerDf, rosterDf, by="owner_id")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fantasy Football"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
            h3("Nothing yet")
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
           tableOutput("owner_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$owner_table <- renderTable({
       ownersAndRosters %>% 
            select(owner_id, owner_name, owner_index)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
