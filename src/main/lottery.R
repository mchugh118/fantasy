# Lottery 
library(ggplot2)
set.seed(456)

# Parameters
ballsPerTeam <- c(20480, 10240, 5120, 2560, 1280, 640, 320, 160, 80, 40, 20, 10)
maxSpotsMoveUp <- 3
maxSpotsSlide <- 3

invertedStandings <- c("Willis", "Josh C", "Evan", "Brendan", "Tristen", "Pinyard", "Jack", "Pat", "Josh K", "Ryan", "Ben", "Nick")
numPlayers <- 12

# Build the lottery balls vector
lotteryBalls <- c()
for (i in 1:numPlayers){
  lotteryBalls <- c(lotteryBalls, rep(invertedStandings[i], ballsPerTeam[i]))
}

runLottery <- function(lotteryBalls, maxSpotsMoveUp, maxSpotsSlide, invertedStandings, numPlayers){
  draftOrder <- c()
  while (length(draftOrder) < numPlayers){
    draftSlotBeingChosen <- length(draftOrder) + 1
    
    # Check if anyone has slid the maximum number of spots in the lottery and needs to be added
    topRemainingPlayer <- setdiff(invertedStandings, draftOrder)[1]
    topPlayerOddsRank <- which(invertedStandings == topRemainingPlayer)
    if ((topPlayerOddsRank + maxSpotsSlide) <= draftSlotBeingChosen){
      ball <- topRemainingPlayer
      draftOrder <- c(draftOrder, ball)
      lotteryBalls <- lotteryBalls[-which(lotteryBalls == ball)]
    } else {
      ball <- sample(lotteryBalls, 1)
      playerOddsRank <- which(invertedStandings == ball)  
      if ((playerOddsRank - maxSpotsMoveUp) <= draftSlotBeingChosen){
        draftOrder <- c(draftOrder, ball)
        lotteryBalls <- lotteryBalls[-which(lotteryBalls == ball)]
      }
    }
  }
  return (draftOrder)
}

# Display distribution of results
showOdds <- function(nSims=10000, lotteryBalls, numPlayers, maxSpotsMoveUp, maxSpotsSlide, invertedStandings){
  nSims <- 10000
  lotterySims <- matrix(nrow=nSims,ncol=numPlayers)
  for (i in 1:nSims){
    lotterySims[i,] <- runLottery(lotteryBalls, maxSpotsMoveUp, maxSpotsSlide, invertedStandings, numPlayers)
  }
  
  for (i in 1:numPlayers){
    player <- invertedStandings[i]
    oddsPerSlot <- (apply(lotterySims, 2, function(x){length(which(x == player))})/nSims)*100
    df <- data.frame(pick=factor(1:numPlayers), odds=oddsPerSlot)
    p <- ggplot(data=df, aes(x=pick, y=odds)) +
      geom_bar(stat="identity", fill="steelblue")+
      ylim(0, 100)+
      geom_text(aes(label=odds), vjust=-0.3, size=3.5)+
      ggtitle(paste("Lottery Rank ", i, " (", player, ")", sep=""))+
      theme_minimal()
    print(p)
  }
}

nSims <- 10000
showOdds(nSims, lotteryBalls, numPlayers, maxSpotsMoveUp, maxSpotsSlide, invertedStandings)

showFullResults <- function(results){
  print("If these results are displayed all at once before they are revealed pick by pick, the lottery is VOID")
  print(results)
}

showResultsOneAtATime <- function(results, numPlayers){
  for (i in 1:numPlayers){
    pickNum <- numPlayers - i + 1
    player <- results[pickNum]
    nextPick <- readline(prompt="Reveal Next Pick")
    cat("Pick ", pickNum, ": ", player, "\n", sep="")
  }
}








results <- runLottery(lotteryBalls, maxSpotsMoveUp, maxSpotsSlide, invertedStandings, numPlayers)
showFullResults(results)
showResultsOneAtATime(results, numPlayers)
