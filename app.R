library(readxl)
library(tidyverse)
library(shiny)
library(data.table)
library(ggimage)
library(plotly)

formals(print.data.frame)$row.names <- FALSE
options(width = 9999)

seasons <- c(2001:2022)

i <- 0

for (i in seasons){
  assign(paste0("nbaLogs", i), read_xlsx(paste0(i, " NBA Season Logs.xlsx")))
}

logList <- mget(ls(pattern = "nbaLogs*"))

logs <- bind_rows(logList)



playerSeasonStats <- data.table(
  "Season" = logs$slugSeason,
  "Player" = logs$namePlayer,
  "Team" = logs$nameTeam,
  "MPG" = logs$minutes,
  "PTS" = logs$pts,
  "FGM" = logs$fgm,
  "FGA" = logs$fga,
  "FG%" = 0,
  "2PM" = logs$fg2m,
  "2PA" = logs$fg2a,
  "2P%" = 0,
  "3PM" = logs$fg3m,
  "3PA" = logs$fg3a,
  "3P%" = 0,
  "FTM" = logs$ftm,
  "FTA" = logs$fta,
  "FT%" = 0,
  "OReb" = logs$oreb,
  "DReb" = logs$dreb,
  "TReb" = logs$treb,
  "AST" = logs$ast,
  "STL" = logs$stl,
  "BLK" = logs$blk,
  "PF" = logs$pf,
  "TOV" = logs$tov,
  "BPM" = logs$plusminus,
  "URL" = logs$urlPlayerThumbnail,
  "graphURL" = logs$urlPlayerPhoto
  
)

playerSeasonStats <- subset(playerSeasonStats, playerSeasonStats$MPG > 0)

playerSeasonStats <- playerSeasonStats %>% group_by(Season, Team, Player, URL, graphURL) %>% 
  summarise ("Minutes Per Game" = mean(MPG), "Points" = mean(PTS),
             "Field Goals Made" = mean(FGM), "Field Goals Attepmted" = mean(FGA),
             "Field Goal Percentage" = sum(`FG%`), "2 Pointers Made" = mean(`2PM`),
             "2 Pointers Attempted" = mean(`2PA`), "2 Point Percentage" = sum(`2P%`),
             "3 Pointers Made" = mean(`3PM`),"3 Pointers Attempted" = mean(`3PA`),
             "3 Point Percentage" = sum(`3P%`), "Free Throws Made" = mean(FTM),
             "Free Throws Attempted" = mean(FTA), "Free Throw Percentage" = sum(`FT%`),
             "Offensive Rebounds" = mean(OReb), "Defensive Rebounds" = mean(DReb),
             "Total Rebounds" = mean(TReb), "Assists" = mean(AST), "Steals" = mean(STL),
             "Blocks" = mean(BLK), "Personal Fouls" = mean(PF), "Turnovers" = mean(TOV),
             BPM = sum(BPM))

playerGamesPlayed <- data.table(
  "Season" = logs$slugSeason,
  "Team" = logs$nameTeam,
  "Player" = logs$namePlayer,
  "GamesPlayed" = logs$numberGamePlayerSeason,
  "MPG" = logs$minutes
)

playerGamesPlayed <- aggregate(GamesPlayed ~ Season + Team + Player, data = playerGamesPlayed, FUN = max, na.rm = TRUE)

playerSeasonStats <- merge(playerSeasonStats, playerGamesPlayed, by = c("Team", "Season", "Player"))


playerSeasonStats[10] <- (playerSeasonStats[8]/playerSeasonStats[9]) * 100
playerSeasonStats[13] <- (playerSeasonStats[11]/playerSeasonStats[12]) * 100
playerSeasonStats[16] <- (playerSeasonStats[14]/playerSeasonStats[15]) * 100
playerSeasonStats[19] <- (playerSeasonStats[17]/playerSeasonStats[18]) * 100

playerSeasonStats[is.na(playerSeasonStats)] <- 0

player.is.inf <- sapply(playerSeasonStats, is.infinite)
playerSeasonStats[player.is.inf] <- 0

player.is.num <- sapply(playerSeasonStats, is.numeric)
playerSeasonStats[player.is.num] <- lapply(playerSeasonStats[player.is.num], round , 2)

playerSeasonStats$Team <- replace(playerSeasonStats$Team, playerSeasonStats$Team == "New Orleans Hornets", "New Orleans Pelicans")
playerSeasonStats$Team <- replace(playerSeasonStats$Team, playerSeasonStats$Team == "Charlotte Bobcats", "Charlotte Hornets")
playerSeasonStats$Team <- replace(playerSeasonStats$Team, playerSeasonStats$Team == "New Jersey Nets", "Brooklyn Nets")
playerSeasonStats$Team <- replace(playerSeasonStats$Team, playerSeasonStats$Team == "LA Clippers", "Los Angeles Clippers")
playerSeasonStats$Team <- replace(playerSeasonStats$Team, playerSeasonStats$Team == "Seattle SuperSonics", "Oklahoma City Thunder")
playerSeasonStats$Team <- replace(playerSeasonStats$Team, playerSeasonStats$Team == "Vancouver Grizzlies", "Memphis Grizzlies")
playerSeasonStats$Team <- replace(playerSeasonStats$Team, playerSeasonStats$Team == "New Orleans/Oklahoma City Hornets", "New Orleans Pelicans")

playerTeamColors <- read_xlsx("NBA Team Colors.xlsx")

playerSeasonStats <- merge(playerSeasonStats, playerTeamColors, by = "Team")


teamSeasonStats <- data.table(
  "Season" = logs$slugSeason,
  "Team" = logs$nameTeam,
  "PTS" = logs$pts,
  "FGM" = logs$fgm,
  "FGA" = logs$fga,
  "FG%" = 0,
  "2PM" = logs$fg2m,
  "2PA" = logs$fg2a,
  "2P%" = 0,
  "3PM" = logs$fg3m,
  "3PA" = logs$fg3a,
  "3P%" = 0,
  "FTM" = logs$ftm,
  "FTA" = logs$fta,
  "FT%" = 0,
  "Reb" = logs$treb,
  "AST" = logs$ast,
  "STL" = logs$stl,
  "BLK" = logs$blk,
  "TOV" = logs$tov
)

teamSeasonStats <- teamSeasonStats %>% group_by(Season, Team)%>% 
  summarise (PTS = sum(PTS), FGM = sum(FGM), FGA = sum(FGA), `FG%` = sum(`FG%`),
             `2PM` = sum(`2PM`), `2PA` = sum(`2PA`), `2P%` = sum(`2P%`), `3PM` = sum(`3PM`),
             `3PA` = sum(`3PA`), `3P%` = sum(`3P%`), FTM = sum(FTM), FTA = sum(FTA),
             `FT%` = sum(`FT%`), Reb = sum(Reb),
             AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TOV = sum(TOV))

teamGamesPlayed <- data.table(
  "Season" = logs$slugSeason,
  "Team" = logs$nameTeam,
  "GamesPlayed" = logs$numberGameTeamSeason
)

teamGamesPlayed <- aggregate(GamesPlayed ~ Season + Team, data = teamGamesPlayed, FUN = max, na.rm = TRUE)

teamSeasonStats <- merge(teamSeasonStats, teamGamesPlayed, by = c("Team", "Season"))

teamGamesWon <- data.table(
  "Season" = logs$slugSeason,
  "Team" = logs$nameTeam,
  "isWin" = logs$isWin
)

teamGamesWon <- aggregate(as.integer(isWin) ~ Season + Team, data = teamGamesWon, FUN = mean, na.rm = TRUE)
colnames(teamGamesWon)[3] <- "WinRate"

teamSeasonStats <- merge(teamSeasonStats, teamGamesWon, by = c("Team", "Season"))
teamSeasonStats$Wins <- round(teamSeasonStats$WinRate * teamSeasonStats$GamesPlayed)
teamSeasonStats$WinRate <- NULL

teamSeasonStats[is.na(teamSeasonStats)] <- 0

teamSeasonStats[3:20] <- teamSeasonStats[3:20] / teamSeasonStats[,21]

teamSeasonStats[7] <- (teamSeasonStats[5]/teamSeasonStats[6]) * 100 
teamSeasonStats[10] <- (teamSeasonStats[8]/teamSeasonStats[9]) * 100 
teamSeasonStats[13] <- (teamSeasonStats[11]/teamSeasonStats[12]) * 100  
teamSeasonStats[16] <- (teamSeasonStats[14]/teamSeasonStats[15]) * 100 

team.is.inf <- sapply(teamSeasonStats, is.infinite)
teamSeasonStats[team.is.inf] <- 0

team.is.num <- sapply(teamSeasonStats, is.numeric)
teamSeasonStats[team.is.num] <- lapply(teamSeasonStats[team.is.num], round , 2)

teamSeasonStats$Record <- paste0(as.character(teamSeasonStats$Wins), '-', 
                                 as.character(teamSeasonStats$GamesPlayed - teamSeasonStats$Wins))

teamSeasonStats$Wins <- NULL
teamSeasonStats <- teamSeasonStats[, c(1:2, 22, 3:21)]

teamSeasonStats$Team <- replace(teamSeasonStats$Team, teamSeasonStats$Team == "New Orleans Hornets", "New Orleans Pelicans")
teamSeasonStats$Team <- replace(teamSeasonStats$Team, teamSeasonStats$Team == "Charlotte Bobcats", "Charlotte Hornets")
teamSeasonStats$Team <- replace(teamSeasonStats$Team, teamSeasonStats$Team == "New Jersey Nets", "Brooklyn Nets")
teamSeasonStats$Team <- replace(teamSeasonStats$Team, teamSeasonStats$Team == "LA Clippers", "Los Angeles Clippers")
teamSeasonStats$Team <- replace(teamSeasonStats$Team, teamSeasonStats$Team == "Seattle SuperSonics", "Oklahoma City Thunder")
teamSeasonStats$Team <- replace(teamSeasonStats$Team, teamSeasonStats$Team == "Vancouver Grizzlies", "Memphis Grizzlies")
teamSeasonStats$Team <- replace(teamSeasonStats$Team, teamSeasonStats$Team == "New Orleans/Oklahoma City Hornets", "New Orleans Pelicans")

metricList <- c(colnames(playerSeasonStats[7:27]))

ui <- fluidPage(
  title = "NBA Stats",
  titlePanel(h1("NBA Team and Player Stats, 2000 - Present", style = 'text-align: center ; color:white')),
  tags$style(HTML("
                  body {
                  background-image: linear-gradient(to bottom right, #000080, #87ceeb);
                  background-repeat: no-repeat;
                  background-size: 100% auto;
                  background-position: center;
                  }
                  ")),
  sidebarPanel(
    selectInput("team", "Team", choices = sort(unique(teamSeasonStats$Team)), selected = "San Antonio Spurs"),
    selectInput("season", "Season", choices = sort(unique(teamSeasonStats$Season)), selected = "2020-21"),
    selectInput("metric", "Display Metric", choices = sort(metricList), selected = "Points"), width = 2
  ),
  
  mainPanel(
    headerPanel(""),
    headerPanel(""),
    span(textOutput("teamStatsHeader"), style = "font-size:22px; color:white;"),
    verbatimTextOutput("teamStats"),
    headerPanel(""),
    headerPanel(""),
    span(textOutput("teamLeadersHeader"), style = "font-size:22px; color:white;"),
    tableOutput("teamLeaders"),
    tags$head(tags$style("#teamLeaders table {background-color: #f5f5f5;}", media="screen", type="text/css")),
    headerPanel(""),
    fluidRow(
      column(3,
             span(textOutput("metricLeaderHeader"), style = "font-size:22px; color:white;"),
             htmlOutput("picture")
      ),
      column(2),
      column(7,
             span(textOutput("metricGraphHeader"), style = "font-size:22px; color:white;"),
             plotlyOutput("metricGraph")
      )
      
      
    ),
    headerPanel(""),
    headerPanel(""),
    span(textOutput("bpmHeader"), style = "font-size:22px; color:white;"),
    plotOutput("bpmPlot", height = 1100)
  ),
)

server <- function(input, output, session){
  
  team <- reactive({
    subset(teamSeasonStats, teamSeasonStats$Team == input$team
           & teamSeasonStats$Season == input$season)
  })
  
  player <- reactive({
    subset(playerSeasonStats, playerSeasonStats$Team == input$team
           & playerSeasonStats$Season == input$season)
  })
  
  output$teamStatsHeader <- renderText(paste0(input$team, " Summary Statistics, ", input$season, " Season:"))
  output$teamStats <- renderPrint(print(team()[1:21]))
  
  output$teamLeadersHeader <- renderText(paste0(input$team, " Scoring Leaders, ", input$season, " Season:"))
  output$teamLeaders <- renderTable(head(select(player()[order(-player()["Points"]),],
                                                c('Player', 'GamesPlayed', 'Minutes Per Game', 'Points',
                                                  'Offensive Rebounds', 'Defensive Rebounds', 'Total Rebounds',
                                                  'Assists', 'Steals', 'Blocks',
                                                  'Personal Fouls', 'Turnovers'))), width = 1225 )
  
  output$metricLeaderHeader <- renderText({
  
        playerSort <- player()[order(-player()[input$metric]),]
        paste0(playerSort[1,]$Player, " is the ", input$metric, " Leader for The ",
               input$season, " ", input$team)
  })
  
  output$picture <- renderText({
  
        playerSortImg <- player()[order(-player()[input$metric]),]
        c(' <img src="', paste0(playerSortImg[1,]$URL), '">')
      
  })
  
  output$metricGraphHeader <- renderText(paste0(input$season, " ", input$team, " ", input$metric, " By Player:"))
  
  output$metricGraph <- renderPlotly({
    
        p <- ggplot(player(), aes(player()[["Minutes Per Game"]], player()[[paste0(input$metric)]])) +
          ggtitle(paste0(input$season, " ", input$team, " MPG Vs. ", input$metric)) +
          geom_point(aes(text = paste(player()$Player)), shape = 23, fill = paste0(player()$Color), colour = paste0(player()$Color2),  size = 3) +
          labs(x = "Minutes Per Game", y = paste0(input$metric))
        
        ggplotly(p, width = 700 )      
  })
  
  output$bpmHeader <- renderText(paste0(input$season, " ", input$team, " Total Box Plus-Minus Leaders:"))
  
  output$bpmPlot <- renderPlot({
    test <- ggplot(player(), aes(x =BPM, y = reorder(Player, BPM))) +
      geom_col(fill = ifelse(player()$BPM >= 0, paste0(player()$Color), paste0(player()$Color2))) +
      geom_image(aes(image = graphURL) , size = 0.07, by = "height") +
      geom_text(aes(x = BPM, y = Player, label = paste(player()$Player,player()$BPM, sep = "\n")),
                nudge_x = ifelse(player()$BPM >= 0,
                                 ifelse(max(player()$BPM) >= 100, (.2 * max(player()$BPM) ), 40),
                                 ifelse(min(player()$BPM) <= -100, (-.2 * max(player()$BPM)), -40)
                ),
                size = 5) +
      xlim(ifelse(min(player()$BPM) >= -100, -165, min(player()$BPM) * 1.65),
           ifelse(max(player()$BPM) <= 100, 165, max(player()$BPM) * 1.65)) +
      
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) +
      labs(x = "Box Plus/Minus", y = "") 
    
    
    print(test)
    
  }, res = .96)
  
}

shinyApp(ui, server)




