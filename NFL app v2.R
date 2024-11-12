source("/Users/mukappa/Documents/Coding/r-useful-funs/sourcewd.R")
sourcewd()
NFLweek <- 10

library(grid)
library(png)
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)

scoreSheets <- do.call(rbind, lapply(1:NFLweek, function(x){
  outp <- readxl::read_xlsx("NFL 2024 results.xlsx", sheet = x)
  outp$Week <- x
  outp[complete.cases(outp), ]
}))
scoreSheets$Game_ID <- 1:nrow(scoreSheets)
scoreSheets <- scoreSheets %>% mutate(Gross_PD = abs(`Visitors score` - `Home score`))

NFLtable <- readxl::read_xlsx("NFL table.xlsx")
NFLtable$`Win history` <- ""
NFLtable$`Location history` <- ""

predictionRecord <- data.frame()

for(i in 1:nrow(scoreSheets)){
  print(i)
  currResult <- scoreSheets[i, ]
  visitors <- currResult$`Visitors team`
  hosts <- currResult$`Home team`
  # can just calculate this, so just do wins/losses
  NFLtable[NFLtable$Team == visitors, ]$Played <- NFLtable[NFLtable$Team == visitors, ]$Played + 1 
  NFLtable[NFLtable$Team == hosts, ]$Played <- NFLtable[NFLtable$Team == hosts, ]$Played + 1
  NFLtable[NFLtable$Team == visitors, ]$`Location history` <- paste0(NFLtable[NFLtable$Team == visitors, ]$`Location history`, "A")
  NFLtable[NFLtable$Team == hosts, ]$`Location history` <- paste0(NFLtable[NFLtable$Team == hosts, ]$`Location history`, "H")
  
  winType <- unname(ifelse(which.max(c(currResult[c(2, 4)])) == 1, "Away", "Home"))
  
  if(winType == "Home"){
    winners <- hosts
    losers <- visitors
  } else {
    winners <- visitors
    losers <- hosts
  }
  
  if(currResult$Prediction != "NA"){
    predictionRecord <- rbind(predictionRecord, data.frame(Week = currResult$Week, Correct_prediction = winners == currResult$Prediction, Game_ID = currResult$Game_ID))
  }
  
  NFLtable[NFLtable$Team == winners, ]$`Win history` <- paste0(NFLtable[NFLtable$Team == winners, ]$`Win history`, "W")
  NFLtable[NFLtable$Team == losers, ]$`Win history` <- paste0(NFLtable[NFLtable$Team == losers, ]$`Win history`, "L")
  NFLtable[NFLtable$Team == winners, ]$Win <- NFLtable[NFLtable$Team == winners, ]$Win + 1
  NFLtable[NFLtable$Team == losers, ]$Loss <- NFLtable[NFLtable$Team == losers, ]$Loss + 1
  
  NFLtable[NFLtable$Team == currResult$`Home team`, ]$`Points against` <- NFLtable[NFLtable$Team == currResult$`Home team`, ]$`Points against` + currResult$`Visitors score`
  NFLtable[NFLtable$Team == currResult$`Visitors team`, ]$`Points for` <- NFLtable[NFLtable$Team == currResult$`Visitors team`, ]$`Points for` + currResult$`Visitors score`
  NFLtable[NFLtable$Team == currResult$`Visitors team`, ]$`Points against` <- NFLtable[NFLtable$Team == currResult$`Visitors team`, ]$`Points against` + currResult$`Home score`
  NFLtable[NFLtable$Team == currResult$`Home team`, ]$`Points for` <- NFLtable[NFLtable$Team == currResult$`Home team`, ]$`Points for` + currResult$`Home score`
  
  if(winType == "Home"){
    NFLtable[NFLtable$Team == hosts, ]$`Home wins` <- NFLtable[NFLtable$Team == hosts, ]$`Home wins` + 1
    NFLtable[NFLtable$Team == visitors, ]$`Away losses` <- NFLtable[NFLtable$Team == visitors, ]$`Away losses` + 1
  } else {
    NFLtable[NFLtable$Team == hosts, ]$`Home losses` <- NFLtable[NFLtable$Team == hosts, ]$`Home losses` + 1
    NFLtable[NFLtable$Team == visitors, ]$`Away wins` <- NFLtable[NFLtable$Team == hosts, ]$`Away wins` + 1
  }
  
  ### this is not working!
  if(currResult$`Home score` > currResult$`Visitors score`){
    NFLtable[NFLtable$Team == hosts, ]$`Home win rate` <- NFLtable[NFLtable$Team == hosts, ]$Played + 1
  }
}

NFLtable$`Points difference` <- NFLtable$`Points for` - NFLtable$`Points against`
NFLtable$`Points difference prop` <- NFLtable$`Points for` / NFLtable$`Points against`
NFLtable$`Win rate` <- NFLtable$Win / NFLtable$Played
NFLtable$`Win rate verbose` <- paste0(NFLtable$Win, "-", NFLtable$Loss)

NFLtable %>% data.frame %>% dplyr::select(Team, Points.difference) %>% arrange(desc(`Points.difference`))

NFLtable %>% 
  dplyr::select(Team, `Win rate`, `Win rate verbose`, `Points for`, `Points against`, `Points difference prop`, Conference, Division) %>% 
  dplyr::arrange(Conference, Division, desc(`Win rate`)) %>%
  print(n = Inf)

divisionStats <- function(con, div){
  NFLtable %>%
    filter(Conference == con, Division == div) %>%
    dplyr::select(Team, `Win rate`, `Win rate verbose`, `Points difference`, `Points difference prop`) %>%
    dplyr::arrange(desc(`Win rate`), desc(`Points difference`)) %>%
    setNames(c("Team", "W%", "W-L", "PD", "PDP")) %>%
    mutate(`W%` = as.integer(`W%` * 100), PD = as.integer(PD))
}

#### need to plug in exhibition games here
winRecords <- lapply(NFLtable$`Win history`, function(x){str_split(x, "") %>% unlist %>% data.frame() %>% setNames("Win_Loss")})
locationRecords <- lapply(NFLtable$`Location history`, function(x){str_split(x, "") %>% unlist %>% data.frame() %>% setNames("Home_Away")})
fullWinRecords <- lapply(1:length(winRecords), function(x){bind_cols(winRecords[[x]], locationRecords[[x]])})
names(fullWinRecords) <- NFLtable$Team

homeAwayRates <- function(teamOfInterest){
  record <- fullWinRecords[[teamOfInterest]]
  homeWins <- record %>% filter(Home_Away == "H")
  homeRate <- mean(homeWins$Win_Loss == "W")
  awayWins <- record %>% filter(Home_Away == "A")
  awayRate <- mean(awayWins$Win_Loss == "W")
  c(homeRate, awayRate)
}

### get streak
NFLtable$Streak <- ""
for(i in 1:nrow(NFLtable)){
  currentRecord <- NFLtable$`Win history`[i]
  previousResult <- str_sub(currentRecord, -1)
  currentStreak <- 1
  streakMatch <- T
  str_sub(currentRecord, -1) <- ""
  while(streakMatch){
    if(str_sub(currentRecord, -1) == previousResult){
      str_sub(currentRecord, -1) <- ""
      currentStreak <- currentStreak + 1
    } else {
      currentStreak <- paste0(currentStreak, previousResult)
      streakMatch <- F
    }
  }
  NFLtable$Streak[i] <- currentStreak
}

getRank <- function(myStat, desc = T){
  rank <- NFLtable[, c("Team", myStat)]
  if(desc){
    rank <- rank %>% arrange(desc(!!sym(myStat)))
  } else {
    rank <- rank %>% arrange(!!sym(myStat))
  }
  rank <- rank %>% mutate(Rank = as.numeric(factor(!!sym(myStat), levels = unique(!!sym(myStat)))))
  rankCounts <- rank %>% count(Rank)
  equalRanks <- rankCounts$Rank[which(rankCounts$n > 1)]
  rank$shortRank <- ifelse(rank$Rank %in% equalRanks, paste0(rank$Rank, "="), as.character(rank$Rank))
  rank$longRank <- paste0(rank$shortRank, " (/", max(rank$Rank), ")")
  rank
}

getRank("Win rate", desc = T)
getRank("Points for", desc = T)
getRank("Points against", desc = F)
getRank("Points difference", desc = T)
getRank("Points difference prop", desc = T)

predictionRecord <- predictionRecord %>% left_join(select(scoreSheets, Game_ID, Gross_PD), by = "Game_ID")
predictionAccuracy <- predictionRecord %>% group_by(Week) %>% summarise(Accuracy = sum(Correct_prediction) / length(Correct_prediction))
predictionPD <- predictionRecord %>% group_by(Week, Correct_prediction) %>% summarise(Mean_PD = median(Gross_PD))
predictionPD$Correct_prediction <- ifelse(predictionPD$Correct_prediction, "Correct", "Incorrect")

NFLteams <- sort(NFLtable$Team)

NFLteamInfo <- NFLtable %>% select(City, Team, Division, Conference)
NFLteamInfo$Division <- factor(NFLteamInfo$Division, levels = c("North", "East", "South", "West"))
NFLteamInfo$Conference <- factor(NFLteamInfo$Conference, levels = c("AFC", "NFC"))
NFLteamInfo <- NFLteamInfo %>% arrange(Division, Conference)
NFLteamInfo$y <- rep(4:1, each = 8)
NFLteamInfo$x <- rep(c(1:4, 6:9), 4)

colour_palette <- list(
  primary = "#59c7f3",
  secondary = "#0490cb",
  background = "#023664",
  text = "#e6e6e6",
  accent = "#fad105"
)

ggNFLtheme <- theme(
  plot.background = element_rect(fill = colour_palette$background, color = NA),
  panel.background = element_rect(fill = colour_palette$background, color = NA),
  panel.grid.major = element_line(color = colour_palette$secondary),
  panel.grid.minor = element_line(color = colour_palette$secondary),
  axis.text = element_text(color = colour_palette$text),
  axis.title = element_text(color = colour_palette$text),
  plot.title = element_text(color = colour_palette$text),
  plot.caption = element_text(color = colour_palette$text)
)

teamOpponentScores <- function(teamOfInterest){
  teamStats <- scoreSheets %>% filter(`Visitors team` == teamOfInterest | `Home team` == teamOfInterest)
  teamStats$`At home` <- ifelse(teamStats$`Visitors team` == teamOfInterest, F, T)
  names(teamStats)[1:4] <- c("Team", "Team score", "Opponent", "Opponent score")
  for(i in 1:nrow(teamStats)){
    if(teamStats$`At home`[i]){
      tempRow <- teamStats[i, ]
      tempRow[1:2] <- teamStats[i, 3:4] 
      tempRow[3:4] <- teamStats[i, 1:2] 
      teamStats[i, ] <- tempRow
    }
  }
  teamStats
}

scorePlot <- function(teamOfInterest, ylimits = c(40, 40)){
  teamStats <- teamOpponentScores(teamOfInterest)
  ggplot(teamStats, aes(Week, `Team score` - `Opponent score`, fill = `At home`)) + 
    geom_col(alpha = 0.8, col = colour_palette$text) + 
    geom_hline(yintercept = 3, size = 1.25, linetype = 2, col = colour_palette$text, alpha = 0.8) +
    geom_hline(yintercept = 7, size = 1.25, col = colour_palette$text, alpha = 0.8) +
    geom_hline(yintercept = 0, size = 1.5, col = colour_palette$text, alpha = 0.8) +
    geom_hline(yintercept = -3, size = 1.25, linetype = 2, col = colour_palette$text, alpha = 0.8) +
    geom_hline(yintercept = -7, size = 1.25, col = colour_palette$text, alpha = 0.8) +
    scale_y_continuous(limits = ylimits) +
    #scale_x_continuous(limits = c(1, max(scoreSheets$Week))) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = 25),
          axis.text = element_text(size = 20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    scale_fill_manual(values = c(colour_palette$primary, colour_palette$accent)) +
    labs(y = "Points difference", x = "Week") +
    geom_label(aes(Week, `Team score` - `Opponent score`, label = Opponent), fill = "white", size = 5) +
    ggNFLtheme
}

pointsScaling <- function(team1, team2){
  team1_range <- teamOpponentScores(team1) %>% select(`Team score`, `Opponent score`) %>% range()
  team2_range <- teamOpponentScores(team2) %>% select(`Team score`, `Opponent score`) %>% range()
  range(team1_range, team2_range)
}

NFLteamRasterGROBs <- lapply(NFLteamInfo$Team, function(x){
  logo_dir <- "Logos/"
  team <- x
  logo_ID <- dir(logo_dir)[which(str_detect(dir(logo_dir), team))]
  image_path <- paste0(logo_dir, logo_ID)
  img <- readPNG(image_path)
  rasterGrob(img, interpolate = TRUE)
})

names(NFLteamRasterGROBs) <- NFLteamInfo$Team

forAgainstPlot <- function(teamOfInterest, ylimits = c(-40, 40)){
  teamStats <- teamOpponentScores(teamOfInterest) %>% select(Team, `Team score`, Opponent, `Opponent score`, `Week`)
  scores <- data.frame(Team = c(teamStats$Team, teamStats$Opponent),
                       Score = c(teamStats$`Team score`, teamStats$`Opponent score`)) %>%
    mutate(Source = ifelse(Team == teamOfInterest, "For", "Against"),
           Week = rep(teamStats$Week, 2))
  scores$Source <- factor(scores$Source, levels = c("For", "Against"))
  yDiff <- diff(ylimits)
  scalingFactor <- 25
  yScaling <- yDiff / scalingFactor
  xScaling <- yDiff / scalingFactor * 0.1
  
  grobs <- lapply(scores$Team, function(x) list(x = 0, y = 0, grob = NFLteamRasterGROBs[[x]]))
  grobs <- lapply(1:length(grobs), function(i) {grobs[[i]]$y <- scores$Score[i]; grobs[[i]]})
  jitterFor <- seq(0.75, 1.25, length.out = nrow(teamStats))
  jitterAgainst <- seq(1.75, 2.25, length.out = nrow(teamStats))
  jitterBox <- c(jitterFor, jitterAgainst)
  grobs[which(scores$Source == "For")] <- lapply(which(scores$Source == "For"), function(i) {grobs[[i]]$x <- jitterBox[i]; grobs[[i]]})
  grobs[which(scores$Source == "Against")] <- lapply(which(scores$Source == "Against"), function(i) {grobs[[i]]$x <- jitterBox[i]; grobs[[i]]})
  
  # fig <- plot_ly(
  #   data = scores, 
  #   x = ~Source,
  #   y = ~Score,
  #   type = "box",
  #   boxpoints = "all",
  #   jitter = 0.3,
  #   pointpos = -1.8
  # )
  
  p <- ggplot(scores, aes(Source, Score)) +
    geom_boxplot(outlier.shape = NA, fill = colour_palette$primary, alpha = 0.8, col = colour_palette$text) + 
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = 25),
          axis.text = element_text(size = 20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "", y = "Points scored") +
    scale_y_continuous(limits = ylimits) +
    ggNFLtheme
  
  for(i in 1:length(grobs)){
    currGrob <- grobs[[i]]
    p <- p +
      annotation_custom(
        currGrob$grob,
        xmin = currGrob$x - xScaling, xmax = currGrob$x + xScaling,
        ymin = currGrob$y - yScaling, ymax = currGrob$y + yScaling
      )
  }
  p
}

NFLteamInfo <- NFLtable %>% select(City, Team, Division, Conference)
NFLteamInfo$Division <- factor(NFLteamInfo$Division, levels = c("North", "East", "South", "West"))
NFLteamInfo$Conference <- factor(NFLteamInfo$Conference, levels = c("AFC", "NFC"))
NFLteamInfo <- NFLteamInfo %>% arrange(Division, Conference)
NFLteamInfo$y <- rep(4:1, each = 8)
NFLteamInfo$x <- rep(c(1:4, 6:9), 4)

NFLteamLogos <- lapply(NFLteamInfo$Team, function(x){
  logo_dir <- "Logos/"
  team <- x
  logo_ID <- dir(logo_dir)[which(str_detect(dir(logo_dir), team))]
  image_path <- paste0(logo_dir, logo_ID)
  base64enc::dataURI(file = image_path, mime = "image/png")
})

allLogos <- lapply(1:nrow(NFLteamInfo), function(x){
  list(
    source = NFLteamLogos[[x]],
    x = NFLteamInfo$x[x],  
    y = NFLteamInfo$y[x],
    sizex = 0.9,
    sizey = 0.9,
    xref = "x",
    yref = "y",
    xanchor = "center",
    yanchor = "middle"
  )
})

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center-text {
        text-align: center;
      }
    ")),
    tags$style(HTML(glue::glue(
      "
      body {{ background-color: {colour_palette$background}; color: {colour_palette$text}; }}
      .btn-primary {{ background-color: {colour_palette$primary}; color: white; }}
      .btn-secondary {{ background-color: {colour_palette$secondary}; color: white; }}
      "
    )))
  ),
  tabsetPanel(
    tabPanel(
      "Team performance",
      fluidRow(
        column(2,
               h3("NFL app v2")
        ),
        column(2,
               "Placeholder"
        ),
        column(2,
               h3(checkboxInput("show_afc", "Show AFC results", value = F))
        ),
        column(2,
               h3(checkboxInput("show_nfc", "Show NFC results", value = F))
        ),
        column(2,
               h3(checkboxInput("show_team_tables", "Show team tables", value = F))
        ),
        column(2,
               h3(checkboxInput("show_season_results", "Show season results", value = T))
        )
      ),
      fluidRow(
        column(2,
               plotOutput("nfl_pd", height = 1200)
        ),
        column(2,
               plotOutput("nfl_win_rate", height = 1200)
        ),
        column(8,
               fluidRow(
                 column(3,
                        tableOutput("AFCnorth")
                 ),
                 column(3,
                        tableOutput("AFCeast")
                 ),
                 column(3,
                        tableOutput("AFCsouth")
                 ),
                 column(3,
                        tableOutput("AFCwest")
                 )
               ),
               fluidRow(
                 column(3,
                        tableOutput("NFCnorth")
                 ),
                 column(3,
                        tableOutput("NFCeast")
                 ),
                 column(3,
                        tableOutput("NFCsouth")
                 ),
                 column(3,
                        tableOutput("NFCwest")
                 )
               ),
               fluidRow(
                 column(3,
                        div(
                          style = "height: 300px; display: flex; align-items: center; justify-content: center; border: 1px solid #ddd;",  # Center container
                          div(
                            style = "max-height: 100%; max-width: 100%; display: flex; align-items: center; justify-content: center;",
                            imageOutput("team1_logo", width = "auto", height = "auto")  # Set height to fill most of container, maintaining aspect ratio
                          )
                        ),
                        h2(textOutput("team1_banner"), class = "center-text"),
                        h2(textOutput("team1_affiliation"), class = "center-text"),
                        h2(textOutput("team1_record"), class = "center-text"),
                        h2(textOutput("team1_home"), class = "center-text"),
                        h2(textOutput("team1_away"), class = "center-text"),
                        h2(textOutput("team1_record_verbose"), class = "center-text"),
                        h2(textOutput("team1_streak"), class = "center-text")
                        #verbatimTextOutput("cum_clicks"),
                        #verbatimTextOutput("team1_selection")
                        
                 ),
                 column(6,
                        plotlyOutput("team_selector"),
                        fluidRow(
                          column(6,
                                 plotOutput("team1_forAgainstPlot")
                          ),
                          column(6,
                                 plotOutput("team2_forAgainstPlot")
                          )
                        )#,
                        #verbatimTextOutput("scaled_y_scores"),
                        #verbatimTextOutput("scaled_y_difference")
                 ),
                 column(3,
                        div(
                          style = "height: 300px; display: flex; align-items: center; justify-content: center; border: 1px solid #ddd;",  # Center container
                          div(
                            style = "max-height: 100%; max-width: 100%; display: flex; align-items: center; justify-content: center;",
                            imageOutput("team2_logo", width = "auto", height = "auto")  # Set height to fill most of container, maintaining aspect ratio
                          )
                        ),
                        h2(textOutput("team2_banner"), class = "center-text"),
                        h2(textOutput("team2_affiliation"), class = "center-text"),
                        h2(textOutput("team2_record"), class = "center-text"),
                        h2(textOutput("team2_home"), class = "center-text"),
                        h2(textOutput("team2_away"), class = "center-text"),
                        h2(textOutput("team2_record_verbose"), class = "center-text"),
                        h2(textOutput("team2_streak"), class = "center-text")
                        #verbatimTextOutput("team2_selection")
                 )
               ),
               fluidRow(
                 column(6,
                        #h2(selectInput("team1", "Team 1", choices = NFLteams, selected = NFLteams[1])),
                        dataTableOutput("team1_table"),
                        # fluidRow(
                        #   column(4, 
                        #          h2(textOutput("team1_record"))
                        #   ),
                        #   column(4, 
                        #          h2(textOutput("team1_home"))
                        #   ),
                        #   column(4, 
                        #          h2(textOutput("team1_away"))
                        #   )
                        # ),
                        # fluidRow(
                        #   column(4, 
                        #          h2(textOutput("team1_record_verbose"))
                        #   ),
                        #   column(4, 
                        #          h2(textOutput("team1_streak"))
                        #   ),
                        #   column(4,
                        #          ""
                        #   )
                        # ),
                        plotOutput("team1_scorePlot", height = 400)
                 ),
                 column(6,
                        #h2(selectInput("team2", "Team 2", choices = NFLteams, selected = NFLteams[2])),
                        dataTableOutput("team2_table"),
                        # fluidRow(
                        #   column(4, 
                        #          h2(textOutput("team2_record"))
                        #   ),
                        #   column(4, 
                        #          h2(textOutput("team2_home"))
                        #   ),
                        #   column(4, 
                        #          h2(textOutput("team2_away"))
                        #   )
                        # ),
                        # fluidRow(
                        #   column(4, 
                        #          h2(textOutput("team2_record_verbose"))
                        #   ),
                        #   column(4, 
                        #          h2(textOutput("team2_streak"))
                        #   ),
                        #   column(4,
                        #          ""
                        #   )
                        # ),
                        plotOutput("team2_scorePlot", height = 400)
                 )
               )
        )
      )
    ),
    tabPanel("My performance",
             fluidRow(
               plotOutput("predictionAccuracy", height = 500),
               plotOutput("predictionPD", height = 500)
             )
    )
  )
)

server <- function(input, output) {
  output$team1_logo <- renderImage({
    logo_dir <- "Logos/"
    team <- selected_team1()
    logo_ID <- dir(logo_dir)[which(str_detect(dir(logo_dir), team))]
    image_path <- paste0(logo_dir, logo_ID)
    list(
      src = image_path,
      contentType = "image/png",
      width = 200,
      #height = 300,
      alt = selected_team1()
    )
  }, deleteFile = FALSE)
  
  output$team2_logo <- renderImage({
    logo_dir <- "Logos/"
    team <- selected_team2()
    logo_ID <- dir(logo_dir)[which(str_detect(dir(logo_dir), team))]
    image_path <- paste0(logo_dir, logo_ID)
    list(
      src = image_path,
      contentType = "image/png",
      width = 200,
      #height = 300,
      alt = selected_team2()
    )
  }, deleteFile = FALSE)
  
  output$team1_affiliation <- renderText({
    paste(
      as.character(NFLteamInfo[NFLteamInfo$Team == selected_team1(), ]$Conference),
      as.character(NFLteamInfo[NFLteamInfo$Team == selected_team1(), ]$Division)
    )
  })
  
  output$team2_affiliation <- renderText({
    paste(
      as.character(NFLteamInfo[NFLteamInfo$Team == selected_team2(), ]$Conference),
      as.character(NFLteamInfo[NFLteamInfo$Team == selected_team2(), ]$Division)
    )
  })
  
  output$nfl_win_rate <- renderPlot({
    team1 <- selected_team1()
    team2 <- selected_team2()
    NFLtab <- NFLtable
    NFLtab %>% 
      dplyr::select(Team, `Win rate`, `Win rate verbose`, `Points for`, `Points against`, `Points difference prop`) %>% 
      dplyr::arrange(desc(`Win rate`), desc(`Points difference prop`)) %>%
      data.frame()
    NFLtab$plot_col <- "other"
    NFLtab$plot_col[NFLtab$Team == team1 | NFLtab$Team == team2] <- "interest"
    NFLtab %>%
      ggplot(aes(forcats::fct_reorder(Team, (`Points difference prop` - 1) * 100), (`Points difference prop` - 1) * 100)) + 
      geom_col(aes(fill = plot_col), alpha = 0.8, col = colour_palette$text) +
      scale_fill_manual(values = c(colour_palette$accent, colour_palette$primary)) +
      coord_flip() +
      geom_hline(yintercept = 0, col = colour_palette$text, size = 1.5) +
      theme_bw() +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            legend.position = "none") +
      labs(y = "PDP (%)", x = "") +
      ggNFLtheme
  })
  
  output$nfl_pd <- renderPlot({
    team1 <- selected_team1()
    team2 <- selected_team2()
    NFLtab <- NFLtable
    NFLtab$plot_col <- "other"
    NFLtab$plot_col[NFLtab$Team == team1 | NFLtab$Team == team2] <- "interest"
    NFLtab %>% 
      ggplot(aes(forcats::fct_reorder(Team, (`Win rate` - 0.5) * 200), (`Win rate` - 0.5) * 200)) + 
      geom_col(aes(fill = plot_col), alpha = 0.8, col = colour_palette$text) + 
      scale_fill_manual(values = c(colour_palette$accent, colour_palette$primary)) +
      coord_flip() +
      geom_hline(yintercept = 0, col = colour_palette$text, size = 1.5) + 
      theme_bw() +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            legend.position = "none") +
      labs(y = "Win rate (%)", x = "") +
      ggNFLtheme
  })
  
  output$AFCnorth <- renderTable({
    if(input$show_afc){
      divisionStats("AFC", "North")
    }
    
  })
  output$AFCeast <- renderTable({
    if(input$show_afc){
      divisionStats("AFC", "East")
    }
  })
  output$AFCsouth <- renderTable({
    if(input$show_afc){
      divisionStats("AFC", "South")
    }
  })
  output$AFCwest <- renderTable({
    if(input$show_afc){
      divisionStats("AFC", "West")
    }
  })
  output$NFCnorth <- renderTable({
    if(input$show_nfc){
      divisionStats("NFC", "North")
    }
  })
  output$NFCeast <- renderTable({
    if(input$show_nfc){
      divisionStats("NFC", "East")
    }
  })
  output$NFCsouth <- renderTable({
    if(input$show_nfc){
      divisionStats("NFC", "South")
    }
  })
  output$NFCwest <- renderTable({
    if(input$show_nfc){
      divisionStats("NFC", "West")
    }
  })
  
  output$team_selector <- renderPlotly({
    fig <- plot_ly(x = NFLteamInfo$x, y = NFLteamInfo$y, 
                   type = 'scatter', 
                   mode = 'markers', 
                   marker = list(color = colour_palette$background),
                   text = paste0(NFLteamInfo$City, "\n", NFLteamInfo$Team), 
                   hoverinfo = "text"
    )
    
    fig <- fig %>%
      layout(
        images = allLogos,
        xaxis = list(showgrid = F, showticklabels = F),
        yaxis = list(showgrid = F, showticklabels = F),
        plot_bgcolor = colour_palette$background,
        hoverlabel = list(font = list(size = 20, color = colour_palette$text)),
        margin = list(l = 1, r = 1, t = 1, b = 1),
        annotations = list(
          list(
            x = 5, y = 4,
            text = "N", 
            showarrow = F,
            font = list(size = 20, color = colour_palette$text)
          ),
          list(
            x = 5, y = 3,
            text = "E",
            showarrow = F,
            font = list(size = 20, color = colour_palette$text)
          ),
          list(
            x = 5, y = 2,
            text = "S",
            showarrow = F,
            font = list(size = 20, color = colour_palette$text)
          ),
          list(
            x = 5, y = 1,
            text = "W",
            showarrow = F,
            font = list(size = 20, color = colour_palette$text)
          ),
          list(
            x = 2.5, y = 4.75,
            text = "AFC",
            showarrow = F,
            font = list(size = 30, color = colour_palette$text)
          ),
          list(
            x = 7.5, y = 4.75,
            text = "NFC",
            showarrow = F,
            font = list(size = 30, color = colour_palette$text)
          ),
          list(
            x = 7.5, y = 0.75,
            text = ".",
            showarrow = F,
            font = list(size = 20, color = colour_palette$background)
          )
        )
      )
    
    fig <- fig %>% config(displayModeBar = F)
    
    fig
  })
  
  cumulative_clicks <- reactiveVal(0)
  selected_team1 <- reactiveVal("Ravens")
  selected_team2 <- reactiveVal("Broncos")
  
  output$cum_clicks <- renderPrint({
    cumulative_clicks()
  })
  
  output$team1_selection <- renderPrint({
    selected_team1()
  })
  
  output$team2_selection <- renderPrint({
    selected_team2()
  })
  
  output$team1_banner <- renderText({
    paste0(NFLteamInfo$City[NFLteamInfo$Team == selected_team1()], "\n", selected_team1())
  })
  
  output$team2_banner <- renderText({
    paste0(NFLteamInfo$City[NFLteamInfo$Team == selected_team2()], "\n", selected_team2())
  })
  
  observeEvent(event_data("plotly_click"), {
    new_click <- event_data("plotly_click")
    cum_clicks <- cumulative_clicks()
    upd_clicks <- cum_clicks + 1
    cumulative_clicks(upd_clicks)
    new_team <- NFLteamInfo %>% filter(x == new_click$x & y == new_click$y) %>% pull(Team)
    if(cumulative_clicks() %% 2 != 0){
      selected_team1(new_team)
    } else {
      selected_team2(new_team)
    }
  })
  
  output$team1_table <- DT::renderDT({
    if(input$show_team_tables){
      scoreSheets %>% filter(`Visitors team` == selected_team1() | `Home team` == selected_team1()) %>% dplyr::select(-Prediction)
    }
  })
  
  output$team2_table <- DT::renderDT({
    if(input$show_team_tables){
      scoreSheets %>% filter(`Visitors team` == selected_team2() | `Home team` == selected_team2()) %>% dplyr::select(-Prediction)
    }
  })
  
  output$team1_record <- renderText({
    paste0("Win %: ", round(NFLtable$`Win rate`[NFLtable$Team == selected_team1()] * 100, 1))
  })
  
  output$team2_record <- renderText({
    paste0("Win %: ", round(NFLtable$`Win rate`[NFLtable$Team == selected_team2()] * 100, 1))
  })
  
  output$team1_home <- renderText({
    paste0("Home %: ", round(homeAwayRates(selected_team1())[1] * 100, 1))
  })
  
  output$team2_home <- renderText({
    paste0("Home %: ", round(homeAwayRates(selected_team2())[1] * 100, 1))
  })
  
  output$team1_away <- renderText({
    paste0("Away %: ", round(homeAwayRates(selected_team1())[2] * 100, 1))
  })
  
  output$team2_away <- renderText({
    paste0("Away %: ", round(homeAwayRates(selected_team2())[2] * 100, 1))
  })
  
  output$team1_record_verbose <- renderText({
    paste0("Record: ", NFLtable$`Win rate verbose`[NFLtable$Team == selected_team1()])
  })
  
  output$team2_record_verbose <- renderText({
    paste0("Record: ", NFLtable$`Win rate verbose`[NFLtable$Team == selected_team2()])
  })
  
  output$team1_streak <- renderText({
    paste0("Streak: ", NFLtable$Streak[NFLtable$Team == selected_team1()])
  })
  
  output$team2_streak <- renderText({
    paste0("Streak: ", NFLtable$Streak[NFLtable$Team == selected_team2()])
  })
  
  scaled_y_axis_scores <- reactive({
    pointsScaling(selected_team1(), selected_team2())
  })
  
  output$scaled_y_scores <- renderPrint({
    scaled_y_axis_scores()
  })
  
  scaled_y_axis_difference <- reactive({
    range(teamOpponentScores(selected_team1()) %>% mutate(Difference = `Team score` - `Opponent score`) %>% pull(Difference),
          teamOpponentScores(selected_team2()) %>% mutate(Difference = `Team score` - `Opponent score`) %>% pull(Difference))  
  })
  
  output$scaled_y_difference <- renderPrint({
    scaled_y_axis_difference()
  })
  
  output$team1_scorePlot <- renderPlot({
    if(input$show_season_results){
      scorePlot(selected_team1(), ylimits = scaled_y_axis_difference())
    }
  })
  
  output$team2_scorePlot <- renderPlot({
    if(input$show_season_results){
      scorePlot(selected_team2(), ylimits = scaled_y_axis_difference())
    }
  })
  
  output$team1_forAgainstPlot <- renderPlot({
    if(input$show_season_results){
      forAgainstPlot(selected_team1(), ylimits = scaled_y_axis_scores())
    }
  })
  
  output$team2_forAgainstPlot <- renderPlot({
    if(input$show_season_results){
      forAgainstPlot(selected_team2(), ylimits = scaled_y_axis_scores())
    }
  })
  
  #####################################
  
  output$predictionAccuracy <- renderPlot({
    ggplot(predictionAccuracy, aes(Week, Accuracy * 100)) + 
      geom_hline(yintercept = 50, col = colour_palette$text) +
      geom_hline(yintercept = 70, col = colour_palette$text, linetype = 2) +
      geom_point(col = colour_palette$accent) + 
      geom_line(col = colour_palette$accent) + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
      scale_x_continuous(limits = c(1, 17), breaks = seq(1, 17, 1)) +
      labs(main = "My prediction performance",
           y = "Accuracy (%)") +
      theme(axis.title = element_text(size = 25),
            axis.text = element_text(size = 20),
            panel.grid.minor.x = element_blank()) +
      ggNFLtheme
  })
  
  output$predictionPD <- renderPlot({
    ggplot(predictionPD, aes(Week, Mean_PD, col = Correct_prediction)) +
      geom_hline(yintercept = 3, col = colour_palette$text, linetype = 2) +
      geom_hline(yintercept = 7, col = colour_palette$text, linetype = 2) +
      geom_point() + 
      geom_line() +
      scale_x_continuous(limits = c(1, 17), breaks = seq(1, 17, 1)) +
      labs(y = "Median points difference") +
      theme(axis.title = element_text(size = 25),
            axis.text = element_text(size = 20),
            legend.background = element_rect(fill = colour_palette$background, color = NA),
            legend.title = element_text(color = colour_palette$text, size = 20),
            legend.text = element_text(color = colour_palette$text, size = 15),
            legend.position = "top",
            panel.grid.minor.x = element_blank()) +
      scale_color_manual(values = rev(c(colour_palette$text, colour_palette$accent)), name = "Prediction") +
      ggNFLtheme
  })
}

shinyApp(ui = ui, server = server)




