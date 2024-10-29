library(grid)
library(png)
library(shiny)

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
                    h2(textOutput("team1_banner"), class = "center-text"),
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
                    h2(textOutput("team2_banner"), class = "center-text"),
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
                    plotOutput("team1_scorePlot", height = 425)
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
                    plotOutput("team2_scorePlot", height = 425)
             )
           )
    )
  )
)

server <- function(input, output) {
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
  selected_team2 <- reactiveVal("Steelers")
  
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
}

shinyApp(ui = ui, server = server)

   


