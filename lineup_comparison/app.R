library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(httr)
library(jsonlite)


## setwd('C:/Users/markg/onedrive/documents/nba/lineup_comparison_2')
seasons_list <- c('2017-18', '2016-17', '2015-16', '2014-15', '2013-14',
                  '2012-13', '2011-12', '2010-11', '2009-10', '2008-09',
                  '2007-08')

nba_logo_colors <- c('#0046AD', '#D6D6C9', '#D0103A')

shot_type <- c('All', "Three's", "Two's")

periods <- c('Game', '1st Half', '2nd Half', '1st Qtr', '2nd Qtr', '3rd Qtr',
             '4th Qtr', 'Overtime')

raw_nba_colors <- read.csv('www/nba_colors.csv', stringsAsFactors = FALSE)
raw_nba_colors <- raw_nba_colors[,1:ncol(raw_nba_colors) - 1]
default_teams <- raw_nba_colors[1:30, 1]

## Function to set the colors and team list for each season
set_colors_by_season <- function(season)({
    if (season %in% c('2017-18', '2016-17', '2015-16', '2014-15')) {
        nba_colors <- raw_nba_colors[1:30,]
    }
    if (season == '2013-14') {
        nba_colors <- rbind(raw_nba_colors[1:3,],
                            raw_nba_colors[31,],
                            raw_nba_colors[5:30,])
    }
    if (season == '2012-13') {
        nba_colors <- rbind(raw_nba_colors[1:3,],
                            raw_nba_colors[31,],
                            raw_nba_colors[5:18,],
                            raw_nba_colors[32,],
                            raw_nba_colors[20:30,])
    }
    if (season %in% c('2011-12', '2010-11', '2009-10', '2008-09')) {
        nba_colors <- rbind(raw_nba_colors[1,],
                            raw_nba_colors[3,],
                            raw_nba_colors[31,],
                            raw_nba_colors[5:18,],
                            raw_nba_colors[33,],
                            raw_nba_colors[32,],
                            raw_nba_colors[20:30,])
    }
    if (season == '2007-08') {
        nba_colors <- rbind(raw_nba_colors[1,],
                            raw_nba_colors[3,],
                            raw_nba_colors[31,],
                            raw_nba_colors[5:18,],
                            raw_nba_colors[33,],
                            raw_nba_colors[32,],
                            raw_nba_colors[20,],
                            raw_nba_colors[22:27,],
                            raw_nba_colors[34,],
                            raw_nba_colors[28:30,])
    }
    return(nba_colors)
})


nba_api_request <- function(api_url, parameters) {
  headers <- c('Accept-Language' = 'en-US,en;q=0.5',
                'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0',
                Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
                Referer = 'http://markgroner.com',
                Connection = 'keep-alive')

  request <- GET(
    api_url,
    query = parameters,
    add_headers(headers)
    )
  request_status <- stop_for_status(request)
  data <- content(request)
  main_result_json <- data$resultSets[[1]]
  rowset_data <- main_result_json$rowSet
  column_headers <- as.character(main_result_json$headers)
  response_df = data.frame(
      matrix(
          unlist(rowset_data),
          ncol = length(column_headers),
          byrow = TRUE
      ),
      stringsAsFactors = FALSE)
  names(response_df) <- column_headers
  response_df <- set_numeric_column_type(response_df, .9)
  return(response_df)
}


## numeric_threshold if this percentage of values in a column can be coerced to numeric, switch column type
set_numeric_column_type <- function(df, numeric_threshold) {
  for (column_name in colnames(df)) {
    name_length <- nchar(column_name)
    if (substr(column_name, name_length - 2, name_length) == '_ID') {
      ## pass
    } else {
        numeric_test_column <- as.numeric(df[[column_name]])
        total_values <- NROW(numeric_test_column)
        non_na_values <- NROW(na.omit(numeric_test_column))
        perc_non_na <- non_na_values / total_values
        if (perc_non_na >= numeric_threshold) {
          df[[column_name]] <- numeric_test_column
        }
    }
  }
  return(df)
}


## Function import and clean lineup data from NBA.com
get_lineup_stats <- function(measure_type, season, home_away_flag, per_mode) {
  lineup_stats_url <- 'http://stats.nba.com/stats/leaguedashlineups'
  lineup_stats_parameters <- list(
    Conference = '',
    DateFrom = '',
    DateTo = '',
    Division = '',
    GameID = '',
    GameSegment = '',
    GroupQuantity = 5,
    LastNGames = 0,
    LeagueID = '00',
    Location = home_away_flag,
    MeasureType = measure_type,
    Month = 0,
    OpponentTeamID = 0,
    Outcome = '',
    PORound = 0,
    PaceAdjust = 'N',
    PerMode = per_mode,
    Period = 0,
    PlusMinus = 'N',
    Rank = 'N',
    Season = season,
    SeasonSegment = '',
    SeasonType = 'Regular Season',
    ShotClockRange = '',
    TeamID = 0,
    VsConference = '',
    VsDivision = '')
  lineup_stats_df <- nba_api_request(lineup_stats_url, lineup_stats_parameters)
  return(lineup_stats_df)
}


## Function to fetch, merge, and group all lineup shots
scale_factor <- 12
get_nba_shots <- function(player_id, season, location) {
  shot_chart_url <- 'http://stats.nba.com/stats/shotchartdetail'
  shot_parameters <- list(
    PlayerID = player_id,
    PlayerPosition = '',
    Season = season,
    ContextMeasure = 'FGA',
    DateFrom = '',
    DateTo = '',
    GameID = '',
    GameSegment = '',
    LastNGames = 0,
    LeagueID = '00',
    Location = location,
    Month = 0,
    OpponentTeamID = 0,
    Outcome = '',
    Period = 0,
    Position = '',
    RookieYear = '',
    SeasonSegment = '',
    SeasonType = 'Regular Season',
    TeamID = 0,
    VsConference = '',
    VsDivision = '')
  shot_df <- nba_api_request(shot_chart_url, shot_parameters)
  return(shot_df)
}

get_lineup_shots <- function(player_id_list, season, location) {
    team_lineup_shots <- data.frame()
    for (player_id in player_id_list) {
        player_lineup_shots <- get_nba_shots(player_id, season, location)
        team_lineup_shots <- rbind(team_lineup_shots, player_lineup_shots)
    }
    return(team_lineup_shots)
}


## Function to smooth shot chart data
smoothShotData <- function(shot_data) {
    shot_data$LOC_X <- round((shot_data$LOC_X/10*12)/scale_factor, 0)
    shot_data$LOC_Y <- round((shot_data$LOC_Y/10*12)/scale_factor, 0) + 15/scale_factor
    shot_data$SHOT_TYPE_NUMERIC <- ifelse(as.character(shot_data$SHOT_TYPE) ==
                                              '3PT Field Goal',
                                          3, 2)
    shot_data$SHOT_VALUE <- (shot_data$SHOT_MADE_FLAG * shot_data$SHOT_TYPE_NUMERIC)
    shot_data$eFG <- shot_data$SHOT_VALUE/2
    shot_data_grouped <- shot_data %>%
        group_by(LOC_X, LOC_Y, SHOT_TYPE_NUMERIC) %>%
        summarise(eFG = mean(eFG),
                  FREQ = length(SHOT_TYPE_NUMERIC))
    shot_data_grouped$DISTANCE <- sqrt(shot_data_grouped$LOC_X ^ 2 +
                                           shot_data_grouped$LOC_Y ^ 2)

    shot_data_grouped$SMOOTH_eFG <- NA

    for (loc in 1:length(shot_data_grouped$LOC_X)) {
        temp_x <- shot_data_grouped$LOC_X[loc]
        temp_y <- shot_data_grouped$LOC_Y[loc]
        temp_type <- shot_data_grouped$SHOT_TYPE_NUMERIC[loc]
        temp_distance <- shot_data_grouped$DISTANCE[loc]
        temp_filtered <- filter(shot_data_grouped,
                                sqrt((LOC_X - temp_x) ^ 2 +
                                         (LOC_Y - temp_y) ^ 2) <= 5 &
                                    abs(DISTANCE - temp_distance) <= 2,
                                SHOT_TYPE_NUMERIC == temp_type)
        shot_data_grouped$SMOOTH_eFG[loc] <- sum((temp_filtered$FREQ /
                                                      sum(temp_filtered$FREQ)) *
                                                     temp_filtered$eFG)
    }
    shot_data_grouped$CAP_SMOOTH_eFG <- ifelse(shot_data_grouped$SMOOTH_eFG > 1, 1, ifelse(shot_data_grouped$SMOOTH_eFG < 0, 0,
        shot_data_grouped$SMOOTH_eFG))

    freq_cap <- length(unique(shot_data$PLAYER_ID))
    shot_data_grouped$CAP_FREQ <- ifelse(shot_data_grouped$FREQ > sort(shot_data_grouped$FREQ,
                                                                          decreasing = TRUE)[freq_cap],
                                            sort(shot_data_grouped$FREQ, decreasing = TRUE)[freq_cap],
                                            shot_data_grouped$FREQ)
    return(shot_data_grouped)
}


## Shot chart color gradients
shots_nba_colors <- c('#22316C', '#0046AD', "#D6D6C9", '#D0103A', '#8B0000')


## Function to create shot chart
shotChartScatter <- function(shot_data) {
    color_gradient <- c('#22316C', '#0046AD', "#D6D6C9", '#D0103A', '#8B0000')
    shot_chart <- ## try(
        ggplot(shot_data, aes(x = LOC_X, y = LOC_Y)) +
        geom_point(shape = 15, aes(size = CAP_FREQ, color = CAP_SMOOTH_eFG)) +
        scale_size(name = 'Shot Freq.', labels = NULL) +
        scale_color_gradientn(name = 'eFG%',
                              colors = color_gradient,
                              limits = c(0, 1),
                              breaks = c(.25 , .5, .75),
                              labels = c('25%', '50%', '75%')) +
        xlim(300/scale_factor, -300/scale_factor) +
        ylim(-48/scale_factor, 372/scale_factor) +
        coord_fixed() +
        geom_segment(aes(x = -264/scale_factor, y = -48/scale_factor,
                         xend = -264/scale_factor, yend = 120/scale_factor),
                     size = 1) +
        geom_segment(aes(x = 264/scale_factor, y = -48/scale_factor,
                         xend = 264/scale_factor, yend = 120/scale_factor),
                     size = 1) +
        geom_curve(aes(x = -264/scale_factor, y = 120/scale_factor,
                       xend = 264/scale_factor, yend = 120/scale_factor),
                   curvature = .68, ncp = 50, size = 1) +
        geom_segment(aes(x = -300/scale_factor, y = -48/scale_factor,
                         xend = 300/scale_factor, yend = -48/scale_factor),
                     size = 1) +
        geom_segment(aes(x = -96/scale_factor, y = 195/scale_factor,
                         xend = 96/scale_factor, yend = 195/scale_factor),
                     size = 1) +
        geom_segment(aes(x = -96/scale_factor, y = -47.4/scale_factor,
                         xend = -96/scale_factor, yend = 195/scale_factor),
                     size = 1) +
        geom_segment(aes(x = 96/scale_factor, y = -47.4/scale_factor,
                         xend = 96/scale_factor, yend = 195/scale_factor),
                     size = 1) +
        geom_segment(aes(x = -72/scale_factor, y = -47.4/scale_factor,
                         xend = -72/scale_factor, yend = 195/scale_factor),
                     size = 1) +
        geom_segment(aes(x = 72/scale_factor, y = -47.4/scale_factor,
                         xend = 72/scale_factor, yend = 195/scale_factor),
                     size = 1) +
        geom_segment(aes(x = -36/scale_factor, y = 0,
                         xend = 36/scale_factor, yend = 0),
                     size = 1) +
        geom_segment(aes(x = -3/scale_factor, y = 0,
                         xend = -3/scale_factor, yend = 6/scale_factor),
                     size = 1) +
        geom_segment(aes(x = 3/scale_factor, y = 0,
                         xend = 3/scale_factor, yend = 6/scale_factor),
                     size = 1) +
        geom_curve(aes(x = 0, y = 6/scale_factor,
                       xend = 0, yend = 24/scale_factor),
                   curvature = 1, ncp = 50, size = 1) +
        geom_curve(aes(x = 0, y = 6/scale_factor,
                       xend = 0, yend = 24/scale_factor),
                   curvature = -1, ncp = 50, size = 1) +
        geom_curve(aes(x = -72/scale_factor, y = 195/scale_factor,
                       xend = 72/scale_factor, yend = 195/scale_factor),
                   curvature = 1, ncp = 50, size = 1) +
        geom_curve(aes(x = -72/scale_factor, y = 195/scale_factor,
                       xend = 72/scale_factor, yend = 195/scale_factor),
                   linetype = 5, curvature = -1, ncp = 50, size = 1) +
        theme(line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = 'bottom',
              legend.box = 'horizontal',
              legend.key = element_blank(),
              panel.background = element_blank())
    ##,
    ##                silent = TRUE)
    return(shot_chart)
}


server <- function(input, output, session) {
    ## Set team colors and team team list by season
    team_nba_colors <- reactive({
        set_colors_by_season(input$team_season)
    })
    ## Set team list
    observe({
        updateSelectInput(session,
                          "team",
                          "Team:",
                          choices = team_nba_colors()$name,
                          selected = 'Cleveland Cavaliers')
    })
    ## Set team abbreviation
    team_abbreviation <- reactive({
        filter(team_nba_colors(), name == input$team)$name_abbr
    })
    ## Set team lineup advanced stats based on home away flag
    team_lineup_advanced_master <- reactive({
        if (input$home_road_flag == "Neutral Site") {
            team_lineup_advanced_master <- get_lineup_stats('Advanced',
                                                            input$team_season,
                                                            '',
                                                            'Totals')
        } else {
            team_lineup_advanced_master <- get_lineup_stats('Advanced',
                                                            input$team_season,
                                                            input$home_road_flag,
                                                            'Totals')
        }
        team_lineup_advanced_master
    })
    ## Set team lineup list
    observe({
        team_lineups <- filter(team_lineup_advanced_master(),
                               TEAM_ABBREVIATION == team_abbreviation())
        team_lineup_list <- as.vector(team_lineups$GROUP_NAME)
        updateSelectInput(session, "team_lineup",
                          choices = team_lineup_list)
    })
    ## Set team lineup advanced stats based on home away flag
    team_lineup_advanced_stats <- reactive({
        filter(team_lineup_advanced_master(),
               GROUP_NAME == input$team_lineup)
    })
    ## Set team lineup per 100 stats based on home away flag
    team_lineup_per_100_stats <- reactive({
        if (input$home_road_flag == "Neutral Site") {
            team_lineup_per_100_stats <- subset(get_lineup_stats('Base',
                                                                 input$team_season,
                                                                 '',
                                                                 'Per100Possessions'),
                                                GROUP_NAME == input$team_lineup)
        } else {
          team_lineup_per_100_stats <- subset(get_lineup_stats('Base',
                                                               input$team_season,
                                                               input$home_road_flag,
                                                               'Per100Possessions'),
                                              GROUP_NAME == input$team_lineup)
        }
        team_lineup_per_100_stats
    })
    ## Set team lineup scoring stats based on home away flag
    team_lineup_scoring_stats <- reactive({
        if (input$home_road_flag == "Neutral Site") {
            team_lineup_scoring_stats <- subset(get_lineup_stats('Scoring',
                                                                 input$team_season,
                                                                 '',
                                                                 'Totals'),
                                                GROUP_NAME == input$team_lineup)
        } else {
          team_lineup_scoring_stats <- subset(get_lineup_stats('Scoring',
                                                               input$team_season,
                                                               input$home_road_flag,
                                                               'Totals'),
                                              GROUP_NAME == input$team_lineup)
        }
        team_lineup_scoring_stats
    })
    ## Set opponent colors and opponent team list by season
    opponent_nba_colors <- reactive({
        set_colors_by_season(input$opponent_season)
    })
    ## Set opponent list
    observe({
        updateSelectInput(session,
                          "opponent",
                          "Opponent:",
                          choices = opponent_nba_colors()$name,
                          selected = 'Golden State Warriors')
    })
    ## Set opponent abbreviation
    opponent_abbreviation <- reactive({
        filter(opponent_nba_colors(), name == input$opponent)$name_abbr
    })
    ## Set opponent lineup advanced stats based on home away flag
    opponent_lineup_advanced_master <- reactive({
        if (input$home_road_flag == "Neutral Site") {
            opponent_lineup_advanced_master <- get_lineup_stats('Advanced',
                                                                input$opponent_season,
                                                                '',
                                                                'Totals')
        } else {
          opponent_lineup_advanced_master <- get_lineup_stats('Advanced',
                                                              input$opponent_season,
                                                              input$home_road_flag,
                                                              'Totals')
        }
        opponent_lineup_advanced_master
    })
    ## Set opponent lineup list
    observe({
        opponent_lineups <- filter(opponent_lineup_advanced_master(),
                                   TEAM_ABBREVIATION == opponent_abbreviation())
        opponent_lineup_list <- as.vector(opponent_lineups$GROUP_NAME)
        updateSelectInput(session, "opponent_lineup",
                          choices = opponent_lineup_list)
    })
    ## Set opponent lineup advanced stats based on home away flag
    opponent_lineup_advanced_stats <- reactive({
        filter(opponent_lineup_advanced_master(),
               GROUP_NAME == input$opponent_lineup)
    })
    ## Set opponent lineup per 100 stats based on home away flag
    opponent_lineup_per_100_stats <- reactive({
        if (input$home_road_flag == "Neutral Site") {
            opponent_lineup_per_100_stats <- subset(get_lineup_stats('Base',
                                                                 input$opponent_season,
                                                                 '',
                                                                 'Per100Possessions'),
                                                GROUP_NAME == input$opponent_lineup)
        } else {
          opponent_lineup_per_100_stats <- subset(get_lineup_stats('Base',
                                                               input$opponent_season,
                                                               input$home_road_flag,
                                                               'Per100Possessions'),
                                              GROUP_NAME == input$opponent_lineup)
        }
        opponent_lineup_per_100_stats
    })
    ## Set opponent lineup scoring stats based on home away flag
    opponent_lineup_scoring_stats <- reactive({
        if (input$home_road_flag == "Neutral Site") {
            opponent_lineup_scoring_stats <- subset(get_lineup_stats('Scoring',
                                                                 input$opponent_season,
                                                                 '',
                                                                 'Totals'),
                                                GROUP_NAME == input$opponent_lineup)
        } else {
          opponent_lineup_scoring_stats <- subset(get_lineup_stats('Scoring',
                                                               input$opponent_season,
                                                               input$home_road_flag,
                                                               'Totals'),
                                              GROUP_NAME == input$opponent_lineup)
        }
        opponent_lineup_scoring_stats
    })
    ## Set possession ratio based on user input slider
    possession_ratio <- reactive({
        ((((team_lineup_advanced_stats()$PACE +
                opponent_lineup_advanced_stats()$PACE)/2)
          /48 * input$minutes_slider)/100)
    })
    ## Set the point differential based on plus-minus of each team
    output$point_diff <- renderUI({
        HTML(paste("<center>",
                   '<font size = "5">',
                   'Projected point differential:',
                   "<b>",
                   toString(round((team_lineup_per_100_stats()$PLUS_MINUS
                                   - opponent_lineup_per_100_stats()$PLUS_MINUS)
                                  * possession_ratio(), 2)),
                   "points",
                   '</font>',
                   "</b>",
                   "</center>"))
    })
    ## Set team list of players in lineup selected by the user
    team_names <- reactive({
        as.vector(strsplit(input$team_lineup, ' - '))[[1]]
    })
    ## Create off, def, net rtg grouped bar chart
    output$ratings_graph <- renderPlot({
        team <- team_abbreviation()
        if (opponent_abbreviation() == team) {
            opponent <- paste0(team, '2')
        }
        if (opponent_abbreviation() != team) {
            opponent <- opponent_abbreviation()
        }
        ratings_data <- try(data.frame(rating_cats <- factor(c("Off Rtg", "Def Rtg", "Net Rtg",
                                                               "Off Rtg", "Def Rtg", "Net Rtg"),
                                                             levels = c("Off Rtg", "Def Rtg", "Net Rtg")),
                                       ratings <- c(team_lineup_advanced_stats()$OFF_RATING,
                                                    team_lineup_advanced_stats()$DEF_RATING,
                                                    team_lineup_advanced_stats()$NET_RATING,
                                                    opponent_lineup_advanced_stats()$OFF_RATING,
                                                    opponent_lineup_advanced_stats()$DEF_RATING,
                                                    opponent_lineup_advanced_stats()$NET_RATING),
                                       team_opponent <- factor(c(team, team, team,
                                                                 opponent, opponent,
                                                                 opponent),
                                                               levels = c(team,
                                                                          opponent))),
                            silent = TRUE)
        try(ggplot(ratings_data,
                   aes(x = team_opponent,
                       y = ratings,
                       fill = rating_cats)) +
                ggtitle('Ratings') +
                geom_bar(stat = "identity",
                         position = position_dodge()) +
                scale_fill_manual(values = nba_logo_colors) +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks = element_blank(),
                      legend.position = "bottom",
                      legend.title = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_line(color = '#DCDCDC'),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.background = element_blank(),
                      axis.line.y = element_line(color = "black",
                                                 size = .5)) +
                geom_hline(yintercept = 0),
            silent = TRUE)
    })
    ## Create shooting grouped bar chart
    output$shooting_graph <- renderPlot({
        team <- team_abbreviation()
        if (opponent_abbreviation() == team) {
            opponent <- paste0(team, '2')
        }
        if (opponent_abbreviation() != team) {
            opponent <- opponent_abbreviation()
        }
        shooting_data <- try(data.frame(shooting_cats <- factor(c("eFG%", "TS%", "3P%",
                                                                  "eFG%", "TS%", "3P%"),
                                                                levels = c("eFG%", "TS%", "3P%")),
                                        shooting_p <- c(team_lineup_advanced_stats()$EFG_PCT,
                                                        team_lineup_advanced_stats()$TS_PCT,
                                                        team_lineup_per_100_stats()$FG3_PCT,
                                                        opponent_lineup_advanced_stats()$EFG_PCT,
                                                        opponent_lineup_advanced_stats()$TS_PCT,
                                                        opponent_lineup_per_100_stats()$FG3_PCT),
                                        team_opponent <- factor(c(team, team, team,
                                                                  opponent, opponent,
                                                                  opponent),
                                                                levels = c(team,
                                                                           opponent))),
                             silent = TRUE)
        try(ggplot(shooting_data,
                   aes(x = team_opponent,
                       y = shooting_p,
                       fill = shooting_cats)) +
                coord_cartesian(ylim = c(0, 1)) +
                ggtitle('Shooting') +
                geom_bar(stat = "identity",
                         position = position_dodge()) +
                scale_fill_manual(values = nba_logo_colors) +
                scale_y_continuous(labels = percent) +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks = element_blank(),
                      legend.position = "bottom",
                      legend.title = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_line(color = '#DCDCDC'),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.background = element_blank(),
                      axis.line.y = element_line(color = "black",
                                                 size = .5)) +
                geom_hline(yintercept = 0),
            silent = TRUE)
    })
    ## Create scoring grouped bar chart
    output$scoring_graph <- renderPlot({
        team <- team_abbreviation()
        if (opponent_abbreviation() == team) {
            opponent <- paste0(team, '2')
        }
        if (opponent_abbreviation() != team) {
            opponent <- opponent_abbreviation()
        }
        scoring_data <- try(data.frame(scoring_cats <- factor(c("% PTs FTs",
                                                                "% PTs Off TO",
                                                                "% FGM UnAST",
                                                                "% PTs FTs",
                                                                "% PTs Off TO",
                                                                "% FGM UnAST"),
                                                              levels = c("% PTs FTs",
                                                                         "% PTs Off TO",
                                                                         "% FGM UnAST")),
                                       scoring_p <- c(team_lineup_scoring_stats()$PCT_PTS_FT,
                                                      team_lineup_scoring_stats()$PCT_PTS_OFF_TOV,
                                                      team_lineup_scoring_stats()$PCT_UAST_FGM,
                                                      opponent_lineup_scoring_stats()$PCT_PTS_FT,
                                                      opponent_lineup_scoring_stats()$PCT_PTS_OFF_TOV,
                                                      opponent_lineup_scoring_stats()$PCT_UAST_FGM),
                                       team_opponent <- factor(c(team, team, team,
                                                                 opponent, opponent,
                                                                 opponent),
                                                               levels = c(team,
                                                                          opponent))),
                            silent = TRUE)
        try(ggplot(scoring_data,
                   aes(x = team_opponent,
                       y = scoring_p,
                       fill = scoring_cats)) +
                coord_cartesian(ylim = c(0, 1)) +
                ggtitle('Scoring') +
                geom_bar(stat = "identity",
                         position = position_dodge()) +
                scale_fill_manual(values = nba_logo_colors) +
                scale_y_continuous(labels = percent) +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks = element_blank(),
                      legend.position = "bottom",
                      legend.title = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_line(color = '#DCDCDC'),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.background = element_blank(),
                      axis.line.y = element_line(color = "black",
                                                 size = .5)) +
                geom_hline(yintercept = 0),
            silent = TRUE)
    })
    ## Create rebounding grouped bar chart
    output$rebs_graph <- renderPlot({
        team <- team_abbreviation()
        if (opponent_abbreviation() == team) {
            opponent <- paste0(team, '2')
        }
        if (opponent_abbreviation() != team) {
            opponent <- opponent_abbreviation()
        }
        rebs_data <- try(data.frame(reb_cats <- factor(c("OREB%", "DREB%", "REB%",
                                                         "OREB%", "DREB%", "REB%"),
                                                       levels = c("OREB%", "DREB%", "REB%")),
                                    reb_p <- c(team_lineup_advanced_stats()$OREB_PCT,
                                               team_lineup_advanced_stats()$DREB_PCT,
                                               team_lineup_advanced_stats()$REB_PCT,
                                               opponent_lineup_advanced_stats()$OREB_PCT,
                                               opponent_lineup_advanced_stats()$DREB_PCT,
                                               opponent_lineup_advanced_stats()$REB_PCT),
                                    team_opponent <- factor(c(team, team, team,
                                                              opponent, opponent,
                                                              opponent),
                                                            levels = c(team,
                                                                       opponent))),
                         silent = TRUE)
        try(ggplot(rebs_data,
                   aes(x = team_opponent,
                       y = reb_p,
                       fill = reb_cats)) +
                coord_cartesian(ylim = c(0, 1)) +
                ggtitle('Rebounding') +
                geom_bar(stat = "identity",
                         position = position_dodge()) +
                scale_fill_manual(values = nba_logo_colors) +
                scale_y_continuous(labels = percent) +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks = element_blank(),
                      legend.position = "bottom",
                      legend.title = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_line(color = '#DCDCDC'),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.background = element_blank(),
                      axis.line.y = element_line(color = "black",
                                                 size = .5)) +
                geom_hline(yintercept = 0),
            silent = TRUE)
    })
    ## Set team shot chart player list
    observe({
        updateSelectInput(session,
                          'team_shot_chart_names',
                          'Players:',
                          team_names(),
                          team_names())
    })
    ## Fetch all team lineup shots
    team_lineup_shots <- reactive({
      team_lineup_ids <- strsplit(team_lineup_advanced_stats()$GROUP_ID, ' - ')[[1]]
        if (input$home_road_flag == "Neutral Site") {
            location <- ""
        }
        if (input$home_road_flag == "Home") {
            location <- "Home"
        }
        if (input$home_road_flag == "Road") {
            location <- "Road"
        }
        get_lineup_shots(team_lineup_ids, input$team_season, location)
    })
    ## Smooth home filtered data
    home_smoothed_shots <- reactive({
        smoothShotData(team_lineup_shots())
    })
    ## Create team shot chart scatter plot
    output$team_shot_chart <- renderPlot({
        shotChartScatter(home_smoothed_shots())
    })
    ## Set opponent list of players in lineup selected by the user
    opponent_names <- reactive({
        as.vector(strsplit(input$opponent_lineup, ' - '))[[1]]
    })
    ## Set shot chart player list
    observe({
        updateSelectInput(session,
                          'opponent_shot_chart_names',
                          'Players:',
                          opponent_names(),
                          opponent_names())
    })
    ## Fetch all opponent lineup shots
    opponent_lineup_shots <- reactive({
        opponent_lineup_ids <- strsplit(team_lineup_advanced_stats()$GROUP_ID, ' - ')[[1]]
        if (input$home_road_flag == "Neutral Site") {
            location <- ""
        }
        if (input$home_road_flag == "Home") {
            location <- "Road"
        }
        if (input$home_road_flag == "Road") {
            location <- "Home"
        }
        get_lineup_shots(opponent_lineup_ids, input$opponent_season, location)
    })
    ## Smooth opponent shot data
    opponent_smoothed_shots <- reactive({
        smoothShotData(opponent_lineup_shots())
    })
    ## Create opponent shot chart scatter plot
    output$opponent_shot_chart <- renderPlot({
        shotChartScatter(opponent_smoothed_shots())
    })
}




ui <- fluidPage(theme = "legronjames.css",
                ## Row one, year, team, and lineup selection
                fluidRow(
                    style = 'padding-left:12px;',
                    style = 'padding-right:12px;',
                    ## First Column
                    ## Team, lineup selection, pie charts, and shot chart
                    column(width = 5,
                           style = 'padding:0px;',
                           fluidRow(
                               ## Team season selection
                               column(width = 3,
                                      style = 'padding-left:12px;',
                                      style = 'padding-right:0px;',
                                      selectInput("team_season",
                                                  "Team Season:",
                                                  seasons_list,
                                                  '2017-18')

                               ),
                               ## Team selection
                               column(width = 9,
                                      style = 'padding-left:12px;',
                                      style = 'padding-right:12px;',
                                      selectInput('team',
                                                  'Team:',
                                                  default_teams,
                                                  'Cleveland Cavaliers',
                                                  width = '100%'))
                           ),
                           ## Team lineup selection
                           fluidRow(
                               style = 'padding-left:12px;',
                               style = 'padding-right:12px;',
                               selectInput("team_lineup",
                                           width = '100%',
                                           "Lineup:",
                                           '')
                           )
                    ),
                    ## Second column
                    ## Probability of outscoring opp, projected point diff, and
                    ## comparative bar charts
                    column(width = 2,
                           ## Select whether team is home or away
                           selectInput("home_road_flag",
                                       label = "Location:",
                                       choices = c("Neutral Site", "Home", "Road")
                           ),
                           ## Select number of possessions for projection
                           sliderInput("minutes_slider",
                                       label = 'Minutes:',
                                       min = 0,
                                       max = 12,
                                       step = .5,
                                       ticks = FALSE,
                                       value = 6)
                    ),
                    ## Third Column
                    ## Opponent, lineup selection, pie charts, and shot chart
                    column(width = 5,
                           style = 'padding:0px;',
                           fluidRow(
                               ## Opponent season selection
                               column(width = 3,
                                      style = 'padding-left:12px;',
                                      style = 'padding-right:0px;',
                                      selectInput("opponent_season",
                                                  "Opponent Season:",
                                                  seasons_list,
                                                  '2017-18')

                               ),
                               ## Opponent selection
                               column(width = 9,
                                      style = 'padding-left:12px;',
                                      style = 'padding-right:12px;',
                                      selectInput('opponent',
                                                  'Opponent:',
                                                  default_teams,
                                                  'Golden State Warriors',
                                                  width = '100%'))
                           ),
                           ## Opponent lineup selection
                           fluidRow(
                               style = 'padding-left:12px;',
                               style = 'padding-right:12px;',
                               selectInput("opponent_lineup",
                                           width = '100%',
                                           "Lineup:",
                                           '')
                           )
                    )
                ),
                ## Row two, point diff
                htmlOutput('point_diff'),
                ## Row three, comparative lineup charts
                fluidRow(
                    column(width = 3,
                           plotOutput("ratings_graph",
                                      height = "200px")),
                    column(width = 3,
                           plotOutput("shooting_graph",
                                      height = "200px")),
                    column(width = 3,
                           plotOutput("scoring_graph",
                                      height = "200px")),
                    column(width = 3,
                           plotOutput("rebs_graph",
                                      height = "200px"))
                ),
                ## Row four, shot charts
                fluidRow(
                  style = 'padding-right:12px;',
                    style = 'padding-left:12px;',
                    ## Team shot chart
                    column(width = 6,
                           style = 'padding:0px;',
                           plotOutput("team_shot_chart")),
                    ## Opponent shot chart
                    column(width = 6,
                           style = 'padding-left:0px;',
                           style = 'padding-right:12px;',
                           plotOutput('opponent_shot_chart'))
                ),
                titlePanel(title = NULL, windowTitle = 'NBA Lineup Comparison'),
                fluidRow(HTML(paste('<font size = "1">',
                                    'All data courtesy of stats.nba.com',
                                    '</font>'))),
                fluidRow(HTML(paste('<font size = "1">',
                                    'Shot chart data is at an individual level (not lineup level)',
                                    '</font>')))
)




shinyApp(ui, server)
