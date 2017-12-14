library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)
library(scales)

## setwd('C:/Users/markg/onedrive/documents/nba/shot_chart_2')
seasons_list <- c('2016-17', '2015-16', '2014-15', '2013-14', '2012-13',
                  '2011-12', '2010-11', '2009-10', '2008-09', '2007-08')

nba_logo_colors <- c('#0046AD', '#D6D6C9', '#D0103A')

shot_type <- c('All Shots', "Three's", "Two's")

periods <- c('Game', '1st Half', '2nd Half', '1st Qtr', '2nd Qtr', '3rd Qtr',
             '4th Qtr', 'Overtime')

clean_nba_colors <- read.csv('www/nba_colors.csv', stringsAsFactors = FALSE)[, 1:12]
default_teams <- read.csv('www/nba_colors.csv', stringsAsFactors = FALSE)[1:30, 1]

## Function to set the colors and team list for each season
set_colors_by_season <- function(season)({
    if (season %in% c('2016-17', '2015-16', '2014-15')) {
        nba_colors <- clean_nba_colors[1:30,]
    }
    if (season == '2013-14') {
        nba_colors <- rbind(clean_nba_colors[1:3,],
                            clean_nba_colors[31,],
                            clean_nba_colors[5:30,])
    }
    if (season == '2012-13') {
        nba_colors <- rbind(clean_nba_colors[1:3,],
                            clean_nba_colors[31,],
                            clean_nba_colors[5:18,],
                            clean_nba_colors[32,],
                            clean_nba_colors[20:30,])
    }
    if (season %in% c('2011-12', '2010-11', '2009-10', '2008-09')) {
        nba_colors <- rbind(clean_nba_colors[1,],
                            clean_nba_colors[3,],
                            clean_nba_colors[31,],
                            clean_nba_colors[5:18,],
                            clean_nba_colors[33,],
                            clean_nba_colors[32,],
                            clean_nba_colors[20:30,])
    }
    if (season == '2007-08') {
        nba_colors <- rbind(clean_nba_colors[1,],
                            clean_nba_colors[3,],
                            clean_nba_colors[31,],
                            clean_nba_colors[5:18,],
                            clean_nba_colors[33,],
                            clean_nba_colors[32,],
                            clean_nba_colors[20,],
                            clean_nba_colors[22:27,],
                            clean_nba_colors[34,],
                            clean_nba_colors[28:30,])
    }
    return(nba_colors)
})

team_roster_url <- 'http://stats.nba.com/stats/commonteamroster?LeagueID=00&Season='

readTeam_roster <- function(address, season, team_id){
    team_address <- paste0(address, season, '&TeamID=', team_id)
    web_data <- readLines(team_address)
    
    x1 <- gsub('[\\{\\}\\]]', '', web_data, perl = TRUE)
    x2 <- gsub('[\\[]', '\n', x1, perl = TRUE)
    x3 <- gsub('\'rowSet\':\n', '', x2, perl = TRUE)
    x4 <- gsub(';', ',',x3, perl = TRUE)
    
    player_data <- read.table(textConnection(x4), header = TRUE, sep = ',',
                              skip = 2, stringsAsFactors = FALSE, fill = TRUE)
    last_player_position <- which(player_data$TeamID == 'headers:')
    player_data <- player_data[1:(last_player_position - 1),
                               1:ncol(player_data) - 1]
    
    return(player_data)
}


## Function to fetch, merge, and group all lineup shots
scale_factor <- 12
fetch_shots <- function(player_id, season, location) {
    req(player_id)
    team_lineup_shots <- data.frame()
    request <- GET(
        'http://stats.nba.com/stats/shotchartdetail',
        query = list(
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
            VsDivision = ''
        )
    )
    
    stop_for_status(request)
    
    data <- content(request)
    
    raw_shots_data <- data$resultSets[[1]]$rowSet
    col_names <- as.character(data$resultSets[[1]]$headers)
    
    if (length(raw_shots_data) == 0) {
        shots <- data.frame(
            matrix(nrow = 0, ncol = length(col_names))
        )
    } else {
        shots = data.frame(
            matrix(
                unlist(raw_shots_data),
                ncol = length(col_names),
                byrow = TRUE
            )
        )
    }
    
    shots <- tbl_df(shots)
    names(shots) <- col_names
    shots <- mutate(shots,
                    PERIOD = as.numeric(as.character(PERIOD)),
                    SHOT_DISTANCE = as.numeric(as.character(SHOT_DISTANCE)),
                    SHOT_MADE_NUMERIC = as.numeric(as.character(SHOT_MADE_FLAG)),
                    SHOT_ATTEMPTED_FLAG = as.numeric(as.character(SHOT_ATTEMPTED_FLAG)))
    return(shots)
}



## Function to smooth shot chart data
smoothShotData <- function(shot_data) {
    shot_data$LOC_X <- round((as.numeric(as.character(shot_data$LOC_X))/10*12)
                             /scale_factor, 0)
    shot_data$LOC_Y <- round((as.numeric(as.character(shot_data$LOC_Y))/10*12)
                             /scale_factor, 0) + 15/scale_factor
    shot_data$SHOT_TYPE_NUMERIC <- ifelse(as.character(shot_data$SHOT_TYPE) ==
                                              '3PT Field Goal',
                                          3, 2)
    shot_data$SHOT_VALUE <- (as.numeric(as.character(shot_data$SHOT_MADE_FLAG))
                             * shot_data$SHOT_TYPE_NUMERIC)
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
shots_dark_colors <- c('#4B0082', '#8A2BE2', '#1C86EE', '#66CD00', '#FFE600', '#FF6600', '#FC1501')

## Function to create shot chart
shotChartScatter <- function(shot_data, color_gradient) {
    
    if (length(color_gradient) == 7) {
        shot_chart <- try(ggplot(shot_data, aes(x = LOC_X, y = LOC_Y)) +
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
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = 264/scale_factor, y = -48/scale_factor,
                                               xend = 264/scale_factor, yend = 120/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_curve(aes(x = -264/scale_factor, y = 120/scale_factor,
                                             xend = 264/scale_factor, yend = 120/scale_factor),
                                         color = '#D6D6C9',
                                         curvature = .68, ncp = 50, size = 1) +
                              geom_segment(aes(x = -300/scale_factor, y = -48/scale_factor,
                                               xend = 300/scale_factor, yend = -48/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = -96/scale_factor, y = 195/scale_factor,
                                               xend = 96/scale_factor, yend = 195/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = -96/scale_factor, y = -47.4/scale_factor,
                                               xend = -96/scale_factor, yend = 195/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = 96/scale_factor, y = -47.4/scale_factor,
                                               xend = 96/scale_factor, yend = 195/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = -72/scale_factor, y = -47.4/scale_factor,
                                               xend = -72/scale_factor, yend = 195/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = 72/scale_factor, y = -47.4/scale_factor,
                                               xend = 72/scale_factor, yend = 195/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = -36/scale_factor, y = 0,
                                               xend = 36/scale_factor, yend = 0),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = -3/scale_factor, y = 0,
                                               xend = -3/scale_factor, yend = 6/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_segment(aes(x = 3/scale_factor, y = 0,
                                               xend = 3/scale_factor, yend = 6/scale_factor),
                                           color = '#D6D6C9',
                                           size = 1) +
                              geom_curve(aes(x = 0, y = 6/scale_factor,
                                             xend = 0, yend = 24/scale_factor),
                                         color = '#D6D6C9',
                                         curvature = 1, ncp = 50, size = 1) +
                              geom_curve(aes(x = 0, y = 6/scale_factor,
                                             xend = 0, yend = 24/scale_factor),
                                         color = '#D6D6C9',
                                         curvature = -1, ncp = 50, size = 1) +
                              geom_curve(aes(x = -72/scale_factor, y = 195/scale_factor,
                                             xend = 72/scale_factor, yend = 195/scale_factor),
                                         color = '#D6D6C9',
                                         curvature = 1, ncp = 50, size = 1) +
                              geom_curve(aes(x = -72/scale_factor, y = 195/scale_factor,
                                             xend = 72/scale_factor, yend = 195/scale_factor),
                                         color = '#D6D6C9',
                                         linetype = 5, curvature = -1, ncp = 50, size = 1) +
                              theme(line = element_blank(),
                                    axis.text.x = element_blank(),
                                    axis.text.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.position = 'bottom',
                                    legend.box = 'horizontal',
                                    legend.key = element_blank(),
                                    panel.background = element_rect(fill = 'black')),
                          silent = TRUE)
    }
    if (length(color_gradient) == 5) {
        shot_chart <- try(ggplot(shot_data, aes(x = LOC_X, y = LOC_Y)) +
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
                                    panel.background = element_blank()),
                          silent = TRUE)
    }
    return(shot_chart)
}


all_players_stats_1 <- paste0("http://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=02%2F19%2F2017&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=")
all_players_stats_2 <- paste0("&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=")
all_players_stats_3 <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")

##"http://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=02%2F19%2F2017&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

## Function import and clean player data from NBA.com
readPlayer_stats <- function(home_away, season){
    address <- paste0(all_players_stats_1, home_away, all_players_stats_2, season, all_players_stats_3)
    web_page <- readLines(address)
    
    x1 <- gsub("[\\{\\}\\]]", "", web_page, perl = TRUE)
    x2 <- gsub("[\\[]", "\n", x1, perl = TRUE)
    x3 <- gsub("\"rowSet\":\n", "", x2, perl = TRUE)
    x4 <- gsub(";", ",",x3, perl = TRUE)
    
    nba <- read.table(textConnection(x4), header = TRUE, sep = ",", skip = 2,
                      stringsAsFactors = FALSE, fill = TRUE)
    
    nba <- nba[,1:ncol(nba) - 1]
    
    return(nba)
}


## Function to create horizontal bar charts
shot_chart_bar_chart <- function(title, nba, overall, zone){
    labels <- c("NBA", "Overall", "Zone")
    stats <- c(nba, overall, zone)
    data <- try(data.frame(labels, stats), silent = T)
    
    try(ggplot(data,
               aes(x = labels,
                   y = stats)) +
            ggtitle(title) +
            theme(plot.title = element_text(face="bold", size=14, hjust=.5)) +
            geom_text(aes(label = percent(round(stats,3))), hjust = -.25) +
            geom_bar(stat = "identity",
                     position = position_dodge(),
                     fill = nba_logo_colors) +
            scale_y_continuous(labels = percent) +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major.x = element_line(color = '#DCDCDC'),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank(), 
                  axis.line.x = element_line(color = "black",
                                             size = .5)) +
            geom_hline(yintercept = 0) +
            coord_flip(ylim = c(0, 1)),
        silent = T)
}



ui <- fluidPage(theme = "legronjames.css",
                fluidRow(div(style = "height:7px")),
                fluidRow(
                    ## Season selection
                    column(width = 2,
                           style = 'padding-left:12px;',
                           style = 'padding-right:0px;',
                           selectInput('season',
                                       'Season:',
                                       seasons_list,
                                       '2016-17')),
                    ## Team selection
                    column(width = 3,
                           style = 'padding-left:12px;',
                           style = 'padding-right:0px;',
                           selectInput('team',
                                       'Team:',
                                       default_teams,
                                       'Cleveland Cavaliers',
                                       width = '100%')),
                    column(width = 7,
                           style = 'padding-left:12px;',
                           style = 'padding-right:12px;',
                           selectInput('roster_names',
                                       'Players:',
                                       '',
                                       width = '100%'))
                ),
                fluidRow(
                    column(width = 3,
                           style = 'padding-left:12px;',
                           style = 'padding-right:0px;',
                           selectInput('home_road_flag',
                                       label = 'Location:',
                                       choices = c('Home and Road',
                                                   'Home',
                                                   'Road'),
                                       width = '100%')),
                    column(width = 3,
                           style = 'padding-left:12px;',
                           style = 'padding-right:0px;',
                           selectInput('shot_period',
                                       'Period:',
                                       periods,
                                       width = '100%')),
                    column(width = 3,
                           style = 'padding-left:12px;',
                           style = 'padding-right:0px;',
                           selectInput('shot_type',
                                       'Shot Type:',
                                       shot_type,
                                       width = '100%')),
                    column(width = 3,
                           style = 'padding-left:12px;',
                           style = 'padding-right:12px;',
                           selectInput('shot_chart_gradient',
                                       'Coloring:',
                                       c('NBA', 'Dark'),
                                       width = '100%'))
                ),
                ## Row four, shot charts
                fluidRow(
                    column(width = 6,
                           align="center",
                           htmlOutput('shot_chart_title'),
                           plotOutput('shot_chart',
                                      height = "475px",
                                      width = "600px",
                                      brush = brushOpts(
                                          id = 'shot_zone',
                                          fill = '#333333',
                                          stroke = '#000000'))),
                    column(width = 6,
                           fluidRow(style='padding-top:50px;',
                               column(width = 6,
                                      fluidRow(plotOutput('eFG_bar_chart',
                                                          height = '200px',
                                                          width = '300px')),
                                      fluidRow(style='padding-top:20px;',
                                               plotOutput('FG_bar_chart',
                                                          height = '200px',
                                                          width = '300px'))),
                               column(width = 6,
                                      fluidRow(plotOutput('FG3_bar_chart',
                                                          height = '200px',
                                                          width = '300px')),
                                      fluidRow(style='padding-top:20px;',
                                               plotOutput('FG2_bar_chart',
                                                          height = '200px',
                                                          width = '300px')))),
                           fluidRow(htmlOutput('zone_prompt')))
                    ),
                titlePanel(title = NULL, windowTitle = 'Interactive NBA Shot Chart')
)


server <- function(input, output, session) {
    ## Set team colors and team team list by season
    nba_colors <- reactive({
        set_colors_by_season(input$season)
    })
    ## Set team list
    observe({
        updateSelectInput(session,
                          'team',
                          'Team:',
                          choices = nba_colors()$name,
                          selected = 'Cleveland Cavaliers')
    })
    ## Set list of teams
    roster_data <- reactive({
        readTeam_roster(team_roster_url, input$season,
                        filter(nba_colors(),
                               name == input$team)$team_id_2016)
    })
    ## Set roster player list
    observe({
        updateSelectInput(session,
                          'roster_names',
                          'Players:',
                          roster_data()$PLAYER)
    })
    ## Fetch all shots
    all_shots <- reactive({
        selected_players <- filter(roster_data(),
                                   PLAYER %in% input$roster_names)
        player_ids <- as.vector(selected_players$PLAYER_ID)
        if (input$home_road_flag == 'Home and Road') {
            location <- ''
        }
        if (input$home_road_flag == 'Home') {
            location <- 'Home'
        }
        if (input$home_road_flag == 'Road') {
            location <- 'Road'
        }
        fetch_shots(player_ids, input$season, location)
    })
    ## Filter shots
    filtered_shots <- reactive({
        ## Filter by period
        if (input$shot_period == 'Game') {
            filtered_shots <- all_shots()
        }
        if (input$shot_period == '1st Half') {
            filtered_shots <- filter(all_shots(), PERIOD %in% c(1, 2))
        }
        if (input$shot_period == '2nd Half') {
            filtered_shots <- filter(all_shots(), PERIOD > 2)
        }
        if (input$shot_period == '1st Qtr') {
            filtered_shots <- filter(all_shots(), PERIOD  == 1)
        }
        if (input$shot_period == '2nd Qtr') {
            filtered_shots <- filter(all_shots(), PERIOD  == 2)
        }
        if (input$shot_period == '3rd Qtr') {
            filtered_shots <- filter(all_shots(), PERIOD  == 3)
        }
        if (input$shot_period == '4th Qtr') {
            filtered_shots <- filter(all_shots(), PERIOD  == 4)
        }
        if (input$shot_period == 'Overtime') {
            filtered_shots <- filter(all_shots(), PERIOD  > 4)
        }
        ## Filter by shot type
        if (input$shot_type == "Three's") {
            filtered_shots <- filter(filtered_shots, 
                                     SHOT_TYPE  == '3PT Field Goal')
        }
        if (input$shot_type == "Two's") {
            filtered_shots <- filter(filtered_shots,
                                     SHOT_TYPE  == '2PT Field Goal')
        }
        filtered_shots
    })
    ## Smooth filtered data
    smoothed_shots <- reactive({
        smoothShotData(filtered_shots())
    })
    ## Create shot chart scatter plot
    output$shot_chart <- renderPlot({
        if (input$shot_chart_gradient == 'NBA') {
            shot_chart_colors <- shots_nba_colors
        }
        if (input$shot_chart_gradient == 'Dark') {
            shot_chart_colors <- shots_dark_colors
        }
        shotChartScatter(smoothed_shots(), shot_chart_colors)
    })
    ## Prompt user to click and drag to select shot zone
    ## Calcuate average NBA FG%
    output$zone_prompt <- reactive({
        zone_data <- brushedPoints(smoothed_shots(), input$shot_zone)
        ifelse(sum(zone_data$FREQ) == 0,
               HTML(paste('<center>', '<b>',
                          'Click and drag to select shot zone',
                          '</b>', '</center>')),
               '')
    })
    ## Fetch player stats
    player_stats <- reactive({
        if (input$home_road_flag == 'Home and Road') {
            location <- ''
        }
        if (input$home_road_flag == 'Home') {
            location <- 'Home'
        }
        if (input$home_road_flag == 'Road') {
            location <- 'Road'
        }
        readPlayer_stats(location, input$season)
    })
    ## Create eFG% bar chart
    output$eFG_bar_chart <- renderPlot({
        percent_threes <- sum(player_stats()$FG3A)/sum(player_stats()$FGA)
        NBA_2pt_numeric <- sum(player_stats()$FGM -player_stats()$FG3M) / sum(player_stats()$FGA - player_stats()$FG3A)
        NBA_3pt_numeric <- sum(player_stats()$FG3M) / sum(player_stats()$FG3A)
        NBA_eFG_numeric <- NBA_2pt_numeric*(1-percent_threes) + NBA_3pt_numeric*3/2*percent_threes
        
        all_shots_eFG_numeric <- sum(smoothed_shots()$eFG * (smoothed_shots()$FREQ / sum(smoothed_shots()$FREQ)))
        
        
        zone_data <- brushedPoints(smoothed_shots(), input$shot_zone)
        zone_eFG_numeric <- sum(zone_data$eFG * (zone_data$FREQ / sum(zone_data$FREQ)))
        
        eFG_title <- 'eFG%'
        
        shot_chart_bar_chart(eFG_title, NBA_eFG_numeric, all_shots_eFG_numeric, zone_eFG_numeric)
    })
    ## Create FG% bar chart
    output$FG_bar_chart <- renderPlot({
        NBA_FG_numeric <- sum(player_stats()$FGM) / sum(player_stats()$FGA)
        
        all_shots_FG_numeric <- sum(ifelse(smoothed_shots()$SHOT_TYPE_NUMERIC == 3, smoothed_shots()$eFG/3*2,
                                           smoothed_shots()$eFG) *
                                        (smoothed_shots()$FREQ / sum(smoothed_shots()$FREQ)))
        
        zone_data <- brushedPoints(smoothed_shots(), input$shot_zone)
        zone_FG_numeric <- sum(ifelse(zone_data$SHOT_TYPE_NUMERIC == 3, zone_data$eFG/3*2,
                                      zone_data$eFG) *
                                   (zone_data$FREQ / sum(zone_data$FREQ)))
        
        FG_title <- 'FG%'
        
        shot_chart_bar_chart(FG_title, NBA_FG_numeric, all_shots_FG_numeric, zone_FG_numeric)
    })
    ## Create 3PT% bar chart
    output$FG3_bar_chart <- renderPlot({
        NBA_3pt_numeric <- sum(player_stats()$FG3M) / sum(player_stats()$FG3A)
        
        all_shots_data_3pt <- filter(smoothed_shots(), SHOT_TYPE_NUMERIC == 3)
        all_shots_3ptFG_numeric <- sum(all_shots_data_3pt$eFG/3*2*
                                           (all_shots_data_3pt$FREQ / sum(all_shots_data_3pt$FREQ)))
        
        zone_data <- brushedPoints(smoothed_shots(), input$shot_zone)
        zone_data_3pt <- filter(zone_data, SHOT_TYPE_NUMERIC == 3)
        zone_3ptFG_numeric <- sum(zone_data_3pt$eFG/3*2*
                                      (zone_data_3pt$FREQ / sum(zone_data_3pt$FREQ)))
        
        FG3_title <- '3pt FG%'
        
        shot_chart_bar_chart(FG3_title, NBA_3pt_numeric, all_shots_3ptFG_numeric, zone_3ptFG_numeric)
    })
    ## Create 2PT% bar chart
    output$FG2_bar_chart <- renderPlot({
        NBA_2pt_numeric <- sum(player_stats()$FGM -player_stats()$FG3M) / sum(player_stats()$FGA - player_stats()$FG3A)
        
        all_shots_data_2pt <- filter(smoothed_shots(), SHOT_TYPE_NUMERIC == 2)
        all_shots_2ptFG_numeric <- sum(all_shots_data_2pt$eFG*
                                           (all_shots_data_2pt$FREQ / sum(all_shots_data_2pt$FREQ)))        
        
        zone_data <- brushedPoints(smoothed_shots(), input$shot_zone)
        zone_data_2pt <- filter(zone_data, SHOT_TYPE_NUMERIC == 2)
        zone_2ptFG_numeric <- sum(zone_data_2pt$eFG*
                                      (zone_data_2pt$FREQ / sum(zone_data_2pt$FREQ)))
        
        FG2_title <- '2pt FG%'
        
        shot_chart_bar_chart(FG2_title, NBA_2pt_numeric, all_shots_2ptFG_numeric, zone_2ptFG_numeric)
    })
    ## Get player data
    ## Filer user selected shots
    output$player_data <- renderDataTable({
        filter(roster_data(), PLAYER == input$roster_names)
    })
    
    output$shot_chart_title <- reactive({
        player_data <- filter(roster_data(), PLAYER == input$roster_names)
        shot_chart_title <- HTML(paste("<center>",
                                       '<font size = "6">',
                                       "<b>",
                                       try(paste0(player_data$PLAYER, ' - #', player_data$NUM), silent = T),
                                       '</font>',
                                       "</b>",
                                       "</center>"))
        ifelse(length(player_data$PLAYER) == 0, '', shot_chart_title)
    })
    
}

shinyApp(ui, server)





