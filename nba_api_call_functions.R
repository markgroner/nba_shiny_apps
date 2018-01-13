library(httr)
library(jsonlite)
library(dplyr)


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
  request_status <- message_for_status(request)
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

## query <- build_shot_chart_parameters(kyrie_player_id, '2016-17', '')
## kyrie_query <- list(
##   PlayerID = kyrie_player_id,
##   PlayerPosition = '',
##   Season = '2016-17',
##   ContextMeasure = 'FGA',
##   DateFrom = '',
##   DateTo = '',
##   GameID = '',
##   GameSegment = '',
##   LastNGames = 0,
##   LeagueID = '00',
##   Location = '',
##   Month = 0,
##   OpponentTeamID = 0,
##   Outcome = '',
##   Period = 0,
##   Position = '',
##   RookieYear = '',
##   SeasonSegment = '',
##   SeasonType = 'Regular Season',
##   TeamID = 0,
##   VsConference = '',
##   VsDivision = '')


get_nba_roster <- function(season, team_id) {
  team_roster_url <- 'http://stats.nba.com/stats/commonteamroster'
  roster_parameters <- list(
    LeagueID = '00',
    Season = season,
    TeamID = team_id)
  roster_df <- nba_api_request(team_roster_url, roster_parameters)
  return(roster_df)
}

get_player_stats <- function(home_away_flag, season) {
  player_stats_url <- 'http://stats.nba.com/stats/leaguedashplayerstats'
  player_stats_parameters <- list(
    College = '',
    Conference = '',
    Country = '',
    DateFrom = '',
    DateTo = '',
    Division = '',
    DraftPick = '',
    DraftYear = '',
    GameScope = '',
    GameSegment = '',
    Height = '',
    LastNGames = 0,
    LeagueID = '00',
    Location = home_away_flag,
    MeasureType = 'Base',
    Month = 0,
    OpponentTeamID = 0,
    Outcome = '',
    PORound = 0,
    PaceAdjust = 'N',
    PerMode = 'Totals',
    Period = 0,
    PlayerExperience = '',
    PlayerPosition = '',
    PlusMinus = 'N',
    Rank = 'N',
    Season = season,
    SeasonSegment = '',
    SeasonType = 'Regular Season',
    ShotClockRange = '',
    StarterBench = '',
    TeamID = 0,
    VsConference = '',
    VsDivision = '',
    Weight = '')
  player_stats_df <- nba_api_request(player_stats_url, player_stats_parameters)
  return(player_stats_df)
}

get_lineup_shots <- function(player_id_list, season, location) {
    team_lineup_shots <- data.frame()
    for (player_id in player_id_list) {
        player_lineup_shots <- get_nba_shots(player_id, season, location)
        team_lineup_shots <- rbind(team_lineup_shots, player_lineup_shots)
    }
    return(team_lineup_shots)
}

## kyrie_player_id <- 202681
## response_df <- get_nba_shots(kyrie_player_id, '2016-17', '')
## print(NROW(response_df))
## response_df <- get_lineup_shots(c(kyrie_player_id, kyrie_player_id), '2016-17', '')
## print(NROW(response_df))

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

lineup_stats_df <- get_lineup_stats('Advanced', '2017-18', '', 'Totals')
print(lineup_stats_df)
