library(httr)
library(jsonlite)
library(dplyr)



nba_api_request <- function(api_url, parameters, headers) {
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

kyrie_player_id <- 202681
kyrie_query <- list(
  PlayerID = kyrie_player_id,
  PlayerPosition = '',
  Season = '2016-17',
  ContextMeasure = 'FGA',
  DateFrom = '',
  DateTo = '',
  GameID = '',
  GameSegment = '',
  LastNGames = 0,
  LeagueID = '00',
  Location = '',
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
shot_chart_url <- 'http://stats.nba.com/stats/shotchartdetail'
headers <- c('Accept-Language' = 'en-US,en;q=0.5',
              'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0',
              Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
              Referer = 'http://markgroner.com',
              Connection = 'keep-alive')
response_df <- nba_api_request(shot_chart_url, kyrie_query, headers)
print(response_df)
