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

  raw_data <- data$resultSets[[1]]$rowSet
  col_names <- as.character(data$resultSets[[1]]$headers)

  if (length(raw_data) == 0) {
      response_df <- data.frame(
          matrix(nrow = 0, ncol = length(col_names))
      )
  } else {
      response_df = data.frame(
          matrix(
              unlist(raw_data),
              ncol = length(col_names),
              byrow = TRUE
          )
      )
  }

  response_df <- tbl_df(response_df)
  names(response_df) <- col_names
  response_df <- mutate(response_df,
                        PERIOD = as.numeric(as.character(PERIOD)),
                        SHOT_DISTANCE = as.numeric(as.character(SHOT_DISTANCE)),
                        SHOT_MADE_NUMERIC = as.numeric(as.character(SHOT_MADE_FLAG)),
                        SHOT_ATTEMPTED_FLAG = as.numeric(as.character(SHOT_ATTEMPTED_FLAG)))
  return(response_df)
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
df <- nba_api_request(shot_chart_url, kyrie_query, headers)
print(df)
