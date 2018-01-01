library(httr)
library(jsonlite)



nba_api_request <- function(api_url, parameters, headers) {
  request <- GET(
    api_url,
    query = parameters,
    headers
    )
  request_status <- stop_for_status(request)
  print(request_status)
  request_data <- content(request)
  print(request_data)
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
headers <- list('Accept-Language' = 'en-US,en;q=0.5',
                'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0',
                Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
                Referer = 'http://markgroner.com',
                Connection = 'keep-alive')
nba_api_request(shot_chart_url, kyrie_query, headers)



