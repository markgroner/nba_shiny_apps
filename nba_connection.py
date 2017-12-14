import requests
print('jello')
import pandas as pd
print('HELLO')
import json
kyrie_player_id = 202681
lineup_url = 'http://stats.nba.com/stats/shotchartdetail'
params = {'PlayerID': kyrie_player_id,
            'PlayerPosition': '',
            'Season': '',
            'ContextMeasure': 'FGA',
            'DateFrom': '',
            'DateTo': '',
            'GameID': '',
            'GameSegment': '',
            'LastNGames': 0,
            'LeagueID': '00',
            'Location': '',
            'Month': 0,
            'OpponentTeamID': 0,
            'Outcome': '',
            'Period': 0,
            'Position': '',
            'RookieYear': '',
            'SeasonSegment': '',
            'SeasonType': 'Regular Season',
            'TeamID': 0,
            'VsConference': '',
            'VsDivision': ''}
print('connecting .... ')
headers = {"Accept-Language": "en-US,en;q=0.5","User-Agent": "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0","Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", "Referer": "http://markgroner.com ","Connection": "keep-alive" }
response = requests.get(lineup_url, params=params, headers=headers, timeout =30)

response_data = response.json()
print(json.dumps(response_data, indent=4, sort_keys=True))
print(len(response_data))
## response_df = pd.DataFrame(response_data)
## print(response_df)
