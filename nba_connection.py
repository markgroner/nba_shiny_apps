import requests
import pandas as pd
import json
headers = {"Accept-Language": "en-US,en;q=0.5","User-Agent": "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0","Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", "Referer": "http://markgroner.com ","Connection": "keep-alive" }



kyrie_player_id = 202681
shots_url = 'http://stats.nba.com/stats/shotchartdetail'
shots_params = {'PlayerID': kyrie_player_id,
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
## print('    CONNECTING TO SHOT DATA API .... ')
## print(f'    {players_response.status_code}')
## print(f'    {players_response.url}')
## shots_response = requests.get(shots_url, params=shots_params, headers=headers, timeout =30)

## shots_data = shots_response.json()
## print(json.dumps(shots_data, indent=4, sort_keys=True))
## print(len(shots_data))
## shots_df = pd.DataFrame(shots_data)
## print(shots_df)


players_url = 'http://stats.nba.com/stats/leagueLeaders'
players_params = {'LeagueID': '00',
                    'PerMode': 'PerGame',
                    'Scope': 'S',
                    'Season': '2017-18',
                    'SeasonType': 'Regular Season',
                    'StatCategory': 'PTS'
                    }
## print('    CONNECTING TO PLAYER DATA API .... ')
## players_response = requests.get(players_url, params=players_params, headers=headers, timeout=30)
## print(f'    {players_response.status_code}')
## print(f'    {players_response.url}')
## players_data = players_response.json()

## print(json.dumps(players_data, indent=4, sort_keys=True))
## print(len(players_data))
## players_df = pd.DataFrame(players_response)
## print(players_df)


def get_nba_com_dataframe(url, params):
    headers = {'Accept-Language': 'en-US,en;q=0.5',
                'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0',
                'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
                'Referer': 'http://markgroner.com',
                'Connection': 'keep-alive'}
    response = requests.get(url, params=params, headers=headers, timeout=30)
    json_data = response.json()
    print(f'    CONNECTING TO - {response.url}')
    print(f'    {response.status_code}')
    headers = json_data['resultSet']['headers']
    rowset_data = json_data['resultSet']['rowSet']
    ## print(json.dumps(json_data, indent=4, sort_keys=True))
    ## print(len(json_data))
    response_df = pd.DataFrame(data=rowset_data, columns=headers)

    print(response_df)

get_nba_com_dataframe(players_url, players_params)
