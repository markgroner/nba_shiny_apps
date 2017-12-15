import requests
import pandas as pd
import json

kyrie_player_id = 202681
shots_url = 'http://stats.nba.com/stats/shotchartdetail'
shots_params = {'PlayerID': kyrie_player_id,
                        'PlayerPosition': '',
                        'Season': '2017-18',
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


players_url = 'http://stats.nba.com/stats/leagueLeaders'
players_params = {'LeagueID': '00',
                    'PerMode': 'PerGame',
                    'Scope': 'S',
                    'Season': '2017-18',
                    'SeasonType': 'Regular Season',
                    'StatCategory': 'PTS'
                    }



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
    ## print(json.dumps(json_data, indent=4, sort_keys=True))
    if 'resultSets' in json_data.keys():
        result_set_json = json_data['resultSets'][0]
    elif 'resultSet' in json_data.keys():
        result_set_json = json_data['resultSet']
    headers = result_set_json['headers']
    rowset_data = result_set_json['rowSet']
    response_df = pd.DataFrame(data=rowset_data, columns=headers)
    return response_df

## shots_df = get_nba_com_dataframe(shots_url, shots_params)
## print(shots_df)


## players_df = get_nba_com_dataframe(players_url, players_params)
## print(players_df)
