import requests
import pandas as pd
import json
from config import headers


def get_nba_com_dataframe(url, params, headers):
    response = requests.get(url, params=params, headers=headers, timeout=30)
    print(f'    CONNECTING TO - {url}')
    print(f'    {response.url}')
    print(f'    {response.status_code}')
    json_data = response.json()
    if 'resultSets' in json_data.keys():
        result_set_json = json_data['resultSets']
    elif 'resultSet' in json_data.keys():
        result_set_json = json_data['resultSet']
    main_result_json = result_set_json[0]
    main_headers = main_result_json['headers']
    main_rowset_data = main_result_json['rowSet']
    main_response_df = pd.DataFrame(data=main_rowset_data, columns=main_headers)
    if len(result_set_json) == 2:
        secondary_result_json = result_set_json[1]
        secondary_headers = secondary_result_json['headers']
        secondary_rowset_data = secondary_result_json['rowSet']
        secondary_response_df = pd.DataFrame(data=secondary_rowset_data,
                                            columns=secondary_headers)
        return main_response_df, secondary_response_df
    else:
        return main_response_df

def add_season_column(nba_df, season_string):
    nba_df['SEASON'] = season_string
    nba_df_columns = nba_df.columns.values.tolist()
    nba_df_columns.insert(2, 'SEASON')
    nba_df_columns = nba_df_columns[:-1]
    nba_df = nba_df[nba_df_columns]
    return nba_df



teams_url = 'http://stats.nba.com/stats/leaguedashteamstats'
teams_params = {'Conference': '',
                'DateFrom': '',
                'DateTo': '',
                'Division': '',
                'GameScope': '',
                'GameSegment': '',
                'LastNGames': 0,
                'LeagueID': '00',
                'Location': '',
                'MeasureType': 'Base',
                'Month': 0,
                'OpponentTeamID': 0,
                'Outcome': '',
                'PORound': 0,
                'PaceAdjust': 'N',
                'PerMode': 'PerGame',
                'Period': 0,
                'PlayerExperience': '',
                'PlayerPosition': '',
                'PlusMinus': 'N',
                'Rank': 'N',
                'Season': '2017-18',
                'SeasonSegment': '',
                'SeasonType': 'Regular Season',
                'ShotClockRange': '',
                'StarterBench': '',
                'TeamID': 0,
                'VsConference': '',
                'VsDivision': ''
                }

## teams_df = get_nba_com_dataframe(teams_url, teams_params, headers)
## print(teams_df)
## print(teams_df.columns)

cavs_2017_18_team_id = 1610612739
rosters_url = 'http://stats.nba.com/stats/commonteamroster'
rosters_params = {'LeagueID': '00',
                    'Season': '2017-18',
                    'TeamID': cavs_2017_18_team_id
                    }

## roster_df, coaches_df = get_nba_com_dataframe(rosters_url, rosters_params, headers)
## print(roster_df)
## print(coaches_df)


kyrie_player_id = 202681
cavs_2017_18_team_id = 1610612739
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
                'OpponentTeamID': 0,
                'Month': 0,
                'Outcome': '',
                'Period': 0,
                'Position': '',
                'RookieYear': '',
                'SeasonSegment': '',
                'SeasonType': 'Regular Season',
                'TeamID': 0,
                'VsConference': '',
                'VsDivision': ''
                }


player_shots_df, league_average_shots_df = get_nba_com_dataframe(shots_url, shots_params, headers)
print(player_shots_df)
print(league_average_shots_df)

players_url = 'http://stats.nba.com/stats/leaguedashplayerstats'
players_params = {'College': '',
                    'Conference': '',
                    'Country': '',
                    'DateFrom': '',
                    'DateTo': '',
                    'Division': '',
                    'DraftPick': '',
                    'DraftYear': '',
                    'GameScope': '',
                    'GameSegment': '',
                    'Height': '',
                    'LastNGames': 0,
                    'LeagueID': '00',
                    'Location': '',
                    'MeasureType': 'Base',
                    'Month': 0,
                    'OpponentTeamID': 0,
                    'Outcome': '',
                    'PORound': 0,
                    'PaceAdjust': 'N',
                    'PerMode': 'Totals',
                    'Period': 0,
                    'PlayerExperience': '',
                    'PlayerPosition': '',
                    'PlusMinus': 'N',
                    'Rank': 'N',
                    'Season': '2017-18',
                    'SeasonSegment': '',
                    'SeasonType': 'Regular Season',
                    'ShotClockRange': '',
                    'StarterBench': '',
                    'TeamID': 0,
                    'VsConference': '',
                    'VsDivision': '',
                    'Weight': ''
                    }


## players_df = get_nba_com_dataframe(players_url, players_params, headers)
## print(players_df)
