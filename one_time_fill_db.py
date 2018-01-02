import pandas as pd
from config import headers, nba_db_dict
from nba_connection import get_nba_com_dataframe
from sqlalchemy import create_engine
from datetime import datetime

nba_engine_string = f'mysql://{nba_db_dict["user"]}:{nba_db_dict["password"]}@{nba_db_dict["host"]}:{nba_db_dict["port"]}/nba_stats'
nba_db_engine = create_engine(nba_engine_string)



first_season_start_year = 2007
today = datetime.now()
current_month = today.month
current_year = today.year
if current_month >= 7:
    current_season_start_year = current_year
else:
    current_season_start_year = current_year - 1

seasons_list = []
for year in range(first_season_start_year, current_season_start_year+1):
    seasons_list.append(f'{year}-{str(year+1)[2:]}')

## this can be more dynamic for yearly, by player id, team id, etc
## take a field that should be updated for each query
## take a list of values for that field (not just seasons lists)
## can also write this to database table if wanted
## def_update_nba_df(api_url, params, param_to_iterate, param_list, headers):
def get_updated_yearly_nba_df(api_url, season_list, params, headers):
    df = pd.DataFrame()
    for season in season_list:
        params['Season'] = season
        print(f'Getting <insert table name> data for {season}')
        single_year_df = get_nba_com_dataframe(api_url, params, headers)
        single_year_df['SEASON'] = season
        df = pd.concat([df, single_year_df])
    return df




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
                   'Season': '',
                   'SeasonSegment': '',
                   'SeasonType': 'Regular Season',
                   'ShotClockRange': '',
                   'StarterBench': '',
                   'TeamID': 0,
                   'VsConference': '',
                   'VsDivision': '',
                   'Weight': ''
                   }
players_df = get_updated_yearly_nba_df(players_url, seasons_list, players_params, headers)
print(players_df)

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
                'Season': '',
                'SeasonSegment': '',
                'SeasonType': 'Regular Season',
                'ShotClockRange': '',
                'StarterBench': '',
                'TeamID': 0,
                'VsConference': '',
                'VsDivision': ''
                }

teams_df = get_updated_yearly_nba_df(teams_url, seasons_list, players_params, headers)




if len(teams_df) >= 28 * len(seasons_list):
    teams_df.to_sql(con=nba_db_engine, name='past_teams',
                    if_exists='replace', index=False)
    print('Updated `past_teams` table')
else:
    print(f'Cannot update. Invalid data returned')
    print(f'Expected 30 teams. Received data for {len(teams_df)} Teams')
