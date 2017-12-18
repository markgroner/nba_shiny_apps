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
teams_df = pd.DataFrame()
if current_month >= 7:
    current_season_start_year = current_year
else:
    current_season_start_year = current_year - 1

## Update past seasons team statistics
teams_url = 'http://stats.nba.com/stats/leaguedashteamstats'
for year in range(first_season_start_year, current_season_start_year):
    season_string = f'{year}-{str(year+1)[2:]}'
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
                    'Season': season_string,
                    'SeasonSegment': '',
                    'SeasonType': 'Regular Season',
                    'ShotClockRange': '',
                    'StarterBench': '',
                    'TeamID': 0,
                    'VsConference': '',
                    'VsDivision': ''
                    }

    single_year_teams_df = get_nba_com_dataframe(teams_url, teams_params, headers)
    single_year_teams_df['SEASON'] = season_string
    teams_df = pd.concat([teams_df, single_year_teams_df])
teams_df = add_season_column(teams_df)
if len(teams_df) >= 30:
    teams_df.to_sql(con=nba_db_engine, name='past_teams',
                    if_exists='replace', index=False)
    print('Updated `past_teams` table')
else:
    print(f'Cannot update. Invalid data returned')
    print(f'Expected 30 teams. Received data for {len(teams_df)} Teams')
