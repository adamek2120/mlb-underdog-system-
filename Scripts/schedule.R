
library(baseballr)

# DATE
date <- Sys.Date()

# Function's
## Today's Game ID
games <- function(date){
  get_game_pks_mlb(date, level_ids = 1)
}
## Today's Starting Pitcher
away_sp <- function(df) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[i] <- get_probables_mlb(df[[i]])[1,3]
  }
  output
}
home_sp <- function(df) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[i] <- get_probables_mlb(df[[i]])[2,3]
  }
  output
}


# Pulling in Today's Game Information
## Today's games 
var_keep <- c("game_pk", "officialDate", "gamesInSeries", "seriesGameNumber",
              "teams.away.team.name", "teams.away.leagueRecord.pct",
               "teams.home.team.name", "teams.home.leagueRecord.pct")
todays_games <- games(date)
todays_games <- todays_games[, var_keep]

## Today's Schedule - NEW I ADDED GAMEpk
todays_schedule <- data.frame(
  GamePk = todays_games$game_pk,
  Date = as.Date(todays_games$officialDate, "%Y-%m-%d"),
  HomeTeam = todays_games$teams.home.team.name,
  AwayTeam = todays_games$teams.away.team.name,
  HomeSP = unlist(home_sp(todays_games$game_pk)),
  AwaySP = unlist(away_sp(todays_games$game_pk)),
  GameinSeries = todays_games$gamesInSeries,
  SeriesGame = todays_games$seriesGameNumber,
  HomeWinPer = todays_games$teams.home.leagueRecord.pct,
  AwayWinPer = todays_games$teams.away.leagueRecord.pct
)

############ NEW

# Get pitchers
pitchers <- mlb_stats(stat_type = 'season', stat_group = 'pitching', position = 'P', season = 2025, player_pool = "All") %>% 
  select(player_full_name, obp, slg, ops, home_runs_per9, hits_per9inn, walks_per9inn, strikeouts_per9inn, strikeout_walk_ratio, pitches_per_inning, 
         ground_outs_to_airouts, strike_percentage, stolen_base_percentage)



# Clean Enviornment
rm(date, games, home_sp, away_sp, var_keep, todays_games)




