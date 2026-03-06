#https://stackoverflow.com/questions/54132853/how-to-collapse-a-dataframe-with-duplicate-ids-and-varying-missing-values-per-id
library(tidyr)
library(baseballr)
#
# This takes the output from sportsodd.R and schedule.R..
# Pulls in standing data
# And finalizes a final decison making df
#

# Get current standings 
## AL
alstanding <- mlb_standings(
  season = 2025,
  league_id = 103
)
alstanding <- alstanding %>% 
  select(team_records_team_name, team_records_runs_allowed, team_records_runs_scored, team_records_run_differential, team_records_streak_streak_type, team_records_streak_streak_number, team_records_records_split_records)
## NL
nlstanding <- mlb_standings(
  season = 2025,
  league_id = 104
)
nlstanding <- nlstanding %>% 
  select(team_records_team_name, team_records_runs_allowed, team_records_runs_scored, team_records_run_differential, team_records_streak_streak_type, team_records_streak_streak_number, team_records_records_split_records)

# Create DF of teams and splits
standing_splitAL <- unnest(alstanding, team_records_records_split_records) %>% 
  filter(type == "home" | type == "away" | type == "lastTen")
standing_splitNL <- unnest(nlstanding, team_records_records_split_records) %>% 
  filter(type == "home" | type == "away" | type == "lastTen")

# Final DF
AL_split <- standing_splitAL %>% 
  pivot_wider(names_from = type, values_from = pct) %>% 
  group_by(team_records_team_name) %>% 
  fill(home, away, lastTen) %>% 
  fill(home, away, lastTen, .direction = 'up') %>%
  distinct(team_records_team_name, .keep_all = TRUE) %>% 
  rename("team" = "team_records_team_name",
         "ra" = "team_records_runs_allowed",
         "rs" = "team_records_runs_scored",
         "diff" = "team_records_run_differential",
         "streak" = "team_records_streak_streak_type",
         "streak_num" = "team_records_streak_streak_number")
  
NL_split <- standing_splitNL %>% 
  pivot_wider(names_from = type, values_from = pct) %>% 
  group_by(team_records_team_name) %>% 
  fill(home, away, lastTen) %>% 
  fill(home, away, lastTen, .direction = 'up') %>%
  distinct(team_records_team_name, .keep_all = TRUE)%>% 
  rename("team" = "team_records_team_name",
         "ra" = "team_records_runs_allowed",
         "rs" = "team_records_runs_scored",
         "diff" = "team_records_run_differential",
         "streak" = "team_records_streak_streak_type",
         "streak_num" = "team_records_streak_streak_number")

team_split <- rbind(AL_split, NL_split)

rm(alstanding, nlstanding, standing_splitAL, standing_splitNL, AL_split, NL_split)



