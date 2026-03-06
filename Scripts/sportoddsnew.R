# Libraries
library(lubridate)
library(oddsapiR)
library(tidyverse)

################
# Get MLB Odds #
################

#https://cran.r-project.org/web/packages/oddsapiR/oddsapiR.pdf

# Set my key
#Sys.setenv(ODDS_API_KEY = "472f239d2d376b51c2b7834306dc767d")

# Get todays Odds - only run this one as it costs usage
odds <- toa_sports_odds(sport_key = 'baseball_mlb',
                     regions = 'us',
                     markets = "h2h", # this is the money line
                     odds_format = 'american',
                     date_format = 'iso')

# Clean columns
odds_clean <- odds %>% 
  select(commence_time, home_team, away_team, bookmaker, outcomes_name, outcomes_price)

# Convert the datetime column to Date format
odds_clean <- odds_clean %>%
  mutate(Date = ymd_hms(commence_time, tz = "UTC")) 

# Define tomorrow's date and time range
start_time <- ymd_hms(paste(Sys.Date(), "00:00:00"), tz = "UTC")
end_time <- ymd_hms(paste(Sys.Date() + 1, "05:00:00"), tz = "UTC")

# Filter to include ONLY rows from tomorrow at or after 05:00 UTC
odds_clean <- odds_clean %>%
  filter(Date >= start_time & Date <= end_time) %>% 
  select(Date, home_team:outcomes_price)

#Change Date to todays Date
odds_clean$Date = Sys.Date()

# Filter fandual and draftkings
##used to be 'odds_filter' but made it today' bc date was wrong today <- subset(odds_filter)
today <- odds_clean %>% 
  filter(bookmaker == "DraftKings") %>% 
  mutate(home_team = ifelse(home_team == "Oakland Athletics", "Athletics", home_team), # This is to address the Oakland A's change in 2025
         away_team = ifelse(away_team == "Oakland Athletics", "Athletics", away_team),
         outcomes_name = ifelse(outcomes_name == "Oakland Athletics", "Athletics", outcomes_name))

# All underdogs
alldogs <- today %>% 
  #Group by the game, used commence_time instead of away team in case of double headers
  group_by(home_team, Date) %>% 
  slice(which.max(outcomes_price)) %>% 
  ungroup()

# Find road underdog
today_dog <- today %>% 
  #Group by the game, used commence_time instead of away team in case of double headers
  group_by(home_team, Date) %>% 
  slice(which.max(outcomes_price)) %>% 
  #The above gives ALL dogs, next code gives ROAD dogs
  filter(away_team == outcomes_name) %>% 
  ungroup()


# Clean Enviornment
rm(odds, odds_clean, today, end_time, start_time)


# How many usages left. 500per month
#toa_requests()
