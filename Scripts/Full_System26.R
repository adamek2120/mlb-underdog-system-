##########
# Library
##########
library(knitr)
library(kableExtra)
library(dplyr)
library(baseballr)
library(googlesheets4)


#############################
# Input code for algorithm
############################

# Model's Path
#path <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
path <- getwd()
file1 <-"/Scripts/sportoddsnew.R" # Input Betting Odds: today_dog, alldogs
file2 <-"/Scripts/schedule.R" # Input information about schedule: todays_schedule, pitchers, 
file3 <- "/Scripts/standings.R" # Input information about team standings: team_split
file4 <- "/Scripts/teamrankingAdded.R" # Input Team rankings: pred, hom, awy, l10, consist, v1_5, v6_10
source(paste0(path,file1))
source(paste0(path,file2))
source(paste0(path,file3))
source(paste0(path,file4))
rm(path, file1, file2, file3, file4)


#########################################
# Merge with schedule.R and standings.R
#########################################

# Ensure join columns are character type
today_dog$away_team <- as.character(today_dog$away_team)
today_dog$home_team <- as.character(today_dog$home_team)
alldogs$away_team <- as.character(alldogs$away_team)
alldogs$home_team <- as.character(alldogs$home_team)

today_merged <- today_dog %>%
  left_join(todays_schedule, by = c("away_team" = "AwayTeam")) %>%
  rename("Dog" = "outcomes_name",
         "Dog_price" = "outcomes_price",
         "Date" = "Date.x") %>% 
  select(Date:away_team, HomeSP:AwayWinPer, Dog, Dog_price) %>% 
  left_join(team_split, by = c("away_team" = "team")) %>% 
  rename("Dog_streak" = "streak",
         "Dog_streakN" = "streak_num",
         "Dog_away_per" = "away",
         "Dog_L10" = "lastTen",
         "Dog_diff" = "diff") %>% 
  select(Date:Dog_price, Dog_streak, Dog_streakN, Dog_away_per, Dog_L10, Dog_diff) %>% 
  left_join(team_split, by = c("home_team" = "team")) %>% 
  rename("Fav_streak" = "streak",
         "Fav_streakN" = "streak_num",
         "Fav_home_per" = "home",
         "Fav_L10" = "lastTen",
         "Fav_diff" = "diff") %>%
  select(-c(ra,rs,wins,losses,away))

##########################
# Teamranking Data
###########################

# Add's Rank to rating
pred <- pred %>%
  mutate(Pred_Rank = row_number())

# Cleans & merges code
tr_merge <- alldogs %>%
  left_join(todays_schedule, by = c("away_team" = "AwayTeam")) %>%
  rename("Dog" = "outcomes_name",
         "Dog_price" = "outcomes_price",
         "Date" = "Date.x") %>% 
  select(Date:away_team, HomeSP:AwayWinPer, Dog, Dog_price) %>% 
  left_join(team_split, by = c("away_team" = "team")) %>% 
  rename("Dog_streak" = "streak",
         "Dog_streakN" = "streak_num",
         "Dog_away_per" = "away",
         "Dog_L10" = "lastTen",
         "Dog_diff" = "diff") %>% 
  select(Date:Dog_price, Dog_streak, Dog_streakN, Dog_away_per, Dog_L10, Dog_diff) %>% 
  left_join(team_split, by = c("home_team" = "team")) %>% 
  rename("Fav_streak" = "streak",
         "Fav_streakN" = "streak_num",
         "Fav_home_per" = "home",
         "Fav_L10" = "lastTen",
         "Fav_diff" = "diff") %>%
  select(-c(ra,rs,wins,losses,away))

tr_merge <- tr_merge %>% 
  # Pred
  left_join(pred, by = c("home_team" = "Team")) %>% 
  rename("Home_Pred_Rating" = "Pred_Rating",
         "Home_Pred_Rank" = "Pred_Rank") %>% 
  left_join(pred, by = c("away_team" = "Team")) %>% 
  rename("Away_Pred_Rating" = "Pred_Rating",
         "Away_Pred_Rank" = "Pred_Rank") %>% 
  # home
  left_join(hom, by = c("home_team" = "Team")) %>% 
  # away
  left_join(awy, by = c("away_team" = "Team")) %>% 
  # l10
  left_join(l10, by = c("home_team" = "Team")) %>% 
  rename("Home_l10_Rating" = "L10_Rating") %>% 
  left_join(l10, by = c("away_team" = "Team")) %>% 
  rename("Away_l10_Rating" = "L10_Rating") %>% 
  # Consist
  left_join(consist, by = c("home_team" = "Team")) %>% 
  rename("Home_Consist_Rating" = "Consist_Rating") %>% 
  left_join(consist, by = c("away_team" = "Team")) %>% 
  rename("Away_Consist_Rating" = "Consist_Rating") %>% 
  # V1-5
  left_join(v1_5, by = c("home_team" = "Team")) %>% 
  rename("Home_v1_5_Rating" = "V1_5_Rating") %>% 
  left_join(v1_5, by = c("away_team" = "Team")) %>% 
  rename("Away_v1_5_Rating" = "V1_5_Rating") %>% 
  # v6-10
  left_join(v6_10, by = c("home_team" = "Team")) %>% 
  rename("Home_v6_10_Rating" = "V6_10_Rating") %>% 
  left_join(v6_10, by = c("away_team" = "Team")) %>% 
  rename("Away_v6_10_Rating" = "V6_10_Rating")

# Add empty column for tracking
tr_merge$DogWinGame_inSeries <- ""

# Filter for only first game in series - I'll only run this when there's a start of series
tr_merge <- tr_merge %>% 
  filter(SeriesGame == 1)

#################################################################
# Road Dog Criteria: Main UD Picks, DT4, The Dip, and Petricho
#################################################################

### Main ###
# 1) Start with Road Dogs					
# 2) Favorite must not have a winning streak of 3 or more games	(FLAG_strk)				
# 3) Dog must not be on a losing streak of 3 or more games	(FLAG_strk)				
# 4) Dog must be at least 4-6 in L10 (FLAG_L10) and at least .400 on the road (FLAG_Away)				
# 5) Favorite must not be better then 6-4 in L10 (FLAG_L10)

### DT-4 ###
# 1)

### The Dip ###
# 1)
# 2)

### Petricho ###
# 1)
# 2

#########################
# Today's System Picks
#########################
crit1_today <- tr_merge %>% 
  mutate( # Decision Tree - 4 rules
    DT4 = case_when(away_team == Dog & as.numeric(Dog_away_per) > 0.39 & as.numeric(Away_l10_Rating) > -1.55 &
                      as.numeric(Home_l10_Rating) <= 0.16 & 
                      (Dog_streak == "losses" | (Dog_streak == "wins" & Dog_streak <= 2)) ~ "Met",
                    .default = "Not-Met"),
    # Buy the Dip model
    Dip = case_when(away_team == Dog & as.numeric(Away_Pred_Rating) >= -0.12 & (Dog_streak == "losses" & Dog_streakN <= 1) ~ "Met",
                    .default = "Not-Met"),
    Petricho = case_when(
      # Dog is the away team
      away_team == Dog &
        (Fav_streak == "losses" | (Fav_streak == "wins" & Fav_streakN <= 1)) &
        (Dog_streak == "wins" | (Dog_streak == "losses" & Dog_streakN <= 3)) &
        as.numeric(Away_Pred_Rank) < 24 &
        as.numeric(Home_Pred_Rank) - as.numeric(Away_Pred_Rating) < 20 ~ "Met-Away",
      
      # Dog is the home team
      home_team == Dog &
        (Dog_streak == "losses" | (Dog_streak == "wins" & Dog_streakN <= 1)) &
        (Fav_streak == "wins" | (Fav_streak == "losses" & Fav_streakN <= 3)) &
        as.numeric(Home_Pred_Rank) < 24 &
        as.numeric(Away_Pred_Rank) - as.numeric(Home_Pred_Rating) < 20 ~ "Met-Home",
      
      .default = "Not-Met"),
    # Criteria for my original dog system
    FLAG_strk = case_when(away_team == Dog & (Dog_streak == "wins" | (Dog_streak == "losses" & Dog_streakN < 3)) &
                            (Fav_streak == "losses" |(Fav_streak == "wins" & Fav_streakN < 3))  ~ "Met",
                          .default = "Not-Met"),
    FLAG_L10 = case_when(away_team == Dog & as.numeric(Dog_L10) >= .400 & as.numeric(Fav_L10) <= .600 ~ "Met",
                         .default = "Not-Met"),
    FLAG_Away = case_when(away_team == Dog & as.numeric(Dog_away_per) >= .400 ~ "Met",
                          .default = "Not-Met"),
    Main = ifelse(FLAG_strk == "Met" & FLAG_L10 == "Met" & FLAG_Away == "Met" & SeriesGame == 1, "Met", "Not-Met"))

# Cleans data so that it can append nicely to googlesheets
picks <- crit1_today %>% 
  rename("HomeTeam" = "home_team",
         "AwayTeam" = "away_team") %>% 
  filter(DT4 == "Met" | Dip == "Met" | Main == "Met" | Petricho == "Met-Away" | Petricho == "Met-Home") %>% 
  select(Date, HomeTeam, AwayTeam, GameinSeries:SeriesGame, Dog_price, DT4, Dip, Main, Petricho)

# Non_filtered, used especially when picks == 0
non_filter <- crit1_today %>% 
  rename("HomeTeam" = "home_team",
         "AwayTeam" = "away_team") %>% 
  select(Date, HomeTeam:SeriesGame, Dog_price,  DT4, Dip, Main, Petricho)

########################
# GoogleSheets
#######################

# Auth both packages with service account
googledrive::drive_auth(path = Sys.getenv("GS_SERVICE_KEY"))
gs4_auth(path = Sys.getenv("GS_SERVICE_KEY"))

# Reads in the variable of the Google Sheet 
atp <- googledrive::drive_get("Underdog System 2026")


# Append Tracking Data
googlesheets4::sheet_append(
  data = tr_merge,
  ss = atp,
  sheet = "TrackingR"
)

# Append Picks Data
googlesheets4::sheet_append(
  data = picks,
  ss = atp,
  sheet = "Picks_R"
)

# Append All road dogs Data
googlesheets4::sheet_append(
  data = non_filter,
  ss = atp,
  sheet = "AllRoadDogs_R"
)

