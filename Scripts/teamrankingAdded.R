library(rvest)
library(stringr)
library(dplyr) # for tibble
library(purrr) # for map function

# Creating object with the address
tr_url <- "https://www.teamrankings.com/mlb/rankings/"
tr <- read_html(tr_url)

# Getting links
tr_links <- tr %>% 
  html_nodes("a") %>% 
  html_attr("href")
#head(tr_links,10)


mlb_links <- tr_links[str_detect(tr_links,"mlb/ranking")]
#head(mlb_links)


mlb_links <- mlb_links[!str_detect(mlb_links, "/mlb/rankings/")]

# Puts links in dataframe
df <- tibble(stat_links = mlb_links)

# Function to get page
get_page <- function(url){
  page <- read_html(url)
  Sys.sleep(sample(seq(.25,2.5,.25),1))
  page
}

# Function to pull data from website
teamrank <- function(date){
  # Set up some variables/list
  d <- paste0("?date=", date)
  output <- list()
  # All the statistics I want to look at
  s1 <- "predictive-by-other"
  s2 <- "home-by-other"
  s3 <- "away-by-other"
  s4 <- "last-10-games-by-other"
  s5 <- "consistency-by-other"
  s6 <- "vs-1-5-by-other"
  s7 <- "vs-6-10-by-other"
  
  # Find the stat
  df1 <- df %>% 
    mutate(s1a = str_detect(stat_links, s1),
           s2a = str_detect(stat_links, s2),
           s3a = str_detect(stat_links, s3),
           s4a = str_detect(stat_links, s4), 
           s5a = str_detect(stat_links, s5), 
           s6a = str_detect(stat_links, s6), 
           s7a = str_detect(stat_links, s7)) %>% 
    filter(s1a == TRUE | s2a == TRUE | s3a == TRUE | s4a == TRUE | s5a == TRUE | s6a == TRUE | s7a == TRUE) %>% 
    mutate(url = paste0('https://www.teamrankings.com', stat_links, d))
  
  # Looping over each statistic type
  
  for(row in 1:nrow(df1)){
    page_data <- map(df1$url[row], get_page)
    tr_data <- map(page_data, html_table)
    
    tr_sub <- tr_data[[1]][1]
    
    output[[row]] <- tr_sub
  }
  
  
  # Return the list
  return(output)
  
}

# Get data
dat <- teamrank(Sys.Date())

# Split data into DFs
pred <- as.data.frame(dat[[1]])
hom <- as.data.frame(dat[[3]])
awy <- as.data.frame(dat[[4]])
l10 <- as.data.frame(dat[[5]])
consist <- as.data.frame(dat[[6]])
v1_5 <- as.data.frame(dat[[7]])
v6_10 <- as.data.frame(dat[[8]])

# Clean data
pred <- pred %>% 
  mutate(Team = sub("\\s+[^ ]+$", "", Team)) %>% 
  rename("Pred_Rating" = "Rating") %>% 
  select(Team, Pred_Rating)

hom <- hom %>% 
  mutate(Team = sub("\\s+[^ ]+$", "", Team)) %>% 
  rename("Home_Rating" = "Rating") %>% 
  select(Team, Home_Rating)

awy <- awy %>% 
  mutate(Team = sub("\\s+[^ ]+$", "", Team)) %>% 
  rename("Away_Rating" = "Rating") %>% 
  select(Team, Away_Rating)

l10 <- l10 %>% 
  mutate(Team = sub("\\s+[^ ]+$", "", Team)) %>% 
  rename("L10_Rating" = "Rating") %>% 
  select(Team, L10_Rating)

consist <- consist %>% 
  mutate(Team = sub("\\s+[^ ]+$", "", Team)) %>% 
  rename("Consist_Rating" = "Rating") %>% 
  select(Team, Consist_Rating)

v1_5 <- v1_5 %>% 
  mutate(Team = sub("\\s+[^ ]+$", "", Team)) %>% 
  rename("V1_5_Rating" = "Rating") %>% 
  select(Team, V1_5_Rating)

v6_10 <- v6_10 %>% 
  mutate(Team = sub("\\s+[^ ]+$", "", Team)) %>% 
  rename("V6_10_Rating" = "Rating") %>% 
  select(Team, V6_10_Rating)


# Change format of team spelling

# Created a table associating each team name in TR with the team name from baseballr to substitute for it:
team_lkup <- c(Cleveland="Cleveland Guardians", Washington="Washington Nationals", Miami="Miami Marlins", Atlanta="Atlanta Braves", `Chi Cubs` ="Chicago Cubs", `St. Louis` ="St. Louis Cardinals",
               Pittsburgh = "Pittsburgh Pirates", Cincinnati = 'Cincinnati Reds', Houston = 'Houston Astros', Milwaukee ='Milwaukee Brewers', `San Diego` = 'San Diego Padres', Arizona = 'Arizona Diamondbacks',
               Colorado = 'Colorado Rockies', `LA Dodgers` = 'Los Angeles Dodgers', `NY Mets` = 'New York Mets', Philadelphia = 'Philadelphia Phillies', `Kansas City` = 'Kansas City Royals', `Chi Sox` = 'Chicago White Sox',
               Minnesota = 'Minnesota Twins', Detroit = 'Detroit Tigers', Sacramento = 'Athletics', Texas = 'Texas Rangers', `LA Angels` = 'Los Angeles Angels', Seattle = 'Seattle Mariners', 
               Baltimore = 'Baltimore Orioles', Boston = 'Boston Red Sox', `NY Yankees` = 'New York Yankees', Toronto = 'Toronto Blue Jays', `Tampa Bay` = 'Tampa Bay Rays', `SF Giants` = 'San Francisco Giants')



pred$Team <- as.character(team_lkup[pred$Team])
hom$Team <- as.character(team_lkup[hom$Team])
awy$Team <- as.character(team_lkup[awy$Team])
l10$Team <- as.character(team_lkup[l10$Team])
consist$Team <- as.character(team_lkup[consist$Team])
v1_5$Team <- as.character(team_lkup[v1_5$Team])
v6_10$Team <- as.character(team_lkup[v6_10$Team])


rm(dat, df, tr, mlb_links, tr_links,tr_url, get_page, teamrank, team_lkup)




























