library(tidyverse)
library(dplyr)
library(tidymodels)
library(bonsai)


# # # # # # # # #
# Load in data  #
# # # # # # # # #
games <- read.csv("projectData/games.csv")
plays <- read.csv("projectData/plays.csv")
tackles <- read.csv("projectData/tackles.csv")
players <- read.csv("projectData/players.csv")

# Import tracking/prediciton results data
  # # Can skip if coming directly from "createMetricModels.R" where we made this dataframe
allWeeks <- read.csv("projectData/allWeeks_withPred.csv") %>% select(-"X") %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(c(is_pass), as.factor))


# # # # # # # # # # # # # # # # # # # # # # # #
# # Getting play/season Over-Expected values  #
# # # # # # # # # # # # # # # # # # # # # # # # 

# Get xTP and TPOE for each individual play
  # # For each unique play/player, get their average eTP across all frames
  # # From their use if they made a tackle - xTP to find their TPOE
TPOE_byPlay <- allWeeks %>%
  group_by(nflId, gameId, playId) %>%
  summarise(made_tackle = mean(tackle), xTP = mean(xTcklPct), TPOE = made_tackle - xTP) %>%
  left_join(subset(plays, select = c(gameId, playId, defensiveTeam), by = c("gameId", "playId"))) %>%
  left_join(subset(players, select = c(nflId, displayName, position)), by = "nflId")

# Find season-long TPOE leaders by player
  # # For all players, find average eTP across all plays to get season eTP
  # # Subtract that from their real season TP to get TPOE
  # # Filter down to all players that played at least 90 defensive snaps (>= 10 per game)
TPOE_leaders <- TPOE_byPlay %>%
  group_by(nflId, defensiveTeam) %>%
  summarise(tackles = sum(made_tackle), plays = n(), TP_season = round((tackles / plays), 3),
            xTP_season = round(mean(xTP),3), TPOE_season = round(mean(TPOE), 3)) %>%
  left_join(subset(players, select = c(nflId, displayName, position))) %>%
  filter(plays > 89) %>%
  arrange(-TPOE_season)

# Find season-long TOE leaders by player
  # # For all players, sum their eTP across all plays to get season expected tackles
  # # Subtract that from their real season tackle total to get TPOE
  # # Filter down to all players that played at least 90 defensive snaps (>= 10 per game)
TOE_leaders <- TPOE_byPlay %>% 
  group_by(nflId, defensiveTeam) %>%
  summarise(tackles = sum(made_tackle), plays = n(), xTack_season = round(sum(xTP),1), TOE_season = tackles - xTack_season) %>%
  left_join(subset(players, select = c(nflId, displayName, position))) %>%
  filter(plays > 89) %>%
  arrange(-TOE_season)

# Get xMTP and MTPOE for each individual play
  # # For each unique play/player, get their average eMTP across all frames
  # # From their use if they missed a tackle - xMTP to find their TPOE
MTPOE_byPlay <- allWeeks %>%
  group_by(nflId, gameId, playId) %>%
  summarise(tackles = mean(tackle), missedTackles = mean(missedTackle),
            xMTP = mean(xMTPct), MTPOE = missedTackles - xMTP) %>% 
  left_join(subset(plays, select = c(gameId, playId, defensiveTeam), by = c("gameId", "playId"))) %>%
  left_join(subset(players, select = c(nflId, displayName, position)), by = "nflId")

# Find season-long TPOE leaders by player
  # # For all players, find average eMTP across all plays to get season eMTP
  # # Subtract that from their real season MTP to get MTPOE
  # # Filter down to all players that played at least 90 defensive snaps (>= 10 per game)
MTPOE_leaders <- MTPOE_byPlay %>%
  group_by(nflId, defensiveTeam) %>%
  summarise(tackles = sum(tackles), missedTackles = sum(missedTackles), plays = n(), MTP_season = round((missedTackles / plays), 3),
            xMTP_season = round(mean(xMTP),3), MTPOE_season = round(mean(MTPOE), 3)) %>%
  left_join(subset(players, select = c(nflId, displayName, position))) %>% 
  filter(plays > 89) %>%
  arrange(-MTPOE_season)

# Find season-long TOE leaders by player
  # # For all players, sum their eMTP across all plays to get season expected missed tackles
  # # Subtract that from their real season missed tackle total to get MTPOE
  # # Filter down to all players that played at least 90 defensive snaps (>= 10 per game)
MTOE_leaders <- MTPOE_byPlay %>% 
  group_by(nflId, defensiveTeam) %>%
  summarise(tackles = sum(tackles), missedTackles = sum(missedTackles), plays = n(), xMT_season = round(sum(xMTP),1), MTOE_season = missedTackles - xMT_season) %>%
  left_join(subset(players, select = c(nflId, displayName, position))) %>%
  filter(plays > 89) %>%
  arrange(-MTOE_season)


# # # # # # # # # # # # 
# Export leaderboards #
# # # # # # # # # # # #

write.csv(TPOE_leaders, "projectData/tpoeSeasonLeaders.csv")
write.csv(TOE_leaders, "projectData/toeSeasonLeaders.csv")
write.csv(MTPOE_leaders, "projectData/mtpoeSeasonLeaders.csv")
write.csv(MTOE_leaders, "projectData/mtoeSeasonLeaders.csv")


# # # # # # # # # # # # # # # # # # #
# Export Metric data for field viz  #
# # # # # # # # # # # # # # # # # # #

# Creating an export of all predicted values by player/frame
  # # Also merging in xTP, xMTP, TPOE, and MTPOE for the entirety of each play on each of the frames for calculation purposes
tpoe_export <- allWeeks %>%
  select(gameId, playId, nflId, frameId, xTP_frame = xTcklPct, xMT_frame = xMTPct) %>%
  left_join(subset(TPOE_byPlay, select = c(gameId, playId, nflId, xTP_play = xTP, TPOE_play = TPOE)), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(subset(MTPOE_byPlay, select = c(gameId, playId, nflId, xMTP_play = xMTP, MTPOE_play = MTPOE)), by = c('gameId', 'playId', 'nflId'))

# Create CSV for dataset created above
write.csv(tpoe_export, "xTP_byFrame.csv")

# Remove dataset for space, if needed
remove(tpoe_export)
