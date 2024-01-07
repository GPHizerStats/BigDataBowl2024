library(tidyverse)
library(dplyr)
library(tidymodels)
library(bonsai)


# # # # # # # # # # #
# Load in base data #
# # # # # # # # # # #

games <- read.csv("projectData/games.csv")
plays <- read.csv("projectData/plays.csv")
tackles <- read.csv("projectData/tackles.csv")
players <- read.csv("projectData/players.csv")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Creating base dataframes to merge onto tracking data for model development  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Get Stats for tackles against, forced missed, tackle attempts against, and rates for each ball carrier
  # # There was one instance of two tackles being made on a play during a 3rd and 5 towards the end of the 2nd quarter of the Ravens/Jets week 1 matchup
  # # Due to this being a one time instance, I removed the second tackle made on the play for the ball carrier stats since the play has Tyler Conklin as the ball carrier, but Garrett Wilson recovered a fumble and was tackled second
plays_cumulativeTackles <- tackles [-303,] %>%
  group_by(gameId, playId) %>%
  summarize(tack_atts = sum(tackle) + sum(pff_missedTackle), tacks = sum(tackle),
            tack_miss = sum(pff_missedTackle))

# Create a dataframe of just tackle/missed tackle stats forball carriers
ballCarrier_pbpTackleStats <- subset(plays, select = c(gameId, playId, ballCarrierId, ballCarrierDisplayName)) %>%
  left_join(subset(players, select = c(nflId, position)), by = c("ballCarrierId" = "nflId")) %>%
  left_join(plays_cumulativeTackles, by = c("gameId", "playId"))

# Replace N/A values for plays when ball carrier went untouched and replace with zeroes
ballCarrier_pbpTackleStats$tack_atts <- ballCarrier_pbpTackleStats$tack_atts %>% replace_na(0)
ballCarrier_pbpTackleStats$tacks <- ballCarrier_pbpTackleStats$tacks %>% replace_na(0)
ballCarrier_pbpTackleStats$tack_miss <- ballCarrier_pbpTackleStats$tack_miss %>% replace_na(0)

# Merge and filter ball carrier stats so we can get their avg. missed tackle rate for the model
ballCarrier_tackleStats <- ballCarrier_pbpTackleStats %>%
  group_by(ballCarrierId, ballCarrierDisplayName, position) %>%
  summarize(touches = length(ballCarrierId), tack_atts_against = sum(tack_atts), tacks_against = sum(tacks),
            tack_miss_forced = sum(tack_miss), tack_pct_against = round((tacks_against / tack_atts_against), 3), 
            miss_pct_forced = round((tack_miss_forced / tack_atts_against), 3), miss_forced_perRush = round((tack_miss_forced / touches), 3)) %>%
  filter(tack_atts_against != 0)


  # # Finally we are adding an additional missed tackle % column to use for training the model

# Get league average for each position
tack_pct_QB <- round(sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "QB"), "tacks_against"]) / 
                       sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "QB"), "tack_atts_against"]), 3)
tack_pct_RB <- round(sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "RB"), "tacks_against"]) / 
                       sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "RB"), "tack_atts_against"]), 3)
tack_pct_WR <- round(sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "WR"), "tacks_against"]) / 
                       sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "WR"), "tack_atts_against"]), 3)
tack_pct_TE <- round(sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "TE"), "tacks_against"]) / 
                       sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "TE"), "tack_atts_against"]), 3)
tack_pct_FB <- round(sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "FB"), "tacks_against"]) / 
                       sum(ballCarrier_tackleStats[which(ballCarrier_tackleStats$position == "FB"), "tack_atts_against"]), 3)

  # Set base tackle % to 0
ballCarrier_tackleStats$ballCarrier_tackPctAgainst <- 0

# Check if they have over 22.5 touches yet (2.5 per game)
  # # If yes then their missed tackle % for the model is their own
  # # If no, then their msised tackle % for model is league average at their position
ballCarrier_tackleStats <- ballCarrier_tackleStats %>% 
  mutate(ballCarrier_tackPctAgainst = if_else(tack_atts_against > 22.5, tack_pct_against,
                                             ifelse(position == "QB", tack_pct_QB,
                                                    ifelse(position == "RB", tack_pct_RB,
                                                           ifelse(position == "WR", tack_pct_WR,
                                                                  ifelse(position == "TE", tack_pct_TE,
                                                                         ifelse(position == "FB", tack_pct_FB,
                                                                         tack_pct_against_leagueAvg)))))))


# # # # # # # # # # # # # # # # # # # # # # # #
# Load in/edit tracking data for use in model #
# # # # # # # # # # # # # # # # # # # # # # # #

# # Function used in for loop to get distance of defender from ball carrier
getDist <- function(x1, y1, x2, y2) {
  p = (x1-x2)^2 + (y1-y2)^2
  return(sqrt(p))
}

# Use for loop to iterate through all tracking data in set
for (i in 1:9) {
  week <- read.csv(paste("projectData/tracking_week_",as.character(i),".csv",sep=""))

  # # Filter out all players that arent either the ball carrier or on defense
  week <- week %>%
    left_join(subset(plays, select = c(gameId, playId, ballCarrierId, defensiveTeam, passResult)), by = c("gameId", "playId")) %>%
    filter(club==defensiveTeam | nflId == ballCarrierId)

  # Merge tackle/missed tackle data to the dataset
  week <- week %>% 
    left_join(subset(tackles, select = c(gameId, playId, nflId, tackle, assist, pff_missedTackle)), by = c("gameId", "playId", "nflId"))

  # Drop all NA's and merge solo + assisted tackles into 1 tackle column
  week$tackle <- week$tackle %>% replace_na(0)
  week$assist <- week$assist %>% replace_na(0)
  week$pff_missedTackle <- week$pff_missedTackle %>% replace_na(0)
  week$tackle <- week$tackle + week$assist
  week <- week %>% select(-assist)

  # Merge ball carrier's tackle rate against to the data
  week <- week %>%
    left_join(subset(ballCarrier_tackleStats, select = c(ballCarrierId, ballCarrier_tackPctAgainst), by = "ballCarrierId"))

  # Merge ball carrier location on field to the dataset
    # # Rename location coordinates to include "_def", while the ballcarrier includes "_bc"
  week <- week %>%
    left_join(subset(week, select = c(gameId, playId, nflId, frameId, x, y, s, dir)),
              by = c("gameId" = "gameId", "playId" = "playId", "ballCarrierId" = "nflId", "frameId" = "frameId")) %>%
    rename("x_def" = "x.x", "y_def" = "y.x", "s_def" =  "s.x", "dir_def" =  "dir.x",
           "x_ballCar" = "x.y", "y_ballCar" = "y.y", "s_ballCar" =  "s.y", "dir_ballCar" =  "dir.y") %>% 
    filter(nflId != ballCarrierId)
  
  # Create binary flag variable to indicate whether it is a run or pass based on "pass result"
  week <- week %>%
    mutate(passResult = ifelse(passResult == "C", 1,
                               ifelse(passResult == "R", 1, 0))) %>%
    rename("is_pass" = "passResult", "missedTackle" = "pff_missedTackle")

  # Get the distance between defenders at each frame and ball carrier using their location coordinates and the function created at top
  x1 <- week$x_def
  y1 <- week$y_def
  x2 <- week$x_ballCar
  y2 <- week$y_ballCar
  dist_ballCar <- vector("numeric", length(x1))
  for (j in 1:length(x1)) {
    dist_ballCar[j] <- getDist(x1[j], y1[j], x2[j], y2[j])
  }

  # Drop all columns that will not be used in model creation and change necessary variables to factors
  week <- select(week, -c(displayName, time, jerseyNumber, club, playDirection, a, dis, o, event, defensiveTeam)) %>%
    mutate(is_pass = as.factor(is_pass),
           tackle = as.factor(tackle), 
           missedTackle = as.factor(missedTackle))

  # If it is week 1, create a new df to store all the data, otherwise append it to the bottom of pre-existing df
  if(i == 1) {
    print(paste("Merging Week", as.character(i),sep=""))
    allWeeks <- week
  }
  else {
    print(paste("Merging Week", as.character(i),sep=""))
    rbind(allWeeks, week)
  }
  
}

# Remove final week's tracking data
remove(week)

# Create backup file of the trimmed data
write.csv(allWeeks, "projectData/allWeeks.csv")


# # # # # # # # # # # # # # # # # # # #
# Model Creation - Expected Tackle %  #
# # # # # # # # # # # # # # # # # # # #

# Bring data back in from merged dataset (if necessary)
allWeeks <- read.csv("projectData/allWeeks.csv") %>% select(-"X") %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(c(is_pass, tackle, missedTackle), as.factor))

# Remove all identifying columns from dataset (game/play/nfl ID's), as well as missed tackle variable
allWeeks_TackleModelData <- allWeeks %>% 
  select(label = tackle, x_def, y_def, s_def, dir_def, x_ballCar, y_ballCar, s_ballCar,
         dir_ballCar, dist_ballCar, ballCarrier_tackPctAgainst, is_pass) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(c(is_pass, label), as.factor))

# Rename the tackle var to "label" to fit with model
allWeeks <- allWeeks %>% rename("label" = "tackle") %>% mutate(across(c(label), as.factor))


# Create train/testing splits, as well as folds for cross-validation
set.seed(1355)  # aka the amount of yards Rashee Rice had after leading the nation in receiving yards (reg season) as a Senior
tackleMod_split <- initial_split(allWeeks_TackleModelData)
tackleMod_train <- training(tackleMod_split)
tackleMod_test <- testing(tackleMod_split)
tackleMod_folds <- vfold_cv(tackleMod_train)

# Create Recipe to be used for model
tackleMod_recipe <- recipe(formula = label ~ ., data = tackleMod_train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Create specs for basic Gradiant Boosting model using the lightgbm engine
tackleMod_specs <- boost_tree() %>%
  set_engine("lightgbm") %>%
  set_mode("classification") %>%
  translate()

# Create the model workflow
tackleMod_workflow <- workflow() %>%
  add_recipe(tackleMod_recipe) %>%
  add_model(tackleMod_specs)

# Fit the tackle model
tackleModel_final <- tackleMod_workflow %>%
  fit(data=tackleMod_test)

# Get prediction results from model using original "allWeeks" set
  # # Return a probability of tackle
pred_xTP <- predict(tackleModel_final, allWeeks, type = "prob")

# Remove these once finished to keep environment size down, however keep the same split and seed in case we need to get them back
remove(allWeeks_TackleModelData, tackleMod_train, tackleMod_test, tackleMod_workflow, tackleMod_split, tackleMod_folds, tackleMod_grid, tackleMod_recipe)

# Change name of tackle column back
allWeeks <- allWeeks %>% rename("tackle" = "label")


# # # # # # # # # # # # # # # # # # # # # # #
# Model Creation - Expected Missed Tackle % #
# # # # # # # # # # # # # # # # # # # # # # #

# Bring data back in from merged dataset
allWeeks <- read.csv("projectData/allWeeks.csv") %>% select(-"X") %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(c(is_pass, tackle, missedTackle), as.factor))

# Remove all identifying columns from dataset (game/play/nfl ID's), as well as tackle variable
allWeeks_missedModelData <- allWeeks %>% 
  select(label = missedTackle, x_def, y_def, s_def, dir_def, x_ballCar, y_ballCar, s_ballCar,
         dir_ballCar, dist_ballCar, ballCarrier_tackPctAgainst, is_pass) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(c(is_pass, label), as.factor))


# Rename the missed tackle var to "label" to fit with model
allWeeks <- allWeeks %>% rename("label" = "missedTackle")

# Create train/testing splits, as well as folds for cross-validation
set.seed(1355)  # aka the amount of yards Rashee Rice had after leading the nation in receiving yards (reg season) as a Senior
missedMod_split <- initial_split(allWeeks_missedModelData)
missedMod_train <- training(missedMod_split)
missedMod_test <- testing(missedMod_split)
missedMod_folds <- vfold_cv(missedMod_train)

# Create Recipe to be used for model
missedMod_recipe <- recipe(formula = label ~ ., data = missedMod_train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Create specs for basic Gradiant Boosting model using the lightgbm engine
missedMod_specs <- boost_tree() %>%
  set_engine("lightgbm") %>%
  set_mode("classification") %>%
  translate()

# Create the model workflow
missedMod_workflow <- workflow() %>%
  add_recipe(missedMod_recipe) %>%
  add_model(missedMod_specs)


# Fit the tackle model
missedModel_final <- missedMod_workflow %>%
  fit(data=missedMod_test)

  # Remove these once finished to keep environment size down, however keep the same split and seed in case we need to get them back
remove(allWeeks_missedModelData, missedMod_train, missedMod_test)

# Get prediction results from model using original "allWeeks" set
  # Return a probability of missed tackle
missed_pred <- predict(missedModel_final, allWeeks, type = "prob")

# Change name of tackle column back
allWeeks <- allWeeks %>% rename("missedTackle" = "label")


# # # # # # # # # # # # 
# Back up model data  #
# # # # # # # # # # # # 

# Only keep prediction of tackle being made (pred = 1) and rename appropriately
predVals <- pred_xTP %>%
  rename(xTcklPct = .pred_1) %>%
  select(-.pred_0)

# Only keep prediction of missed tackle being made (pred = 1) and rename appropriately
  # Being merged with predicted tackle %
predVals <- cbind(predVals, missed_pred) %>%
  rename(xMTPct = .pred_1) %>%
  select(-.pred_0)

# Bind predicted values to their associated plays and save
allWeeks <- cbind(allWeeks, predVals)
write.csv(allWeeks, "allWeeks_withPred.csv")

# Remove prediciton values to save space (once saved)
remove(pred_xTP, missed_pred, predVals)

    # # Note: at this point it is also okay to delete the original "allWeeks.csv" dataset if needed

# # #
# # Getting play/season Over-Expected values

TPOE_byPlay <- allWeeks %>%
  mutate_at("tackle", as.numeric) %>%
  group_by(nflId, gameId, playId) %>%
  summarise(made_tackle = mean(tackle-1), xTP = mean(xTcklPct), TPOE = made_tackle - xTP)


TPOE_leaders <- TPOE_byPlay %>%
  group_by(nflId) %>%
  summarise(tackles = sum(made_tackle), xTP_season = round(mean(xTP),3), TPOE_season = round(mean(TPOE), 3)) %>%
  left_join(subset(players, select = c(nflId, displayName, position))) %>%
  filter(tackles > 9) %>%
  arrange(-TPOE_season)

TOE_leaders <- TPOE_byPlay %>% 
  group_by(nflId) %>%
  summarise(tackles = sum(made_tackle), xTack_season = round(sum(xTP),1), TOE_season = tackles - xTack_season) %>%
  left_join(subset(players, select = c(nflId, displayName, position))) %>%
  filter(tackles > 9) %>%
  arrange(-TOE_season)

{
tpoe_export <- allWeeks %>%
  select(gameId, playId, nflId, frameId, madeTackle = tackle, xTP_frame = xTcklPct)

tpoe_export2 <- TPOE_byPlay %>%
  select(gameId, playId, nflId, xTP_play = xTP, TPOE_play = TPOE)

tpoe_export <- tpoe_export %>%
  left_join(tpoe_export2, by = c("gameId", "playId", "nflId"))
write.csv(tpoe_export, "tpoeExport_byFrame.csv")
}

tpoe_export <- allWeeks %>%
  select(gameId, playId, nflId, frameId, xTP_frame = xTcklPct, xMT_frame = xMTPct) %>%
  left_join(subset(TPOE_byPlay, select = c(gameId, playId, nflId, xTP_play = xTP, TPOE_play = TPOE)), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(subset(MTPOE_byPlay, select = c(gameId, playId, nflId, xMTP_play = xMTP, MTPOE_play = MTPOE)), by = c('gameId', 'playId', 'nflId'))


write.csv(tpoe_export, "xTP_byFrame.csv")

remove(missed_pred, missedMod_folds, missedMod_grid, missedMod_recipe, missedMod_specs, missedMod_split, missedMod_workflow, missedModel_final, pred_missed)