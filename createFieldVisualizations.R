library(tidyverse)
library(ggplot2)
library(dplyr)
options(warn=-1)


# # # # # # # # # # # # # #
# Load in necessary data  #
# # # # # # # # # # # # # #

# Load in provided tracking data
track_df <- read.csv("projectData/tracking_week_1.csv")
for (i in 2:9) {
  track_df <- rbind(track_df, read.csv(paste("projectData/tracking_week_",as.character(i),".csv",sep="")))
}

# Load in Grant's csv file with the metrics
  # # Can skip if coming directly from "createMetricLeaderboards.R" where we made this dataframe
xTrack <- read.csv("projectData/xTP_byFrame.csv") |>
  select(-X)

# Join the tracking data with the metric data
track_df <- track_df |>
  left_join(xTrack, by=c("gameId","playId","nflId","frameId"))

# Remove from environment to save space
rm(xTrack)

# Load in the tackle data and play data
tackle_df <- read.csv("projectData/tackles.csv")
play_data <- read.csv("projectData/plays.csv")


# # # # # # # # # # #
# Function Creation #
# # # # # # # # # # #

# # This function indicates when a missed tackle occurred
get_missed_tackles <- function(track, tackle, play) {
  
  # Get ballcarrier info for every play
  new_df <- play |>
    select(gameId, playId, ballCarrierId) |>
    right_join(track, by=c("gameId","playId")) |>
    mutate(isBallCarrier = ifelse(ballCarrierId==nflId, 1, 0))
  
  # Filter everything out except for the ball carrier and certain columns
  ball_Carrier <- new_df |>
    filter(isBallCarrier==1) |>
    select(gameId, playId, frameId, displayName, x, y)
  
  # Combine our tracking data with ball carrier data
  ball_carrier_track <- track |>
    inner_join(tackle, by=c("gameId","playId","nflId")) |>
    inner_join(ball_Carrier, by=c("gameId", "playId", "frameId")) |>
    dplyr::rename(ballCarrierName = displayName.y, x.ballCarrier=x.y, y.ballCarrier=y.y, x=x.x, y=y.x) 
  
  # Calculate the euclidean distance to get how far a defender is from ballcarrier
  missed_tackles <- ball_carrier_track |>
    filter(pff_missedTackle==1) |>
    mutate(e_distance = sqrt( (x.ballCarrier - x)**2 + (y.ballCarrier - y)**2 ))
  
  # From here we assume the closest point from the defender to ballcarrier that 
  # would not be considered a tackle is when the missed tackle occurred
  location_missed_tackle <- missed_tackles |>
    filter(event != "tackle", event != "out_of_bounds", event != "fumble", event != "safety", event != "qb_slide") |>
    group_by(gameId, playId, nflId) |>
    filter(e_distance == min(e_distance)) |>
    mutate(is_missedTackle = 1) |>
    select(gameId, playId, nflId, frameId, x, y, is_missedTackle)
  
  # Join everything to the original tracking dataframe and return
  final_track_df <- new_df |>
    left_join(location_missed_tackle, by=c("gameId","playId", "nflId", "frameId", "x","y")) |>
    mutate(is_missedTackle = ifelse(is.na(is_missedTackle), 0, is_missedTackle),
           event = ifelse(is_missedTackle == 1, "missed tackle", event))
  
  return(final_track_df)
}


# Get miss tackles
track_df <- get_missed_tackles(track_df, tackle_df, play_data)

# # Function to get when the exact tackle occurred
get_tackle_occurrence <- function(track, tackle) {
  
  # All the possible events where a tackle would be considered
  events <- c("tackle","out_of_bounds", "fumble", "safety", "qb_slide")
  
  # Join tackle data with track data and filter out to only tackle events
  new_df <- track |>
    left_join(tackle_df, by=c("gameId"="gameId", "playId"="playId","nflId"="nflId")) |>
    filter(tackle==1 | assist==1, event %in% events) |>
    select(gameId, playId, nflId, frameId, tackle, assist) |>
    select(-tackle, -assist)
  
  # Filter out any occurrence where the code above counted a player for two tackles in one play
  # Once filtered out we create a new column indicating this is the frame the tackle occurred on this play
  new_df <- new_df[!duplicated(new_df[c('gameId','playId', 'nflId')]), ]
  new_df$is_tackle <- 1
  
  # Join to tracking data and return
  new_track <- track |>
    left_join(new_df, by=c("gameId","playId","nflId","frameId"))
  
  return(new_track)
}

# Get tackle occurrence
track_df <- get_tackle_occurrence(track_df, tackle_df)


# # # # # # # # # # # # # # # # # # # # # # # #
# Creating the field visualization functions  #
# # # # # # # # # # # # # # # # # # # # # # # #

# This function creates the visualization
player_tackle_distribution <- function(track, play_info, team, measure, metric) {
  
  # We combine the play data with the tracking data mainly to get where the LOS is
  # From there we normalize the field so its frame is facing one direction
  # Then given where the player is and how far from LOS he is, we give him a field location for that frame
  play <-  track |>
    mutate(x = ifelse(playDirection=="left", 120-x, x),
           y = ifelse(playDirection=="left", 53.3-y, y)) |>
    left_join(play_info, by=c("gameId","playId")) |>
    mutate(absoluteYardlineNumber = ifelse(playDirection=="left", 120-absoluteYardlineNumber, absoluteYardlineNumber),
           x_bal = x - absoluteYardlineNumber) |>
    mutate(tackle_depth = case_when(x_bal < 0 ~ "TFL",
                                    x_bal < 5 ~ "0 to 4",
                                    x_bal < 10 ~ "5 to 9",
                                    x_bal >= 10 ~ "10+",
                                    .default = NA)) |>
    mutate(tackle_width = case_when(y <= 11.75 ~ "Right Sideline",
                                    y <= 23.55 ~ "Right Middle",
                                    y <= 29.75 ~ "Hashes",
                                    y <= 41.525 ~ "Left Middle",
                                    y <= 60 ~ "Left Sideline",
                                    .default = NA)) |>
    mutate(y_depth = case_when(x_bal < 0 ~ 55,
                               x_bal < 5 ~ 62.5,
                               x_bal < 10 ~ 67.5,
                               x_bal >= 10 ~ 77.5,
                               .default = NA)) |> 
    mutate(x_width = case_when(y <= 11.75 ~ 53.3 - 5.875,
                               y <= 23.55 ~ 53.3 - 17.65,
                               y <= 29.75 ~ 53.3 - 26.65,
                               y <= 41.525 ~ 53.3 - 35.6375,
                               y <= 60 ~ 53.3 - 47.4125,
                               .default = NA)) |>
    filter(!is.na(tackle_width))
  
  # This is where we calculate the metric for a player in a given field location
  # First we goup by play, player, tackle location to get our averages per play/tackle location
  # From there we just group by play and tackle location to get our metric values for a player in a given tackle location
  metrics <- play |>
    mutate(is_tackle = ifelse(is.na(is_tackle), 0, is_tackle)) |>
    group_by(gameId, playId, nflId, displayName, tackle_width, tackle_depth) |>
    summarise(made_tackle = sum(is_tackle), XTP=mean(xTP), TPOE = made_tackle - XTP, missedTackles = sum(is_missedTackle), XMTP=mean(xMTP), 
              MTPOE=missedTackles-XMTP, x = last(x_width), y=last(y_depth), club=last(club)) |>
    group_by(nflId, displayName, tackle_width, tackle_depth) |>
    summarise(tackles = sum(made_tackle), plays=n(), total_attempts = sum(made_tackle) + sum(missedTackles), 
              TPOE_season = round(mean(TPOE),3), TOE_season = tackles-round(sum(XTP),3), missed_tackles = sum(missedTackles),
              MTPOE_season = round(mean(MTPOE),3), MTOE_season = missed_tackles-round(sum(XMTP),3),
              x = last(x), y=last(y), club=last(club)) |>
    mutate(over_att = ifelse(plays >=15 & !is.na(TOE_season), 1, 0))
  
  # To make it dynamic, given the metric input we will subset our dataset to contain that metric and create variables for that specific metric
  if (metric=="TPOE_season") {
    metrics <- metrics |>
      select(nflId, displayName, tackle_width, tackle_depth, TPOE_season, over_att, x, y, club) |>
      mutate(value=TPOE_season)
    abbreviation <- "TPOE"
    big_title <- "Tackle Probability Over Expected (TPOE)"
  } else if (metric=="TOE_season") {
    metrics <- metrics |>
      select(nflId, displayName, tackle_width, tackle_depth, TOE_season, over_att,x, y, club)|>
      mutate(value=TOE_season)
    abbreviation <- "TOE"
    big_title <- "Tackle Over Expected (TOE)"
  } else if (metric=="MTPOE_season") {
    metrics <- metrics |>
      select(nflId, displayName, tackle_width, tackle_depth, MTPOE_season, over_att,x, y, club)|>
      mutate(value=MTPOE_season)
    abbreviation <- "MTPOE"
    big_title <- "Missed Tackle Probability Over Expected (MTPOE)"
  } else if (metric=="MTOE_season") {
    metrics <- metrics |>
      select(nflId, displayName, tackle_width, tackle_depth, MTOE_season, over_att,x, y, club)|>
      mutate(value=MTOE_season)
    abbreviation <- "MTOE"
    big_title <- "Missed Tackle Over Expected (MTOE)"
  } else {
    return(NA)
  }
  
  fun_play <- metrics
  
  # Here we get the max and min of the metric in given tackle location, then get the percentile
  fun_play$max_value <- ave(fun_play$value, fun_play$tackle_depth, fun_play$tackle_width, fun_play$over_att, FUN=max)
  fun_play$min_value <- ave(fun_play$value, fun_play$tackle_depth, fun_play$tackle_width, fun_play$over_att, FUN=min)
  
  fun_play$percentile <- round(((fun_play$value - fun_play$min_value) / (fun_play$max_value - fun_play$min_value)) * 100,0)
  fun_play$percentile <- ifelse(fun_play$percentile == "NaN", 0, fun_play$percentile)
  
  # If we want a specific team, then we subset it and find the best/worst tacklers given a tackle location for that team
  if (team != "NFL") {
    fun_play <- subset(fun_play, club == team)
    fun_play$max_value <- ave(fun_play$value, fun_play$tackle_depth, fun_play$tackle_width, fun_play$over_att, FUN=max)
    fun_play$min_value <- ave(fun_play$value, fun_play$tackle_depth, fun_play$tackle_width, fun_play$over_att, FUN=min)
    
  } 
  
  # Depending on if we want the max (best) or min (worst) players for that given metric
  if (measure == "max") {
    fun_play <- subset(fun_play, value == max_value & over_att == 1)
  } else {
    fun_play <- subset(fun_play, value == min_value & over_att == 1)
  }
  
  # This is our final dataframe where we group by field location and gather all the best or worst players for that metric
  fun_play <- fun_play |>
    group_by(tackle_depth, tackle_width) |>
    dplyr::summarise(name = paste(displayName, collapse="\n"),
                     value = last(value),
                     percentile = ifelse(metric=="MTOE_season" | metric=="MTPOE_season", 100-last(percentile), last(percentile)),
                     x_width = last(x),
                     y_depth = last(y)) |>
    mutate(new_text = paste(name, "\n",abbreviation,": ", as.character(value),"\n", "NFL Percentile: ", as.character(percentile), sep=""))
  
  
  # remove duplicates
  fun_play <- fun_play[!duplicated(fun_play[c('tackle_width','tackle_depth','name')]), ]
  
  # These two lines are used for visualization portion
  labels <- function(x) {
    x-60
  }
  vlines <- c(11.75, 23.55, 29.75, 41.525)
  
  # We use premade code for this field
  # From there I edit in a way that we need for our visualization and add the data from final dataframe
  source("gg_field.R")
  plot <- ggplot(subset(fun_play)) +
    gg_field(direction="vert", yardmin=50, yardmax=85, buffer=1) +
    geom_hline(yintercept=vlines,linetype=5, alpha=1, color="black", linewidth=1) +
    geom_segment(aes(x = 60, y = 0, xend=60, yend=53.3), color="blue", linewidth=2) +
    geom_segment(aes(x = 65, y = 0, xend=65, yend=53.3), color="black", linewidth=1, linetype=5) +
    geom_segment(aes(x = 70, y = 0, xend=70, yend=53.3), color="black", linewidth=1, linetype=5) +
    geom_text(aes(y_depth, x_width, label=new_text)) +
    scale_x_continuous(label = labels) +
    labs(title = paste(team, big_title, "Map", sep=" "),
         subtitle = "Min. 15 snaps played in that zone") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # return visualization
  return(plot)
}


# # # # # # # # # # # # # # # # # # #
# Create field viz's from function  #
# # # # # # # # # # # # # # # # # # #

player_tackle_distribution(track_df, play_data, "HOU", "min", "TOE_season") # HOU worst players by TOE by field section

player_tackle_distribution(track_df, play_data, "HOU", "min", "TPOE_season") # HOU worst players by TPOE by field section

player_tackle_distribution(track_df, play_data, "HOU", "max", "MTOE_season") # HOU worst players by MTOE by field section

player_tackle_distribution(track_df, play_data, "HOU", "max", "MTPOE_season") # HOU worst players by MTPOE by field section

player_tackle_distribution(track_df, play_data, "HOU", "max", "TOE_season") # HOU best players by TOE by field section

player_tackle_distribution(track_df, play_data, "HOU", "max", "TPOE_season") # HOU best players by TPOE by field section

player_tackle_distribution(track_df, play_data, "HOU", "min", "MTOE_season") # HOU best players by MTOE by field section

player_tackle_distribution(track_df, play_data, "HOU", "min", "MTPOE_season") # HOU best players by MTPOE by field section

