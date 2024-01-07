library(tidyverse)
library(dplyr)
library(tidymodels)
library(bonsai)
library(nflfastR)
library(nflplotR)
library(ggplot2)
library(gt)
library(gtExtras)
library(fmsb)


# # # # # # # # # # # # # #
# Load in necessary data  #
# # # # # # # # # # # # # #

# Import basic data
games <- read.csv("projectData/games.csv")
plays <- read.csv("projectData/plays.csv")
tackles <- read.csv("projectData/tackles.csv")
players <- read.csv("projectData/players.csv")

# Bring in metric leader data created in google sheets / TPOE leaderboard created in "createMetricLeaderboards.r"
metric_rankings <- read.csv("projectData/metric_rankings.csv")
TPOE_leaders <- read.csv("projectData/tpoeSeasonLeaders.csv") %>% select(-"X")

# Import tracking/prediction results data
  # # Can skip if coming directly from "createMetricModels.R" where we made this dataframe
allWeeks <- read.csv("projectData/allWeeks_withPred.csv") %>% select(-"X") %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(c(is_pass), as.factor))

# Bring in URL data from nflreadR for Team Logo and Player Headshots
teams <- nflreadr::load_teams(current = TRUE) %>%
  select(team_abbr, team_logo_wikipedia)
player_headshots <- nflreadr::load_players() %>%
  select(gsis_it_id, headshot)


# # # # # # # # # # # # # # # # # # # # # #
# Create tables necessary for submission  #
# # # # # # # # # # # # # # # # # # # # # #

# # Create Houston team chart
  # # Separate only HOU players from metric rankings and merge player headshots and plays
ranks_HOU <- metric_rankings %>%
  filter(team == "HOU") %>%
  select(-c(TPOE, TOE, MTPOE, MTOE, Average.Tackle.Rank, Average.Missed.Tackle.Rank, Average.Rank)) %>%
  left_join(player_headshots, by = c("nflId" = "gsis_it_id")) %>%
  left_join(subset(TPOE_leaders, select = c("nflId", "plays")), by = c("nflId" = "nflId"))

  # # Create GT object table for all qualified Texans defenders
  # # To just print out table instead of save, delete "HOU_table <- "
HOU_table <- ranks_HOU %>%
  gt(rowname_col = "displayName") %>%
  cols_hide(c(nflId, team)) %>% 
  tab_header(
    title = md("<img src= 'https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/100px-Houston_Texans_logo.svg.png' style='height:30px;'>     
             Houston Defender Leaguewide Tackle Rankings"),
    subtitle = md("*2022 NFL Season, Weeks 1-9  |  Min. 90 snaps ~ 538 Qualified Players*")
  ) %>%
  tab_stubhead(label = "Player") %>%
  gt_img_rows(headshot, height = 25) %>%
  cols_label(
    displayName = "Player",
    headshot = "",
    pos = "Pos.",
    plays = "Snaps",
    TPOE_rank = "Lg. Rank: TPOE",
    TOE_rank = "Lg. Rank: TOE",
    MTPOE_rank = "Lg. Rank: MTPOE",
    MTOE_rank = "Lg. Rank: MTOE",
    Overall.Tackle.Rank = "Composite Tackle Rank", 
    Percentile = "League Percentile"
  ) %>%
  cols_move_to_start(columns = c(headshot, pos, plays)) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c("displayName")) %>%
  gt_color_box(Overall.Tackle.Rank, 
               domain = range(ranks_HOU$Overall.Tackle.Rank),
               palette = c("green3", "lightgrey", "red3"),
               accuracy = 1,
               width = 150
  ) %>%
  gt_color_box(Percentile, 
               domain = 0:100,
               palette = c("red3", "lightgrey", "green3"),
               accuracy = 0.01,
               width = 100
  ) %>%
  tab_source_note(source_note = md("**NFL Big Data Bowl 2024 ~ Coaching Presentation Route** <br> 
                                 *Grant Hizer: @GPHizerStats | CJ Olson: @CJOlsonFB | Nick Fullerton: @NickFullerton00* <br>
                                 *Data: NFL Operations, nflreadR (@nflfastR)*")) %>%
  gtExtras::gt_theme_guardian()

# Save table as PNG for use in submission 
gtsave(HOU_table,"projectTables/HOU_defender_rankings.png")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Create Houston DBs chart
  # # Separate only HOU DBs from HOU metric rankings
HOU_dbs <- ranks_HOU  %>%
  filter(pos == "CB" | pos == "FS")
    
  # # Create GT object table for all qualified Texans DBs
  # # To just print out table instead of save, delete "HOU_dbs_table <- "
HOU_dbs_table <- HOU_dbs %>%
  gt(rowname_col = "displayName") %>%
  cols_hide(c(nflId, team, pos, Percentile)) %>% 
  tab_header(
    title = md("<img src= 'https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/100px-Houston_Texans_logo.svg.png' style='height:30px;'>     
             Houston DB Tackle Rankings Breakdown"),
    subtitle = md("*2022 NFL Season, Weeks 1-9  |  Min. 90 snaps*")
  ) %>%
  tab_stubhead(label = "Player") %>%
  gt_img_rows(headshot, height = 25) %>%
  cols_label(
    displayName = "Player",
    headshot = "",
    plays = "Snaps",
    TPOE_rank = html("Lg. Rank:<br>TPOE"),
    TOE_rank = html("Lg. Rank:<br>TOE"),
    MTPOE_rank = html("Lg. Rank:<br>MTPOE"),
    MTOE_rank = html("Lg. Rank:<br>MTOE"),
    Overall.Tackle.Rank = "Composite Tackle Rank", 
    Percentile = "League Percentile"
  ) %>%
  cols_move_to_start(columns = c(headshot, plays)) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c("displayName")) %>%
  gt_color_box(Overall.Tackle.Rank, 
               domain = range(HOU_dbs$Overall.Tackle.Rank),
               palette = c("green3", "lightgrey", "red3"),
               accuracy = 1,
               width = 150
  ) %>%
  gt_color_box(c(TPOE_rank, TOE_rank, MTPOE_rank, MTOE_rank), 
               domain = range(1:538),
               palette = c("#adff70", "lightgrey", "#ff9b9b"),
                 accuracy = 1,
             width = 75
  ) %>%
  gt_color_box(Percentile, 
               domain = 0:100,
               palette = c("red3", "lightgrey", "green3"),
               accuracy = 0.01,
               width = 100
  ) %>%
  tab_source_note(source_note = md("**NFL Big Data Bowl 2024 ~ Coaching Presentation Route** <br> 
                                 *Grant Hizer: @GPHizerStats | CJ Olson: @CJOlsonFB | Nick Fullerton: @NickFullerton00* <br>
                                 *Data: NFL Operations, nflreadR (@nflfastR)*")) %>%
  gtExtras::gt_theme_guardian()
 
# Save table as PNG for use in submission 
gtsave(HOU_dbs_table,"projectTables/HOU_DB_rankings.png")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Create Houston DTs chart
  # # Separate only HOU DTs from HOU metric rankings
HOU_dts <- ranks_HOU  %>%
  filter(pos == "DT")

  # # Create GT object table for all qualified Texans DTs
  # # To just print out table instead of save, delete "HOU_dts_table <- "
HOU_dts_table <- HOU_dts %>%
  gt(rowname_col = "displayName") %>%
  cols_hide(c(nflId, team, pos, Percentile)) %>% 
  tab_header(
    title = md("<img src= 'https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/100px-Houston_Texans_logo.svg.png' style='height:30px;'>     
             Houston DT Tackle Rankings Breakdown"),
    subtitle = md("*2022 NFL Season, Weeks 1-9  |  Min. 90 snaps*")
  ) %>%
  tab_stubhead(label = "Player") %>%
  gt_img_rows(headshot, height = 25) %>%
  cols_label(
    displayName = "Player",
    headshot = "",
    plays = "Snaps",
    TPOE_rank = html("Lg. Rank:<br>TPOE"),
    TOE_rank = html("Lg. Rank:<br>TOE"),
    MTPOE_rank = html("Lg. Rank:<br>MTPOE"),
    MTOE_rank = html("Lg. Rank:<br>MTOE"),
    Overall.Tackle.Rank = "Composite Tackle Rank", 
    Percentile = "League Percentile"
  ) %>%
  cols_move_to_start(columns = c(headshot, plays)) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c("displayName")) %>%
  gt_color_box(Overall.Tackle.Rank, 
               domain = range(HOU_dts$Overall.Tackle.Rank),
               palette = c("green3", "lightgrey", "red3"),
               accuracy = 1,
               width = 150
  ) %>%
  gt_color_box(c(TPOE_rank, TOE_rank, MTPOE_rank, MTOE_rank), 
               domain = range(1:538),
               palette = c("#adff70", "lightgrey", "#ff9b9b"),
               accuracy = 1,
               width = 75
  ) %>%
  gt_color_box(Percentile, 
               domain = 0:100,
               palette = c("red3", "lightgrey", "green3"),
               accuracy = 0.01,
               width = 100
  ) %>%
  tab_source_note(source_note = md("**NFL Big Data Bowl 2024 ~ Coaching Presentation Route** <br> 
                                 *Grant Hizer: @GPHizerStats | CJ Olson: @CJOlsonFB | Nick Fullerton: @NickFullerton00* <br>
                                 *Data: NFL Operations, nflreadR (@nflfastR)*")) %>%
  gtExtras::gt_theme_guardian()

# Save table as PNG for use in submission 
gtsave(HOU_dts_table,"projectTables/HOU_DT_rankings.png")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Create DT player comparison chart
  # # Separate players for report from metric rankings and merge player headshots and plays
dt_ranks <- metric_rankings %>%
  filter(displayName %in% c("Khalen Saunders", "Micheal Clemons", "A.J. Epenesa", "Dawuane Smoot")) %>%
  select(-c(TPOE, TOE, MTPOE, MTOE, Average.Tackle.Rank, Average.Missed.Tackle.Rank, Average.Rank)) %>%
  left_join(player_headshots, by = c("nflId" = "gsis_it_id")) %>%
  left_join(subset(TPOE_leaders, select = c("nflId", "plays")), by = c("nflId" = "nflId"))%>%
  left_join(teams, by = c("team" = "team_abbr"))

  # # Create GT object table for DLs from report
  # # To just print out table instead of save, delete "dt_ranks_table <- "
dt_ranks_table <- dt_ranks %>%
  gt(rowname_col = "displayName") %>%
  cols_hide(c(nflId, team, pos, Percentile)) %>% 
  tab_header(
    title = md("<img src= 'https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/100px-Houston_Texans_logo.svg.png' style='height:30px;'>     
               Potential DL Acquisition Breakdown"),
    subtitle = md("*2022 NFL Season, Weeks 1-9  |  Min. 90 snaps*")
  ) %>%
  tab_stubhead(label = "Player") %>%
  gt_img_rows(headshot, height = 25) %>%
  gt_img_rows(team_logo_wikipedia, height = 25) %>%
  cols_label(
    displayName = "Player",
    headshot = "",
    team_logo_wikipedia = "",
    plays = "Snaps",
    TPOE_rank = html("Lg. Rank:<br>TPOE"),
    TOE_rank = html("Lg. Rank:<br>TOE"),
    MTPOE_rank = html("Lg. Rank:<br>MTPOE"),
    MTOE_rank = html("Lg. Rank:<br>MTOE"),
    Overall.Tackle.Rank = "Composite Tackle Rank", 
    Percentile = "League Percentile"
  ) %>%
  cols_move_to_start(columns = c(headshot, team_logo_wikipedia, plays)) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c("displayName")) %>%
  gt_color_box(Overall.Tackle.Rank, 
               domain = range(1:538),
               palette = c("green3", "lightgrey", "red3"),
               accuracy = 1,
               width = 150
  ) %>%
  gt_color_box(c(TPOE_rank, TOE_rank, MTPOE_rank, MTOE_rank), 
               domain = range(1:538),
               palette = c("#adff70", "lightgrey", "#ff9b9b"),
               accuracy = 1,
               width = 75
  ) %>%
  gt_color_box(Percentile, 
               domain = 0:100,
               palette = c("red3", "lightgrey", "green3"),
               accuracy = 0.01,
               width = 100
  ) %>%
  tab_source_note(source_note = md("**NFL Big Data Bowl 2024 ~ Coaching Presentation Route** <br> 
                                   *Grant Hizer: @GPHizerStats | CJ Olson: @CJOlsonFB | Nick Fullerton: @NickFullerton00* <br>
                                   *Data: NFL Operations, nflreadR (@nflfastR)*")) %>%
  gtExtras::gt_theme_guardian()

# Save table as PNG for use in submission
gtsave(dt_ranks_table,"projectTables/DL_acquisition_rankings.png")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Create LB player comparison chart
  # # Separate players for report from metric rankings and merge player headshots and plays
lb_ranks <- metric_rankings %>%
  filter(displayName %in% c("Devin Bush", "Cole Holcomb", "Akeem Davis-Gaither", "Tae Crowder", "Frankie Luvu")) %>%
  select(-c(TPOE, TOE, MTPOE, MTOE, Average.Tackle.Rank, Average.Missed.Tackle.Rank, Average.Rank)) %>%
  left_join(player_headshots, by = c("nflId" = "gsis_it_id")) %>%
  left_join(subset(TPOE_leaders, select = c("nflId", "plays")), by = c("nflId" = "nflId"))%>%
  left_join(teams, by = c("team" = "team_abbr"))


  # # Create GT object table for LBs from report
  # # To just print out table instead of save, delete "lb_ranks_table <- "
lb_ranks_table <- lb_ranks %>%
  gt(rowname_col = "displayName") %>%
  cols_hide(c(nflId, team, pos, Percentile)) %>% 
  tab_header(
    title = md("<img src= 'https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/100px-Houston_Texans_logo.svg.png' style='height:30px;'>     
               Potential LB Acquisition Breakdown"),
    subtitle = md("*2022 NFL Season, Weeks 1-9  |  Min. 90 snaps*")
  ) %>%
  tab_stubhead(label = "Player") %>%
  gt_img_rows(headshot, height = 25) %>%
  gt_img_rows(team_logo_wikipedia, height = 25) %>%
  cols_label(
    displayName = "Player",
    headshot = "",
    team_logo_wikipedia = "",
    plays = "Snaps",
    TPOE_rank = html("Lg. Rank:<br>TPOE"),
    TOE_rank = html("Lg. Rank:<br>TOE"),
    MTPOE_rank = html("Lg. Rank:<br>MTPOE"),
    MTOE_rank = html("Lg. Rank:<br>MTOE"),
    Overall.Tackle.Rank = "Composite Tackle Rank", 
    Percentile = "League Percentile"
  ) %>%
  cols_move_to_start(columns = c(headshot, team_logo_wikipedia, plays)) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c("displayName")) %>%
  gt_color_box(Overall.Tackle.Rank, 
               domain = range(1:538),
               palette = c("green3", "lightgrey", "red3"),
               accuracy = 1,
               width = 150
  ) %>%
  gt_color_box(c(TPOE_rank, TOE_rank, MTPOE_rank, MTOE_rank), 
               domain = range(1:538),
               palette = c("#adff70", "lightgrey", "#ff9b9b"),
               accuracy = 1,
               width = 75
  ) %>%
  gt_color_box(Percentile, 
               domain = 0:100,
               palette = c("red3", "lightgrey", "green3"),
               accuracy = 0.01,
               width = 100
  ) %>%
  tab_source_note(source_note = md("**NFL Big Data Bowl 2024 ~ Coaching Presentation Route** <br> 
                                   *Grant Hizer: @GPHizerStats | CJ Olson: @CJOlsonFB | Nick Fullerton: @NickFullerton00* <br>
                                   *Data: NFL Operations, nflreadR (@nflfastR)*")) %>%
  gtExtras::gt_theme_guardian()

# Save table as PNG for use in submission 
gtsave(lb_ranks_table,"projectTables/LB_acquisition_rankings.png")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Create DB player comparison chart
  # # Separate players for report from metric rankings and merge player headshots and plays
db_ranks <- metric_rankings %>%
  filter(displayName %in% c("Myles Bryant", "Jabrill Peppers", "Cornell Armstrong", "Bradley Roby", "Nick Scott", "Eli Apple")) %>%
  select(-c(TPOE, TOE, MTPOE, MTOE, Average.Tackle.Rank, Average.Missed.Tackle.Rank, Average.Rank)) %>%
  left_join(player_headshots, by = c("nflId" = "gsis_it_id")) %>%
  left_join(subset(TPOE_leaders, select = c("nflId", "plays")), by = c("nflId" = "nflId"))%>%
  left_join(teams, by = c("team" = "team_abbr"))

  # # Create GT object table for DLs from report
  # # To just print out table instead of save, delete "dt_ranks_table <- "
db_ranks_table <- db_ranks %>%
  gt(rowname_col = "displayName") %>%
  cols_hide(c(nflId, team, pos, Percentile)) %>% 
  tab_header(
    title = md("<img src= 'https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/100px-Houston_Texans_logo.svg.png' style='height:30px;'>     
               Potential DB Acquisition Breakdown"),
    subtitle = md("*2022 NFL Season, Weeks 1-9  |  Min. 90 snaps*")
  ) %>%
  tab_stubhead(label = "Player") %>%
  gt_img_rows(headshot, height = 25) %>%
  gt_img_rows(team_logo_wikipedia, height = 25) %>%
  cols_label(
    displayName = "Player",
    headshot = "",
    team_logo_wikipedia = "",
    plays = "Snaps",
    TPOE_rank = html("Lg. Rank:<br>TPOE"),
    TOE_rank = html("Lg. Rank:<br>TOE"),
    MTPOE_rank = html("Lg. Rank:<br>MTPOE"),
    MTOE_rank = html("Lg. Rank:<br>MTOE"),
    Overall.Tackle.Rank = "Composite Tackle Rank", 
    Percentile = "League Percentile"
  ) %>%
  cols_move_to_start(columns = c(headshot, team_logo_wikipedia, plays)) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c("displayName")) %>%
  gt_color_box(Overall.Tackle.Rank, 
               domain = range(1:538),
               palette = c("green3", "lightgrey", "red3"),
               accuracy = 1,
               width = 150
  ) %>%
  gt_color_box(c(TPOE_rank, TOE_rank, MTPOE_rank, MTOE_rank), 
               domain = range(1:538),
               palette = c("#adff70", "lightgrey", "#ff9b9b"),
               accuracy = 1,
               width = 75
  ) %>%
  gt_color_box(Percentile, 
               domain = 0:100,
               palette = c("red3", "lightgrey", "green3"),
               accuracy = 0.01,
               width = 100
  ) %>%
  tab_source_note(source_note = md("**NFL Big Data Bowl 2024 ~ Coaching Presentation Route** <br> 
                                   *Grant Hizer: @GPHizerStats | CJ Olson: @CJOlsonFB | Nick Fullerton: @NickFullerton00* <br>
                                   *Data: NFL Operations, nflreadR (@nflfastR)*")) %>%
  gtExtras::gt_theme_guardian()

# Save table as PNG for use in submission 
gtsave(db_ranks_table,"projectTables/DB_acquisition_rankings.png")