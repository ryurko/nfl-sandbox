# PURPOSE: Visualize Barkley's EPA per run play by index, and cumulative display

library(tidyverse)
library(nflreadr)


# Get teams and colors ----------------------------------------------------

nfl_team_colors <- load_teams() |>
  dplyr::select(team_abbr, team_division, team_color)

# Load rosters as well and grab RBs:
nfl_rbs <- load_rosters() |>
  filter(position == "RB")

# Get the play-by-play data for this season -------------------------------

nfl_pbp <- load_pbp()

# Only look at RB running plays -------------------------------------------

rb_pbp <- nfl_pbp |>
  filter(!is.na(epa), play_type == "run", penalty == 0,
         rusher_player_id %in% nfl_rbs$gsis_id) |>
  dplyr::select(game_id, play_id, week, qtr, defteam,
                rusher_player_id, rusher_player_name, 
                yards_gained, epa, wpa, touchdown)


# Visualize Barkley's EPA by carry ----------------------------------------

# Just a simple bar plot for every single carry across each game:
rb_pbp |>
  filter(rusher_player_id == "00-0034844") |>
  mutate(rush_index = 1:n(),
         is_positive = epa > 0) |>
  ggplot(aes(x = rush_index, y = epa, fill = as.factor(is_positive))) +
  geom_bar(stat = "identity", color = "white") +
  geom_point(data = {(
    rb_pbp |>
      filter(rusher_player_id == "00-0034844") |>
      mutate(rush_index = 1:n(), is_positive = epa > 0) |>
      filter(touchdown == 1, is_positive)
  )}, aes(x = rush_index, y = epa), color = "black", shape = 8) +
  scale_fill_manual(values = c("red", "blue")) +
  scale_y_continuous(breaks = c(-6, -3, 0, 3, 6)) +
  labs(x = "Carry index", y = "EPA",
       title = "Saquon Barkley's 2024 silver slugger campaign",
       subtitle = "Bars display EPA for each carry in 2024 season with points for touchdowns",
       caption = "Data courtesy of nflreadr") +
  theme_minimal() +
  theme(legend.position = "none")

# Create cumulative display with a line for every RB ----------------------

rb_pbp |>
  group_by(rusher_player_id) |>
  mutate(rush_index = 1:n(),
         total_epa = cumsum(epa)) |>
  ungroup() |>
  mutate(is_saquon = rusher_player_id == "00-0034844") |>
  ggplot(aes(x = rush_index, y = total_epa, group = rusher_player_id,
             color = is_saquon, alpha = is_saquon)) +
  geom_line() +
  scale_color_manual(values = c("gray", "#004C54")) +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Carry index", y = "Cumulative EPA",
       title = "Saquon Barkley's 2024 silver slugger campaign",
       subtitle = "Cumulative EPA for every RB in 2024 season with Barkley highlighted in green",
       caption = "Data courtesy of nflreadr") 
  
  
  
