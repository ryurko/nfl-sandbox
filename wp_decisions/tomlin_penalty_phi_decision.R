# PURPOSE: See what happens based on nflfastR WP model with Tomlin's decision making

library(tidyverse)
library(nflfastR)


# Load pbp data -----------------------------------------------------------

steelers_pbp <- nflreadr::load_pbp() |>
  filter(game_id == "2024_15_PIT_PHI")

# Find the play with the penalty decision ---------------------------------

# Looked this up:
steelers_pbp |>
  filter(play_id == 2431) |>
  dplyr::select(ep, wp, epa, wpa, vegas_home_wp)
#         ep    wp   epa    wpa vegas_home_wp
#      <dbl> <dbl> <dbl>  <dbl>         <dbl>
#   1  1.77 0.713  2.00 0.0680         0.794

# Calculate the version if it was 1st and 30 ------------------------------

steelers_pbp |>
  filter(play_id == 2431) |>
  mutate(down = 1, ydstogo = 30, receive_2h_ko = 0,
         yardline_100 = yardline_100 + 10) |>
  dplyr::select(receive_2h_ko, score_differential, posteam,
                home_team, half_seconds_remaining, game_seconds_remaining,
                spread_line, down, ydstogo, yardline_100, 
                posteam_timeouts_remaining, defteam_timeouts_remaining) |>
  calculate_win_probability() |>
  dplyr::select(wp, vegas_wp)
# # A tibble: 1 × 2
#       wp vegas_wp
#     <dbl>    <dbl>
#   1 00.709    0.796
  
# Interesting...