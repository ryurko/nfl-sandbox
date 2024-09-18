# PURPOSE: Compare success rate based on EPA and WPA... how well do they align
#          with the team's record? Do this for the current 2024 season

library(tidyverse)
library(nflreadr)
library(nflplotR)

# Get the play-by-play data for this season -------------------------------

nfl_pbp <- load_pbp()

# Without filtering anything compute success rates ------------------------

# Just do this for off perspective:
team_success_summary <- nfl_pbp |>
  filter(!is.na(posteam), !is.na(epa), !is.na(wpa)) |>
  group_by(posteam) |>
  summarize(epa_sr = mean(epa > 0),
            wpa_sr = mean(wpa > 0),
            .groups = "drop")


# Plot the rates against each other ---------------------------------------

team_success_summary %>%
  ggplot(aes(x = epa_sr, y = wpa_sr)) +
  # Add reference line:
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  # Add the logo geom layer:
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.075, alpha = 0.75) +
  labs(x = "EPA-based success rate",
       y = "WPA-based success rate",
       title = "Who is playing to win the game?",
       subtitle = "Comparison of EPA and WPA-based success rates in 2024 NFL season",
       caption = "Data courtesy of nflreadr") +
  coord_fixed() +
  theme_minimal()

