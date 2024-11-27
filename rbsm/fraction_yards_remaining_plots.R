# PURPOSE: Explore modeling yards gained as a function of what's available on the
#          field, ignore EPA/WPA stuff...

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
  dplyr::select(game_id, play_id, week, qtr, defteam, yardline_100, ydstogo,
                rusher_player_id, rusher_player_name, 
                yards_gained, epa, wpa, touchdown) |>
  mutate(perc_gained = yards_gained / yardline_100)


# Make a simple histogram of Saquon's runs --------------------------------

rb_pbp |>
  filter(rusher_player_id == "00-0034844") |>
  ggplot(aes(x = perc_gained)) +
  geom_histogram() +
  theme_light()

# Negative values are kind of weird, that extreme value is due to 1 yard loss
# on the 1 yard line...

saquon_plot <- rb_pbp |>
  filter(rusher_player_id == "00-0034844") |>
  mutate(perc_gained = pmax(0, perc_gained)) |>
  ggplot(aes(x = perc_gained)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), closed = "left",
                 color = "gray", fill = "#004C54") +
  labs(x = "Fraction of yards to endzone Saquon gained on carry",
       y = "Number of carries") +
  theme_light()


other_rb_plot <- rb_pbp |>
  filter(rusher_player_id != "00-0034844") |>
  mutate(perc_gained = pmax(0, perc_gained)) |>
  ggplot(aes(x = perc_gained)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), closed = "left",
                 color = "black", fill = "gray") +
  labs(x = "Fraction of yards to endzone gained on carry",
       y = "Number of carries") +
  theme_light()

rb_pbp |>
  mutate(is_saquon = rusher_player_id == "00-0034844",
         perc_gained = pmax(0, perc_gained)) |>
  ggplot(aes(x = perc_gained, fill = is_saquon,
             y = after_stat(density))) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), closed = "left",
                 color = "black", position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("gray", "#004C54"),
                    labels = c("Everyone else", "Saquon")) +
  guides(fill = guide_legend(position = "inside",
                             override.aes = list(size = 12))) +
  labs(x = "Fraction of yards to endzone gained on carry",
       y = "Conditional density estimate",
       title = "Saquon hits more explosive plays than other NFL RBs in 2024 season",
       subtitle = "Conditional density histograms for fraction of yards to endzone gained on carries, split between Saquon's carries (green) versus all other NFL RBs (gray)",
       caption = "Data courtesy of nflreadr") +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position.inside = c(.3, .7),
        legend.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12))

# ECDF is probably what we really want here...
rb_pbp |>
  mutate(is_saquon = rusher_player_id == "00-0034844",
         perc_gained = pmin(pmax(0, perc_gained), 1)) |>
  ggplot(aes(x = perc_gained, color = is_saquon)) +
  stat_ecdf(linewidth = 1.5) +
  annotate("text", x = .1, y = .9, label = "Everyone else", color = "gray",
           size = 10) +
  annotate("text", x = .5, y = .75, label = "Saquon", color = "#004C54",
           size = 10) +
  scale_color_manual(values = c("gray", "#004C54"),
                    labels = c("Everyone else", "Saquon")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "Fraction of yards to endzone gained on carry",
       y = "Distribution percentiles",
       title = "Saquon hits more explosive plays than other NFL RBs in 2024 season",
       subtitle = "ECDF for fraction of yards to endzone gained on carries, split between Saquon's carries (green) versus all other NFL RBs (gray)",
       caption = "Data courtesy of nflreadr") +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12))

# So what does the passing version look like? -----------------------------

pass_pbp <- nfl_pbp |>
  filter(!is.na(epa), play_type == "pass", penalty == 0) |>
  dplyr::select(game_id, play_id, week, qtr, defteam, yardline_100, ydstogo,
                yards_gained, epa, wpa, touchdown) |>
  mutate(perc_gained = yards_gained / yardline_100)

rb_pbp |>
  filter(rusher_player_id == "00-0034844") |>
  mutate(play_type = "saquon_runs") |>
  bind_rows(mutate(pass_pbp, play_type = "nfl_pass")) |>
  mutate(perc_gained = pmin(pmax(0, perc_gained), 1)) |>
  ggplot(aes(x = perc_gained, color = play_type)) +
  stat_ecdf(linewidth = 1.5) +
  annotate("text", x = .35, y = 1, label = "NFL passing plays",
           color = "darkorange", size = 10) +
  annotate("text", x = .75, y = .8, label = "Saquon", color = "#004C54",
           size = 10) +
  scale_color_manual(values = c("darkorange", "#004C54"),
                     labels = c("NFL passing plays", "Saquon")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "Fraction of yards to endzone gained on carry",
       y = "Distribution percentiles",
       title = "Is Saquon more explosive than passing plays?",
       subtitle = "ECDF for fraction of yards to endzone gained on Saquon's carries (green) versus NFL passing plays (orange)",
       caption = "Data courtesy of nflreadr") +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12))



