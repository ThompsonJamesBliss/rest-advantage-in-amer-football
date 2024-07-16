library(dplyr)
library(readr)
library(tidyr)

df_games_raw <- read_csv("data/games_raw.csv")

df_games <- df_games_raw |>
  pivot_longer(
    cols = c("home_team", "away_team"),
    names_to = "side",
    values_to = "team"
  ) |>
  mutate(id = as.integer(as.factor(team))) |>
  group_by(season, team) |>
  arrange(season, team, game_date) |>
  mutate(
    bye = as.integer(!is.na(lag(week)) &
      (week != lag(week) + 1)),
    days_since_last_game = as.integer(difftime(game_date, lag(game_date), units = "days"))
  ) |>
  filter(
    is.na(lag(game_date)) | (weekdays(lag(game_date)) != "Tuesday"),
    weekdays(game_date) != "Tuesday",
    is.na(lag(schedule_status)) | !(lag(schedule_status) %in% c("C", "R")),
    !(schedule_status %in% c("C", "R"))
  ) |>
  ungroup() |>
  pivot_wider(names_from = side, values_from = c("team", "id", "bye", "days_since_last_game")) |>
  mutate(
    bye = bye_home_team - bye_away_team,
    mini = case_when(
      days_since_last_game_home_team %in% seq(9, 11) &
        !(days_since_last_game_away_team %in% seq(9, 11)) &
        (abs(days_since_last_game_home_team - days_since_last_game_away_team) >= 2) ~ 1,
      days_since_last_game_away_team %in% seq(9, 11) &
        !(days_since_last_game_home_team %in% seq(9, 11)) &
        (abs(days_since_last_game_away_team - days_since_last_game_home_team) >= 2) ~ -1,
      TRUE ~ 0
    ),
    mnf = case_when(
      (days_since_last_game_home_team == 6 &
        days_since_last_game_away_team >= 7) |
        (days_since_last_game_home_team == 5 &
          days_since_last_game_away_team >= 6) ~ -1,
      (days_since_last_game_away_team == 6 &
        days_since_last_game_home_team >= 7) |
        (days_since_last_game_away_team == 5 &
          days_since_last_game_home_team >= 6) ~ 1,
      TRUE ~ 0
    ),
    era = ifelse(season >= 2011,
      "2011-23",
      "2002-10"
    )
  ) |>
  select(
    game_id,
    season,
    era,
    home_team_name = team_home_team,
    away_team_name = team_away_team,
    home_team_id = id_home_team,
    away_team_id = id_away_team,
    true_home,
    bye,
    mnf,
    mini,
    point_diff,
    spread_line,
    days_since_last_game_home_team,
    days_since_last_game_away_team
  ) |>
  drop_na(-c(
    "days_since_last_game_home_team",
    "days_since_last_game_away_team"
  ))


df_games |>
  write.csv("data/games_clean.csv", row.names = FALSE)
