library(dplyr)
library(tidyr)
library(nflreadr)



data_params <- list(
  seasons = seq(2002, 2023),
  season_type = "REG",
  team_location_updates = c(
    "OAK" = "LV",
    "SD" = "LAC",
    "STL" = "LA"
  ),
  cba_cutoff = 2011,
  days_cutoff_bye = 5
)

df_games <- load_schedules(seasons = data_params$seasons) |>
  filter(game_type == data_params$season_type)  |>
  mutate(
    bye_pre_cba = case_when(
      season < data_params$cba_cutoff & ((home_rest - away_rest) > data_params$days_cutoff_bye) ~ 1,
      season < data_params$cba_cutoff & ((home_rest - away_rest) < -data_params$days_cutoff_bye) ~ -1,
      T ~ 0
    ),
    bye_post_cba = case_when(
      season >= data_params$cba_cutoff & ((home_rest - away_rest) > data_params$days_cutoff_bye) ~ 1,
      season >= data_params$cba_cutoff & ((home_rest - away_rest) < -data_params$days_cutoff_bye) ~ -1,
      T ~ 0
    ),
    true_home = as.integer(location == "Home"),
    era = ifelse(season >= params$cba_cutoff,
                 "2011-23",
                 "2002-10")
  )  |>
  pivot_longer(cols = c("home_team", "away_team"), names_to = "side", values_to = "team") |>
  mutate(id = as.integer(as.factor(paste0(season, "_", team))),
         name = ifelse(team %in% names(data_params$team_location_updates),
                       data_params$team_location_updates[as.character(team)],
                       team
         )) |>
  select(-c("team")) |>
  pivot_wider(names_from = side, values_from = c("name","id")) |>
  select(
    old_game_id,
    season,
    away_team_id = id_away_team,
    home_team_id = id_home_team,
    home_team_name = name_home_team,
    away_team_name = name_away_team,
    true_home,
    bye_pre_cba,
    bye_post_cba,
    point_diff = result,
    spread = spread_line
  )
