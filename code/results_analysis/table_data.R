library(stringr)
library(purrr)
library(dplyr)
library(readr)
library(rstan)


df_games_reg <- read_csv("data/games_clean.csv")

df_games_both_sides_reg <- df_games_reg |>
  mutate(Side = "Home") |>
  bind_rows(df_games_reg |>
            rename(
              away_team_name = home_team_name,
              home_team_name = away_team_name,
              away_team_id = home_team_id,
              home_team_id = away_team_id
            ) |>
            mutate(
              point_diff = -point_diff,
              spread_line = -spread_line,
              bye = -bye,
              mnf = -mnf,
              tnf = -tnf
            ) |>
              mutate(Side = "Away"))

df_games_both_sides_reg <- df_games_both_sides_reg |>
  bind_rows(
    df_games_both_sides_reg |>
      mutate(era = "All")
  )

table_data_reg <- df_games_both_sides_reg |>
  filter(bye == 0,
         mnf == 0,
         tnf == 0)  |>
  mutate(Type = "Equivalent Rest")  |>
  bind_rows(
    df_games_both_sides_reg |>
      filter(mnf == 1)  |>
      mutate(Type = "MNF Rest")
  ) |>
  bind_rows(
    df_games_both_sides_reg |>
      filter(tnf == 1)  |>
      mutate(Type = "TNF Rest")
  ) |>
  bind_rows(
    df_games_both_sides_reg |>
      filter(bye == 1)  |>
      mutate(Type = "Bye Rest")
  ) |>
  mutate(Type = factor(Type,
                       c("Equivalent Rest",
                         "MNF Rest",
                         "TNF Rest",
                         "Bye Rest"))) |>
  group_by(Side, Type, era) |>
  summarise(
    n = n(),
    `Point Diff` = mean(point_diff),
    `Win Pct` = mean(point_diff > 0) + 0.5 * mean(point_diff == 0),
    `Exp Win Pct` = mean(points_to_prob(spread_line)),
    `Cover %` = mean(point_diff > spread_line) + 0.5 * mean(point_diff == spread_line)
  ) |>
  mutate(Type2 = ifelse(row_number() == 2,
                        as.character(Type),
                        "")) |>
  ungroup() |>
  arrange(Side, Type, era) |>
  select(-c("Type")) |>
  select(Side, Type = Type2,  everything())


write(print(xtable::xtable(x = table_data_reg, digits = 3), include.rownames=FALSE),
      "other_results/rest_table_reg.txt")

dropped_games <- read_csv("data/games_raw.csv") |>
  anti_join(df_games_reg) |>
  mutate(
    drop_reason = case_when(weekdays(game_date) == "Tuesday" |
                              schedule_status != "A" ~ "Rescheduled or Cancelled Game",
                            T ~ "Game After Rescheduled or Cancelled Game")
  ) |>
  select(season, week, home_team, away_team, drop_reason)



write(print(xtable::xtable(x = dropped_games, digits = 0), include.rownames=FALSE),
      "other_results/dropped_games.txt")

table_games_count <- df_games_reg |>
  drop_na(c(days_since_last_game_home_team, days_since_last_game_away_team)) |>
  rename(Mini = tnf,
         `Rest Days Home` = days_since_last_game_home_team,
         `Rest Days Away` = days_since_last_game_away_team,
         Bye = bye,
         MNF = mnf) |>
  group_by(`Rest Days Home`, `Rest Days Away`, Bye, Mini, MNF)|>
  summarise(
    Count = n()
  ) |>
  ungroup()


write(print(xtable::xtable(x = table_games_count, digits = 0), include.rownames=FALSE),
      "other_results/table_count.txt")



df_games_post <- read_csv("data/post_season_games.csv")


df_games_both_sides_post <- df_games_post

df_games_both_sides_post <- df_games_both_sides_post |>
  bind_rows(
    df_games_both_sides_post |>
      mutate(era = "All")
  )


table_data_post <- df_games_both_sides_post |>
  filter(bye == 0)  |>
  mutate(Type = "Equivalent Rest")  |>
  bind_rows(
    df_games_both_sides_post |>
      filter(bye == 1)  |>
      mutate(Type = "Bye Rest")
  ) |>
  mutate(Type = factor(Type,
                       c("Equivalent Rest",
                         "Bye Rest"))) |>
  group_by(Type, era) |>
  summarise(
    n = n(),
    `Point Diff` = mean(point_diff),
    `Win Pct` = mean(point_diff > 0) + 0.5 * mean(point_diff == 0),
    `Exp Win Pct` = mean(points_to_prob(spread_line)),
    `Cover %` = mean(point_diff > spread_line) + 0.5 * mean(point_diff == spread_line)
  ) |>
  mutate(Type2 = ifelse(row_number() == 2,
                        as.character(Type),
                        "")) |>
  ungroup() |>
  arrange(Type, era) |>
  select(-c("Type")) |>
  select(Type = Type2,  everything())


write(print(xtable::xtable(x = table_data_post, digits = 3), include.rownames=FALSE),
      "other_results/rest_table_post.txt")



