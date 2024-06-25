library(worldfootballR)
library(dplyr)

df_results <- fb_match_results(season_end_year = c(2019, 2024),
                               gender = "M",
                               country = "ENG",
                               tier = "1st")

df_results |>
  mutate(
    day_of_week = weekdays(Date)
  ) |>
  mutate(day_of_week = ifelse(day_of_week == "Saturday",
                              "Saturday",
                              "Not Saturday")) |>
  group_by(Season_End_Year, day_of_week) |>
  count() |>
  ungroup() |>
  write.csv("other_results/epl_results.csv", row.names = )
