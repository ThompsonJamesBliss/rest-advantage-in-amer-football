library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(rstan)

params <- list(
  seed = 73097,
  chains = 4,
  iter = 3000,
  warmup = 1000,
  adapt_delta = 0.95,
  outcomes = c("point_diff", "spread_line")
)

df_games <- read_csv("data/games_clean.csv")

### List of Stan Params
stan_data <- list(
  num_clubs = length(unique(df_games$away_team_id)),
  num_games = nrow(df_games),
  num_seasons = length(unique(df_games$season)),
  num_eras = length(unique(df_games$era)),
  home_team_code = df_games$home_team_id,
  away_team_code = df_games$away_team_id,
  season = df_games$season - min(df_games$season) + 1,
  era = as.integer(as.factor(df_games$era)),
  h_adv = df_games$true_home,
  bye = df_games$bye,
  mini = df_games$mini,
  mnf = df_games$mnf
)


for (o in params$outcomes) {
  stan_data$outcome <- df_games |> pull(o)


  # ### Fit Model
  model <- stan(
    file = "stan/model_no_split.stan",
    data = stan_data,
    seed = params$seed,
    chains = params$chains,
    iter = params$iter,
    warmup = params$warmup,
    control = list(adapt_delta = params$adapt_delta),
    pars = c("mu"),
    include = F
  )

  write_rds(
    model,
    paste0("stan_results/no_split__", o, "__", params$min_season, "_", params$max_season, ".rds")
  )


  rm("model")

  # ### Fit Model
  model <- stan(
    file = "stan/model_split_bye.stan",
    data = stan_data,
    seed = params$seed,
    chains = params$chains,
    iter = params$iter,
    warmup = params$warmup,
    control = list(adapt_delta = params$adapt_delta),
    pars = c("mu"),
    include = F
  )

  write_rds(
    model,
    paste0("stan_results/split_bye__", o, "__", params$min_season, "_", params$max_season, ".rds")
  )

  rm("model")
}
