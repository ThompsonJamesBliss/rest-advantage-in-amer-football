library(stringr)
library(purrr)
library(dplyr)
library(readr)
library(rstan)

params <- list(
  seed = 73097,
  chains = 4,
  iter = 2000,
  warmup = 500,
  adapt_delta = 0.95,
  outcomes = c("point_diff", "spread"))

source("code/get_data.R")

### List of Stan Params
stan_data <- list(
  num_clubs = length(unique(df_games$away_team_id)),
  num_games = nrow(df_games),
  num_seasons = length(unique(df_games$season)),
  home_team_code = df_games$home_team_id,
  away_team_code = df_games$away_team_id,
  season = df_games$season - min(df_games$season) + 1,
  h_adv = df_games$true_home,
  bye_pre_cba = df_games$bye_pre_cba,
  bye_post_cba = df_games$bye_post_cba
)




for(o in params$outcomes){

  stan_data$outcome = df_games |> pull(o)

  # ### Fit Model
  model <- stan(
    file = "stan/model.stan",
    data = stan_data,
    seed = params$seed,
    chains = params$chains,
    iter = params$iter,
    warmup = params$warmup,
    control = list(adapt_delta = params$adapt_delta),
    pars = c("mu"),
    include = F
  )


}
