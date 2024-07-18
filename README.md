# Bye-Bye, Bye Advantage: Estimating the competitive impact of rest differential in the National Football League
This repository contains the data and code used in a manuscript "Bye-Bye, Bye Advantage: Estimating the competitive impact of rest differential in the National Football League" by Thompson Bliss and Michael Lopez. ([Link](google.com))

## Data

* __data/games_raw.csv:__ Includes info on times, dates, season, teams, scores and spreads for each NFL game from the 2002-2023 Regular Seasons. (Used to create games_clean)
* __data/games_clean.csv:__ Cleaned version of games_raw.csv which includes columns that indicate various rest advantages for each NFL game from the 2002-2023 Regular Seasons. (Used directly in model)
* __data/post_season_games.csv:__ Includes info on season, teams, scores, spreads and bye advantages for each NFL game from the 2002-2023 Postseasons. (Used for supporting analysis)

NOTE: The raw data was partially pulled from internal NFL database so no R file is included to pull the raw data.

## code

### Model Run

* __code/model/prep_data.R__ Script to clean the raw data and format it to include the rest advantages.
* __code/model/run_analysis.R__ Script to iteratively fit the four models.

### Results Analysis

* __code/results_analysis/split_bye_box.R__: Script to create a visualization for the era-by-era bye model.
* __code/results_analysis/epl_check.R__: Script to create a csv for metrics on the English Premier League Schedule.
* __code/results_analysis/table_data.R__: Script to create tables for latex.
* __code/results_analysis/visualize_and_analyze_results.R__: Script to calculate LOO, plot team strengths and plot density plots.


---

Note the `rstan` package is required to work with model objects and/or run the modeling scripts. For assistance installing Stan, please refer to the [official documentation](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).
