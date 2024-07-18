library(ggridges)
library(tidyverse)
library(rstan)
library(loo)

source("code/utils.R")

df_games <- read_csv("data/games_clean.csv")

params <- list(team_division = "AFC East")

df_club <- nflreadr::load_teams() |>
  select(
    team_name = team_abbr,
    team_division,
    team_color,
    team_color2
  )


loo_objects <- list()

i <- 1

min_season <- min(df_games$season)

max_season <- max(df_games$season)

for (f in list.files("stan_results", pattern = ".rds")) {
  print(f)

  file_split <- str_split_1(f, "__|\\.")

  type <- file_split[1]

  outcome <- file_split[2]

  model_title <- case_when(
    outcome == "point_diff" & type == "no_split" ~ "Point Differential Impact",
    outcome == "point_diff" & type == "split_bye" ~ "Point Differential Impact",
    outcome == "spread_line" & type == "no_split" ~ "Point Spread Impact",
    outcome == "spread_line" & type == "split_bye" ~ "Point Spread Impact"
  )

  model_sub <- case_when(
    outcome == "point_diff" & type == "no_split" ~ "(Model 1, constant Bye Advantage effect)",
    outcome == "point_diff" & type == "split_bye" ~ "(Model 2, split Bye Advantage effect)",
    outcome == "spread_line" & type == "no_split" ~ "(Model 3, constant Bye Advantage effect)",
    outcome == "spread_line" & type == "split_bye" ~ "(Model 4, split Bye Advantage effect)"
  )


  model <- read_rds(paste0("stan_results/", f))

  model_results <- model |> rstan::extract()

  df_team_strengths <- data.frame()

  for (t in seq(dim(model_results$theta)[2])) {
    for (s in seq(dim(model_results$theta)[3])) {
      df_team_strengths <- data.frame(
        team_id = t,
        season = s + min_season - 1,
        theta = median(model_results$theta[, t, s]),
        outcome = outcome
      ) |>
        bind_rows(df_team_strengths)
    }
  }

  plot_team_strength <- df_games |>
    select(season, team_id = away_team_id, team_name = away_team_name) |>
    distinct() |>
    inner_join(df_club, by = "team_name") |>
    inner_join(df_team_strengths, by = c("season", "team_id")) |>
    filter(team_division == params$team_division) |>
    ggplot(aes(season, theta, color = team_color, fill = team_color2)) +
    geom_line(lwd = 2) +
    geom_point(shape = 21, size = 2.5) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_bw() +
    labs(
      x = "Season",
      y = "Points Above Average",
      subtitle = model_sub
    ) +
    scale_x_continuous(
      breaks = seq(
        as.integer(min_season),
        as.integer(max_season)
      ),
      labels = paste0(
        "'",
        substr(
          seq(
            as.integer(min_season),
            as.integer(max_season)
          ),
          3,
          4
        )
      )
    ) +
    ggtitle(paste0("Team Strengths - ", model_title))

  ggsave(paste0("visualizations/points_added__", outcome, "__", type, ".jpeg"),
    plot_team_strength,
    width = 5,
    height = 3.1
  )

  trace_plot <- traceplot(model, pars = c(
    "alpha_bye",
    "alpha_mnf",
    "alpha_mini"
  )) +
    xlab("Iteration") +
    ggtitle(paste0("Trace Plot - ", model_title)) +
    theme_bw() +
    labs(subtitle = model_sub) +
    facet_wrap(~parameter, scales = "fixed", labeller = as_labeller(function(x) {
      c(
        "alpha_mnf" = "MNF",
        "alpha_mini" = "Mini",
        "alpha_bye" = "Bye",
        "alpha_bye[1]" = "Bye, Pre",
        "alpha_bye[2]" = "Bye, Post"
      )[x]
    }), nrow = 2, ncol = 2)

  ggsave(paste0("visualizations/trace__", outcome, "__", type, ".jpeg"), trace_plot, width = 5, height = 4)

  if (length(dim(model_results$alpha_bye)) == 1) {
    df_plot_data <- data.frame(
      type = "Bye",
      value = model_results$alpha_bye
    )
  } else {
    df_plot_data <- data.frame(
      type = "Bye (2002-10)",
      value = model_results$alpha_bye[, 1]
    ) |>
      bind_rows(data.frame(
        type = "Bye (2011-23)",
        value = model_results$alpha_bye[, 2]
      ))
  }

  df_plot_data <- df_plot_data |>
    bind_rows(data.frame(
      type = "MNF",
      value = model_results$alpha_mnf
    )) |>
    bind_rows(data.frame(
      type = "Mini",
      value = model_results$alpha_mini
    ))   |>
    bind_rows(data.frame(
      type = paste0("Home Adv (", max_season, ")"),
      value = model_results$alpha_ha_trend * (max_season - min_season + 1) + model_results$alpha_ha_intercept
    )) |>
    bind_rows(data.frame(
      type = paste0("Home Adv (", min_season, ")"),
      value = model_results$alpha_ha_trend + model_results$alpha_ha_intercept
    ))
  
  
  df_plot_data <- df_plot_data |>
    mutate(
      type = fct_reorder(type, value),
    )


  if (outcome == "point_diff") {
    lim <- c(-2, 6)
  } else if (outcome == "spread_line") {
    lim <- c(-1, 5)
  }

  plot_density_ridges <- df_plot_data |>
    ggplot(aes(value, type)) +
    geom_density_ridges(fill = "lightblue", scale = 0.95, bandwidth = 0.15) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_bw() +
    xlab("Points Added") +
    ylab("") +
    scale_x_continuous(limits = lim) +
    ggtitle(model_title) +
    geom_text(
      df_plot_data |>
        group_by(type) |>
        summarise(text = paste0(pretty_digits(mean(value > 0) * 100, 1), "%")),
      mapping = aes(min(lim) + 0.05, type, label = text),
      vjust = -0.3,
      hjust = 0,
      size = 3
    ) +
    labs(subtitle = model_sub) +
    geom_text(
      df_plot_data |>
        group_by(type) |>
        summarise(text = paste0(
          pretty_digits(median(value), 2),
          " (",
          pretty_digits(quantile(value, 0.025), 2),
          ", ",
          pretty_digits(quantile(value, 0.975), 2),
          ")"
        )),
      mapping = aes(x = max(lim) - 0.05, type, label = text),
      vjust = -0.3,
      hjust = 1,
      size = 3
    ) +
    geom_text(
      df_plot_data |>
        ungroup() |>
        filter(type == last(levels(type))) |>
        group_by(type) |>
        summarise(
          text = "Pct >0:",
        ),
      fontface = "bold",
      mapping = aes(x = min(lim) + 0.05, label = text, y = type),
      vjust = -5.1,
      hjust = 0,
      size = 3
    ) +
    geom_text(
      df_plot_data |>
        ungroup() |>
        filter(type == last(levels(type))) |>
        group_by(type) |>
        summarise(
          text = "Median (95% CI):",
        ),
      fontface = "bold",
      mapping = aes(x = max(lim) - 0.05, label = text, y = type),
      vjust = -5.1,
      hjust = 1,
      size = 3
    )


  plot_density_ridges <- plot_density_ridges +
    geom_segment(
      data = ggplot_build(plot_density_ridges)$data[[1]] |>
        group_by(y) |>
        filter(ymax == max(ymax)) |>
        ungroup(),
      aes(x = x, xend = x, y = y, yend = ymax)
    )



  ggsave(paste0("visualizations/density__", outcome, "__", type, ".jpeg"),
    plot_density_ridges,
    width = 5, height = 5
  )


  df_games_temp <- df_games |>
    filter(
      season >= min_season,
      season <= max_season
    )

  stan_data <- list(
    num_clubs = length(unique(df_games_temp$away_team_id)),
    num_games = nrow(df_games_temp),
    num_seasons = length(unique(df_games_temp$season)),
    num_eras = length(unique(df_games_temp$era)),
    home_team_code = df_games_temp$home_team_id,
    away_team_code = df_games_temp$away_team_id,
    season = df_games_temp$season - min(df_games_temp$season) + 1,
    era = as.integer(as.factor(df_games_temp$era)),
    h_adv = df_games_temp$true_home,
    bye = df_games_temp$bye,
    mini = df_games_temp$mini,
    mnf = df_games_temp$mnf
  )

  log_lik <- expand_log_lik(model, type, stan_data, as.numeric(unlist(df_games_temp[outcome])))

  loo_objects[[i]] <- loo(log_lik)

  i <- i + 1
}


df_compare_point_diff <- loo_compare(loo_objects[c(1, 3)])
rownames(df_compare_point_diff)[rownames(df_compare_point_diff) == "model1"] <- "No Split"
rownames(df_compare_point_diff)[rownames(df_compare_point_diff) == "model2"] <- "Split Bye"

df_compare_point_diff |>
  as.data.frame() |>
  mutate(
    outcome = "Point Diff",
    "n_se" = abs(elpd_diff / pmax(1e-6, se_diff))
  ) |>
  write.csv("other_results/Loo_Results_Point_Diff.csv")

df_compare_spread <- loo_compare(loo_objects[c(2, 4)])
rownames(df_compare_spread)[rownames(df_compare_spread) == "model1"] <- "No Split"
rownames(df_compare_spread)[rownames(df_compare_spread) == "model2"] <- "Split Bye"

df_compare_spread |>
  as.data.frame() |>
  mutate(
    outcome = "Spread",
    "n_se" = abs(elpd_diff / pmax(1e-6, se_diff))
  ) |>
  write.csv("other_results/Loo_Results_Spread.csv")
