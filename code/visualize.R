library(nflreadr)
library(rstan)
library(ggridges)
library(tidyverse)

source("code/get_data.R")
source("code/utils.R")

params <- list(
  outcomes = c("point_diff", "spread"),
  team_division = "AFC East",
  bye_types = c("pre", "post"))


df_bye_adv <- data.frame()

for(o in params$outcomes){

  model <- read_rds(paste0("stan_results/model_", o, ".rds"))

  model_plot <- model

  model_results <- model |> extract()

  df_team_strengths <- data.frame()

  for(t in seq(model_results$theta[1,])){

    df_team_strengths <- data.frame(team_id = t,
                                    theta = median(model_results$theta[,t]),
                                    outcome = o) |>
      bind_rows(df_team_strengths)

  }



  plot_team_strength <- df_games |>
    select(season, team_id = away_team_id, team_name = away_team_name) |>
    distinct() |>
    inner_join(df_club) |>
    inner_join(df_team_strengths) |>
    filter(team_division == params$team_division) |>
    ggplot(aes(season, theta, color = team_color, fill = team_color2))  +
    geom_line(lwd = 2) +
    geom_point(shape = 21, size = 2.5) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_bw()  +
    labs(x = "Season",
         y = "Points Above Average") +
    scale_x_continuous(breaks = seq(2002, 2023), labels = paste0("'", substr(seq(2002, 2023), 3, 4))) +
    ggtitle(paste0(params$team_division, " Team Strengths"),
            subtitle = paste0("(", tools::toTitleCase(gsub("_", " ", o)), " Model)"))

  ggsave(paste0("visualizations/points_added_", o, ".png"), p, width = 5, height = 3.1)

  for(b in params$bye_types){

    df_bye_adv <- data.frame(beta =  as.numeric(unlist(model_results[paste0("beta_", b)])),
                             bye_type = b,
                             outcome = o) |>
      bind_rows(
        df_bye_adv
      )

  }



  trace_plot <- traceplot(model, paste0("beta_", params$bye_types)) +
    xlab("Iteration") +
    ggtitle(paste0(tools::toTitleCase(gsub("_", " ", o)), " Model Trace Plot"))  +
    theme_bw() +
    facet_wrap(~parameter,scales="fixed")

  ggsave(paste0("visualizations/trace_", o, ".png"), trace_plot, width = 5, height = 3.1)


}


plot_density_ridges <- df_bye_adv |>
  mutate(outcome = tools::toTitleCase(gsub("_", " ", outcome)),
         bye_type = ifelse(bye_type == "pre",
                           "Bye Week:\n2002-10",
                           "Bye Week:\n2011-23")) |>
  ggplot(aes(beta,bye_type)) +
  geom_density_ridges(fill = "lightblue", scale = 0.95) +
  facet_wrap(~outcome) +
  theme_bw() +
  xlab("Points Added") +
  ylab("") +
  ggtitle("Model Estimates of Bye Week")


plot_data <- ggplot2::ggplot_build(plot_density_ridges)$data[[1]] |>
  group_by(group, PANEL) |>
  filter(ymax == max(ymax)) |>
  filter(row_number() == 1) |>
  ungroup() |>
  mutate(
    text = format(round(x, 2), nsmall = 2),
    outcome = ifelse(PANEL == 1,
                   "Point Diff",
                   "Spread")
  )

plot_density_ridges <- plot_density_ridges +
  geom_segment(
    data = plot_data,
    mapping = ggplot2::aes(
      x = x,
      xend = x,
      y = group,
      yend = ymax
    ),
    lwd = 1
  ) +
  geom_text(
    data = plot_data,
    mapping = ggplot2::aes(
      x = x,
      y = group + 0.2,
      label = text
    ),
    hjust = -0.1,
    size = 3
  )


ggsave("visualizations/model_bye_density_ridges.png", plot_density_ridges, width = 5, height = 3)

