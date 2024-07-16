library(ggridges)
library(tidyverse)
library(rstan)

df_plot_data <- data.frame()

df_compare <- data.frame()

source("code/utils.R")


for (f in list.files("stan_results", pattern = "^(^split_bye|^.rds)")) {
  file_split <- str_split_1(f, "__|\\.")

  outcome <- tools::toTitleCase(gsub("_", " ", file_split[2]))

  outcome <- ifelse(outcome == "Spread Line",
    "Point Spread",
    "Point Differential"
  )

  model <- read_rds(paste0("stan_results/", f))

  model_results <- model |> rstan::extract()

  df_plot_data <- data.frame(
    type = "2002-10\n(Pre CBA Change)",
    value = model_results$alpha_bye[, 1],
    outcome = outcome
  ) |>
    bind_rows(
      data.frame(
        type = "2011-23\n(Post CBA Change)",
        value = model_results$alpha_bye[, 2],
        outcome = outcome
      ),
      df_plot_data
    )

  df_compare <- data.frame(
    outcome = outcome,
    percent_pre_greater = mean(model_results$alpha_bye[, 1] > model_results$alpha_bye[, 2])
  ) |>
    bind_rows(df_compare)
}


plot_box <- df_plot_data |>
  ggplot(aes(type, value, fill = outcome, color = outcome)) +
  geom_hline(mapping = aes(yintercept = 0), linetype = "dashed") +
  geom_boxplot(alpha = 0.3, outlier.alpha = 1) +
  theme_bw() +
  xlab("") +
  ylab("Points Added") +
  scale_fill_manual(values = c("lightblue", "white"), guide = "none") +
  scale_color_manual(values = c("lightblue", "black")) +
  ggtitle("Point advantage from the bye week") +
  labs(color = "") +
  scale_y_continuous(breaks = seq(-3, 5))



ggsave(paste0("visualizations/box_plot_bye_compare.png"),
  plot_box,
  width = 5, height = 4
)
