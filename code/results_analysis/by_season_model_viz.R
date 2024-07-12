library(ggridges)
library(tidyverse)
library(rstan)

df_plot_data = data.frame()

source("code/utils.R")

for(f in list.files("stan_results", pattern = "^(^split_bye_season|^.rds)")){
  
  file_split <- str_split_1(f, "__|\\.")
  
  outcome = tools::toTitleCase(gsub("_", " ", file_split[2]))
  
  outcome = ifelse(outcome == "Spread Line",
                   "Point Spread",
                   "Point Differential")
  
  model <- read_rds(paste0("stan_results/", f))
  
  model_results <- model |>  rstan::extract()
  
  df_plot_data <- model_results$alpha_bye |>
    as.data.frame() |>
    pivot_longer(cols = everything(), names_to = "season", values_to = "value") |>
    mutate(season = as.integer(gsub("V", "", season)) + 2001,
           outcome = outcome) |>
    bind_rows(df_plot_data)
  
}


plot_box_by_season <- df_plot_data |>
  ggplot(aes(x = paste0("'", substr(as.character(season), 3, 4)),
             y = value)) +
  geom_boxplot() +
  geom_vline(aes(xintercept = 9.5), linetype = "dashed") +
  facet_wrap(~outcome) +
  ggtitle("Point advantage from the bye week") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  xlab("Season") +
  ylab("Estimate") +
  scale_x_discrete(labels = ifelse(seq(2, 24) %% 2 == 0,
                                   paste0("'", str_pad(seq(2, 24), pad = 0, width = 2)),
                                   ""))
 



ggsave(paste0("visualizations/box_plot_bye_by_season.png"),
       plot_box_by_season,
       width = 7, height = 4)

