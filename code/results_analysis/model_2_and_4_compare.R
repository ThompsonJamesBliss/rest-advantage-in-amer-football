library(ggridges)
library(tidyverse)
library(rstan)

df_plot_data = data.frame()

source("code/utils.R")


for(f in list.files("stan_results", pattern = "^(^split_bye|^.rds)")[!grepl("season", list.files("stan_results", pattern = "^(^split_bye|^.rds)"))]){

  file_split <- str_split_1(f, "__|\\.")

  outcome = tools::toTitleCase(gsub("_", " ", file_split[2]))
    
  outcome = ifelse(outcome == "Spread Line",
                   "Point Spread",
                   "Point Differential")
  
  model <- read_rds(paste0("stan_results/", f))

  model_results <- model |>  rstan::extract()
  
  df_plot_data <- data.frame(type = "2002-10\n(Pre CBA Change)",
                           value = model_results$alpha_bye[,1],
                           outcome = outcome) |>
    bind_rows(data.frame(type = "2011-23\n(Post CBA Change)",
                         value = model_results$alpha_bye[,2],
                         outcome = outcome),
              df_plot_data)
  
  
  # df_plot_data <- data.frame(#type = "Bye (2002-10)",
  #                            value = model_results$alpha_bye[,2]- model_results$alpha_bye[,1],
  #                            outcome = outcome) |>
  #   bind_rows(df_plot_data)
  
  #|>
    # bind_rows(data.frame(type = "Bye (2011-23)",
    #                      value = model_results$alpha_bye[,2],
    #                      outcome = outcome),
    #           df_plot_data)

}
# 
# plot_density_ridges <- df_plot_data |>
#   ggplot(aes(value, fill = outcome)) +
#   geom_density(alpha = 0.4, color = "black") +
#   theme_bw() +
#   theme(axis.line.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank()) +
#   xlab("Difference in Points Added") +
#   ylab("") +
#   scale_fill_manual(values=c("lightblue", "coral1")) +
#   ggtitle("2011-23 Bye Effect − 2002-10 Bye Effect") +
#   labs(subtitle = "(split Bye Advantage effect)",
#        fill = "")
# 
# 
# 
# ggsave(paste0("visualizations/density_plot_bye_compare.png"),
#        plot_density_ridges,
#        width = 5, height = 3)




plot_box <- df_plot_data |>
  ggplot(aes(type, value, color = outcome)) +
  geom_hline(mapping = aes(yintercept = 0), linetype = "dashed") +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Points Added") +
  scale_color_manual(values=c("lightblue", "coral1")) +
  ggtitle("Point advantage from the bye week") +
  labs(color = "") +
  scale_y_continuous(breaks = seq(-3, 5))



ggsave(paste0("visualizations/box_plot_bye_compare.png"),
       plot_box,
       width = 5, height = 4)
