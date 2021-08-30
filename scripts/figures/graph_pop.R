# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = total_pop)) +
  geom_col(aes(fill = area), position = position_stack()) +
  scale_y_continuous(
    labels = scales::label_number()
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#BFD7AC", "#f092cd", "#007AC9", "#AAD3ED")
  ) +
  labs(
    title = "Väestö",
    x =  NULL,
    y = NULL
  ) +
  theme_mal_graph() +
  theme(legend.position = "right")

ggsave_graph(here::here("figures", "graph_pop.png"))
