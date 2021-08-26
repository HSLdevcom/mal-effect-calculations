# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = car_density)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  scale_y_continuous(
    labels = scales::label_number()
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#7DAD58", "#BFD7AC")
  ) +
  labs(
    title = "Henkilöautotiheys",
    x =  NULL,
    y = "autoa / 1000 asukasta"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_car_density.png"))
