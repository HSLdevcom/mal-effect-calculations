# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(scenario != "2018 Nykytila")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = pop_share_ensi)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, suffix = "")(pop_share_ensi)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 1),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = mal_greens_fill[-1]
  ) +
  labs(
    title = "Uusien asukkaiden kohdistuminen\nensisijaisesti kehitettäville vyöhykkeille",
    x =  NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_ensi.png"))
