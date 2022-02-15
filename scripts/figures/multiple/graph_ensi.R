# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(scenario != "2018 Nykytila")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = pop_increase_ensi_share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, suffix = "")(pop_increase_ensi_share)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 1),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("2018 Nykytila" = "#3E8606", "2040 Vertailupohja" = "#7DAD58", "2040 Varjo" = "#BFD7AC")
  ) +
  labs(
    title = "Asuntotuotannon kohdistuminen ensisijaisesti kehitettäville vyöhykkeille",
    x =  NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_ensi.png"))
