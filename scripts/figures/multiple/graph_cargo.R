# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "cargo_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = group, y = cost)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = cost_lower, ymax = cost_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    size = 0.35
  ) +
  geom_text(
    aes(y = cost / 2, label = scales::label_number(accuracy = 0.1, decimal.mark = ",")(cost)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_number(),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(25)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#7DAD58", "#BFD7AC")
  ) +
  labs(
    title = "Tavaraliikennekuljetuksen keskikustannus",
    x =  NULL,
    y = "e / matka"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_cargo.png"))
