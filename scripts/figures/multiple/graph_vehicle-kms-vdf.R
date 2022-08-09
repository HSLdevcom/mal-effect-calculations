# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "vdfs_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = vdf, y = share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = share_lower, ymax = share_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    size = 0.35
  ) +
  geom_text(
    aes(
      y = share / 2,
      label = scales::label_percent(accuracy = 1, suffix = "", decimal.mark = ",")(share),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(20)
  ) +
  scale_fill_manual(
    name = NULL,
    values = hsl_blues_fill
  ) +
  scale_color_manual(
    guide = "none",
    values = mal_color
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen kilometrisuorite väylätyypeittäin",
    x = NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_vehicle-kms-vdf.png"))
