# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "emissions_all.rds")) %>%
  dplyr::filter(vehicle != "total")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = vehicle, y = dist, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = dist_lower, ymax = dist_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = dist / 2,
      label = scales::label_number(accuracy = 0.1, scale = 10^(-6), decimal.mark = ",")(dist),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 10^(-6)),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = mal_greens_fill
  ) +
  scale_color_manual(
    guide = "none",
    values = mal_color
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen suorite Helsingin seudulla",
    x =  NULL,
    y = "milj. ajon.km / arki-vrk"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_dist.png"), width = 150, height = 84)
