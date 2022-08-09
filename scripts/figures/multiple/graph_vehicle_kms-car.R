# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = vehicle_kms_car, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = vehicle_kms_car_lower, ymax = vehicle_kms_car_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    size = 0.35
  ) +
  geom_text(
    aes(
      y = vehicle_kms_car / 2,
      label = scales::label_number(scale = 0.000001, accuracy = 0.1, decimal.mark = ",")(vehicle_kms_car),
      group = area,
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.000001),
    expand = expansion(mult = c(0.025, 0.1)),
    limits = c(0, 45000000)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = mal_fill
  ) +
  scale_color_manual(
    guide = "none",
    values = mal_color
  ) +
  labs(
    title = "Henkilöautoliikenteen liikennesuorite Helsingin seudulla",
    x =  NULL,
    y = "milj. ajon.km / arki-vrk"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_vehicle_kms_car.png"))
