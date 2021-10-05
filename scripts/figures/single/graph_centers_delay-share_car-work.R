# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(omxr)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", config::get("scenario"))))


# Plot --------------------------------------------------------------------

ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = delay_share_car_work)) +
  scale_x_discrete(position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = NULL,
    type = "div",
    palette = "PuOr",
    breaks = seq(-0.35, 0.35, 0.1),
    limits = c(-0.35, 0.35),
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = "Ruuhkaviiveen osuus henkilöautoliikenteen matka-ajasta aamuhuipputuntina",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name")),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

ggsave_graph(here::here("figures", sprintf("graph_delay-share_car-work_%s.png", config::get("scenario"))))
