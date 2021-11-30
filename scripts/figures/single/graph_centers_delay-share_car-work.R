# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(omxr)


# Data --------------------------------------------------------------------

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(hub)

results <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", config::get("scenario")))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)


# Plot --------------------------------------------------------------------

ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = delay_share_car_work)) +
  scale_x_discrete(position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = "%",
    palette = "Reds",
    direction = 1,
    breaks = seq(0.05, 0.25, 0.05),
    limits = c(0, 0.30),
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = "Ruuhkaviiveen osuus henkilöautoliikenteen\nmatka-ajasta aamuhuipputuntina",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name")),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

ggsave_graph(here::here("figures", sprintf("graph_delay-share_car-work_%s.png", config::get("scenario"))))
