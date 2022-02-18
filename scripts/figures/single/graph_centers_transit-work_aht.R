# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(hub)

results <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", scenario_attributes[["scenario"]]))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)


# Plot --------------------------------------------------------------------

ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = ttime_transit_work_aht)) +
  scale_x_discrete(position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = "minuuttia",
    palette = "BuPu",
    direction = 1,
    breaks = seq(50, 300, 50),
    limits = c(0, 350),
    labels = scales::number_format()
  ) +
  labs(
    title = "Joukkoliikenteen matkavastus\naamuhuipputuntina",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], config::get("scenario_name")),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

ggsave_graph(here::here("figures", sprintf("graph_ttime_transit-work_aht_%s.png", scenario_attributes[["scenario"]])))
