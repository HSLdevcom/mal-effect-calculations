# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(hub)

results <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", config::get("scenario")))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)


# Plot --------------------------------------------------------------------

ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = ttime_car_work_aht)) +
  scale_x_discrete(position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = "minuuttia",
    palette = "OrRd",
    direction = 1,
    breaks = seq(10, 60, 10),
    limits = c(0, 70),
    labels = scales::number_format()
  ) +
  labs(
    title = "Henkilöautoliikenteen matka-aika\naamuhuipputuntina",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name")),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

ggsave_graph(here::here("figures", sprintf("graph_car-work_aht_%s.png", config::get("scenario"))))
