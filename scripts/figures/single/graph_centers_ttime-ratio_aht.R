# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(center)

results <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", config::get("scenario")))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)



# Plot --------------------------------------------------------------------

ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = ttime_ratio_aht)) +
  scale_x_discrete(position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = NULL,
    breaks = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0),
    limits = c(0, 4.0),
    type = "div",
    palette = "PiYG",
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = "Joukkoliikenteen ja henkilöauton matka-aikojen suhde",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name")),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

ggsave_graph(here::here("figures", sprintf("graph_ttime-ratio_aht_%s.png", config::get("scenario"))))
