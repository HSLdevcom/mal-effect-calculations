# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(omxr)

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "ic")

ttimes <- read_helmet_omx(file.path(config::get("helmet_data"),
                                    config::get("results"),
                                    "Matrices",
                                    "time_aht.omx")) %>%
  dplyr::select(origin, destination, car_work, transit_work) %>%
  dplyr::filter(origin %in% centers$level & destination %in% centers$level) %>%
  dplyr::mutate(
    origin = factor(origin, levels = centers$level, labels = centers$label),
    destination = factor(destination, levels = centers$level, labels = centers$label),
    ttime_ratio = transit_work / car_work
  )


# Plot --------------------------------------------------------------------

ggplot(data = ttimes) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = ttime_ratio)) +
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
  theme(legend.position = "right")

ggsave_graph(here::here("figures", sprintf("graph_centers_%s.png", config::get("scenario"))))
