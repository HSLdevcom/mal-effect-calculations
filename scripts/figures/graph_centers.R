# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(omxr)

path <- here::here("data", "Tulokset", "2020", "Matrices", "time_aht.omx")

aht <- omxr::read_all_omx(path)
zone_numbers <- omxr::read_lookup(path, name = "zone_number")

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "ic")

ttimes <- aht %>%
  dplyr::select(origin, destination, car_work, transit_work) %>%
  dplyr::mutate(
    origin = zone_numbers$Lookup[origin],
    destination = zone_numbers$Lookup[destination]
  ) %>%
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
    name = "Suhde",
    breaks = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0),
    limits = c(0, 4.0),
    type = "div",
    palette = "PiYG",
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", colour = "#333333", size = 10),
    panel.grid = element_blank()
  )

ggsave_graph(here::here("figures", "graph_centers.png"))
