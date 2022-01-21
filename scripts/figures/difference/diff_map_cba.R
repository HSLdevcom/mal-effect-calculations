# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

cba <- read_tsv_helmet("T:/JohannaP/cba_2040_ve1_2040_ve0.txt", first_col_name = "zone")

results <- readr::read_rds(here::here("results", "zones_2018.rds")) %>%
  dplyr::select(zone) %>%
  dplyr::left_join(cba, by = "zone")


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = transit_work_time + transit_leisure_time),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "PRGn",
    direction = -1,
    name = "minuuttia",
    limits = c(-8000, 8000),
    labels = scales::label_number(accuracy = 1),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Matka-ajan muutos joukkoliikenteellä",
    subtitle = "2040 Vertailupohja \U2192 2040 Varjo"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_cba_transit-time.png"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = car_work_time + car_leisure_time),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "PRGn",
    direction = -1,
    name = "minuuttia",
    limits = c(-800, 800),
    labels = scales::label_number(accuracy = 1),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Matka-ajan muutos henkilöautolle",
    subtitle = "2040 Vertailupohja \U2192 2040 Varjo"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_cba_car-time.png"))
