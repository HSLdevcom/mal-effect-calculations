# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(center)

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::filter(zone %in% centers$level) %>%
  dplyr::mutate(center_number = match(zone, centers$level)) %>%
  sf::st_centroid()


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap() +
  geom_sf(data = zones, color = "#64BE1E", size = points2mm(15)) +
  geom_sf_text(data = zones, mapping = aes(label = center_number), size = points2mm(10)) +
  annotate(
    "text",
    x = bbox$xmin,
    y = bbox$ymax - 8000,
    hjust = 0,
    vjust = 1,
    label = glue::glue_collapse(glue::glue("{rownames(centers)}. {centers$label}"), sep = "\n"),
    size = points2mm(10),
    colour = "#333333"
  ) +
  annotate_map(
    title = "Helsingin seudun keskukset",
    subtitle = NULL
  ) +
  coord_sf_mal() +
  theme_mal_map()

ggsave_map(here::here("figures", "map_centers.png"))
