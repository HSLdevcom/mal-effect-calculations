# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

centers <- readr::read_rds(here::here("results", "centers_uusimaa-2050.rds")) %>%
  dplyr::mutate(center_number = row_number())


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap() +
  geom_sf(data = centers, aes(color = kuvaus), size = points2mm(15)) +
  geom_sf_text(data = centers, mapping = aes(label = center_number), size = points2mm(10)) +
  annotate(
    "text",
    x = bbox$xmin,
    y = bbox$ymax - 4000,
    hjust = 0,
    vjust = 1,
    label = glue::glue_collapse(glue::glue("{rownames(centers)}. {centers$kohteenNimi}"), sep = "\n"),
    size = points2mm(7),
    colour = "#333333"
  ) +
  annotate_map(
    title = "Helsingin seudun keskukset (Uusimaa-kaava 2050)",
    subtitle = NULL
  ) +
  scale_color_manual(
    values = c("#f29714", "#f8c884", "#f29714")
  ) +
  coord_sf_mal() +
  theme_mal_map() +
  theme(legend.position = "none")

ggsave_map(here::here("figures", "map_centers_uusimaa-2050.png"))
