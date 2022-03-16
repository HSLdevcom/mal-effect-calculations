# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
library(ggnewscale)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "squares.rds")) %>%
  dplyr::mutate(floor_area_diff = dplyr::if_else(!ensi, -floor_area_increase_2021_2040_ve0, floor_area_increase_2021_2040_ve0))

ensi <- readr::read_rds(here::here("results", "ensi.rds"))


# Plot --------------------------------------------------------------------

df_value <- data.frame(
  x = c(-1, 1, 1, -1),
  y = c(-1, -1, 1, 1),
  value = 2,
  group = "g1",
  category = factor(c("Kyllä"), levels = c("Kyllä", "Ei"))
)

category_colors <- RColorBrewer::brewer.pal(5, "PiYG")[c(5, 1)]

ggplot() +
  geom_sf(mapping = aes(fill = floor_area_diff),
          data = results, color = NA) +
  geom_basemap() +
  geom_sf(mapping = aes(),
          data = ensi, color = "#333333", fill = NA) +
  scale_fill_fermenter(
    palette = "PiYG",
    direction = 1,
    name = "kerros-m2",
    labels = scales::label_number(accuracy = 1),
    breaks = c(-5000, -1000, -250, -5, 5, 250, 1000, 5000),
    limits = c(-10000, 10000),
    oob = scales::squish,
    guide = guide_none()
  ) +
  new_scale_fill() +
  geom_polygon(data = df_value, aes(x = x, y = y, fill = value, group = group)) +
  scale_fill_fermenter(
    palette = "Greys",
    direction = 1,
    name = "kerros-m2",
    limits = c(0, 10000),
    breaks = c(5, 250, 1000, 5000),
    guide = guide_colorsteps(order = 1)
  ) +
  geom_line(data = df_value, aes(x = x, y = y, color = category), group = 1, key_glyph = draw_key_rect) +
  scale_color_manual(
    name = "Ensisijaisella\nvyöhykkeellä",
    values = category_colors,
    drop = FALSE,
    guide = guide_legend(order = 2)
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Uudet asuinkerrosneliöt ensisijaisesti kehitettävillä vyöhykkeillä",
    subtitle = "2040 Vertailupohja"
  ) +
  theme_mal_map() +
  theme(legend.box = "horizontal",
        legend.box.just = "top")

ggsave_map(here::here("figures", "map_ensi-kem2_2040_ve0.png"))
