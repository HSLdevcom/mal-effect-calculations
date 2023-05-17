# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
library(ggnewscale)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "squares.rds")) %>%
  dplyr::mutate(apartments_diff = dplyr::if_else(!ensi, -apartments_diff_2022_2035, apartments_diff_2022_2035))

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
  geom_sf(mapping = aes(fill = apartments_diff),
          data = results, color = NA) +
  geom_basemap() +
  geom_sf(mapping = aes(),
          data = ensi, color = "#333333", fill = NA, linewidth = 0.4) +
  scale_fill_fermenter(
    palette = "PiYG",
    direction = 1,
    name = "asuntoa",
    labels = scales::label_number(accuracy = 1),
    breaks = c(-150, -50, -25, -5, 5, 25, 50, 150),
    limits = c(-500, 500),
    oob = scales::squish,
    guide = guide_none()
  ) +
  new_scale_fill() +
  geom_polygon(data = df_value, aes(x = x, y = y, fill = value, group = group)) +
  scale_fill_fermenter(
    palette = "Greys",
    direction = 1,
    name = "asuntoa",
    limits = c(0, 500),
    breaks = c(5, 25, 100, 250),
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
    title = "Ennustettu asuntotuotanto ensisijaisilla vyöhykkeillä",
    subtitle = NULL
  ) +
  theme_mal_map() +
  theme(legend.box = "horizontal",
        legend.box.just = "top")

ggsave_map(here::here("figures", "map_ensi_apartments.png"))
