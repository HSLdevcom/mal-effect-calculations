# # -*- coding: utf-8-unix -*-
# library(here)
# library(tidyverse)
# library(sf)
# source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
# library(ggnewscale)
#
#
# # Data --------------------------------------------------------------------
#
# results <- readr::read_rds(here::here("results", "squares.rds")) %>%
#   dplyr::mutate(diff_pop = pop_diff_2020_2040_ve1) %>%
#   dplyr::filter(diff_pop >= 5) %>%
#   dplyr::mutate(diff_pop = dplyr::if_else(center_or_station_2040_ve1, diff_pop, -diff_pop))
#
#
# # Plot --------------------------------------------------------------------
#
# df_value <- data.frame(
#   x = c(-1, 1, 1, -1),
#   y = c(-1, -1, 1, 1),
#   value = 2,
#   group = "g1",
#   category = factor(c("Kyllä"), levels = c("Kyllä", "Ei"))
# )
#
# category_colors <- RColorBrewer::brewer.pal(5, "PRGn")[c(5, 1)]
#
# ggplot() +
#   geom_sf(mapping = aes(fill = diff_pop),
#           data = results, color = NA) +
#   geom_basemap() +
#   scale_fill_fermenter(
#     palette = "PRGn",
#     name = "asukasta",
#     direction = 1,
#     labels = scales::label_number(accuracy = 1),
#     breaks = c(-100, -50, -25, -5, 5, 25, 50, 100),
#     limits = c(-150, 150),
#     oob = scales::squish,
#     guide = guide_none()
#   ) +
#   new_scale_fill() +
#   geom_polygon(data = df_value, aes(x = x, y = y, fill = value, group = group)) +
#   scale_fill_fermenter(
#     palette = "Greys",
#     name = "asukasta",
#     direction = 1,
#     labels = scales::label_number(accuracy = 1),
#     breaks = c(5, 25, 50, 100),
#     limits = c(0, 150),
#     oob = scales::squish,
#     guide = guide_colorsteps(order = 1)
#   ) +
#   geom_line(data = df_value, aes(x = x, y = y, color = category), group = 1, key_glyph = draw_key_rect) +
#   scale_color_manual(
#     name = "Keskuksessa tai\nraideliikenteen piirissä",
#     values = category_colors,
#     drop = FALSE,
#     guide = guide_legend(order = 2)
#   ) +
#   coord_sf_mal() +
#   annotate_map(
#     title = "Uusien asukkaiden sijoittuminen seudun keskuksiin ja raskaan raideliikenteen piiriin",
#     subtitle = "2040 1. luonnos"
#   ) +
#   theme_mal_map() +
#   theme(legend.box = "horizontal",
#         legend.box.just = "top")
#
# ggsave_map(here::here("figures", "map_centers-and-stations_2040_ve1.png"))
