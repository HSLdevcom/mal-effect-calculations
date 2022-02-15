# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "squares.rds")) %>%
  dplyr::mutate(diff_pop = pop_increase_2020_2040_ve0) %>%
  dplyr::filter(diff_pop >= 5) %>%
  dplyr::mutate(diff_pop = dplyr::if_else(is.na(luokka), -diff_pop, diff_pop))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_pop),
          data = results, color = NA) +
  geom_basemap() +
  scale_fill_fermenter(
    palette = "PRGn",
    name = "asukasta",
    direction = 1,
    labels = scales::label_number(accuracy = 1),
    breaks = c(-100, -50, -25, -5, 5, 25, 50, 100),
    limits = c(-150, 150),
    oob = scales::squish,
    guide = "none"
  ) +
  # geom_sf(mapping = aes(fill = diff_pop),
  #         data = results, color = NA) +
  # geom_sf(mapping = aes(),
  #         data = dplyr::filter(results, is.na(luokka)), color = NA, fill = "#4d9221", alpha = 0.5) +
  # geom_sf(mapping = aes(),
  #         data = dplyr::filter(results, !is.na(luokka)), color = NA, fill = "#c51b7d", alpha = 0.5) +
  # geom_basemap() +
  # scale_fill_steps(
  #   name = "asukasta",
  #   labels = scales::label_number(accuracy = 1),
  #   breaks = c(5, 25, 50, 100),
#   limits = c(0, 150),
#   low = "#ffffff",
#   high = "#000000",
#   oob = scales::squish
# ) +
coord_sf_mal() +
  annotate_map(
    title = "Uusien asukkaiden sijoittuminen seudun keskuksiin ja raskaan raideliikenteen piiriin",
    subtitle = "2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_uml_2040_ve0.png"))
