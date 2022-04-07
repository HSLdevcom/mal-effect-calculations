# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "links_2040_ve0.rds")) %>%
  dplyr::select(relative_speed) %>%
  dplyr::mutate(wkt = sf::st_as_text(geometry)) %>%
  sf::st_drop_geometry()

results1 <- readr::read_rds(here::here("results", "links_2040_ve1.rds")) %>%
  dplyr::select(relative_speed, volume_aht) %>%
  dplyr::mutate(wkt = sf::st_as_text(geometry)) %>%
  sf::st_drop_geometry()

results <- results0 %>%
  dplyr::full_join(results1, by = "wkt", suffix = c("0", "1")) %>%
  dplyr::rename(geometry = wkt) %>%
  sf::st_as_sf(wkt = "geometry", remove = TRUE, crs = 3879) %>%
  dplyr::mutate(in0 = !is.na(relative_speed0),
                in1 = !is.na(relative_speed1)) %>%
  dplyr::mutate(relative_speed0 = tidyr::replace_na(relative_speed0, 0.0),
                relative_speed1 = tidyr::replace_na(relative_speed1, 0.0)) %>%
  dplyr::mutate(diff_relative_speed = relative_speed1 - relative_speed0,
                diff_rel_relative_speed = diff_relative_speed / relative_speed0) %>%
  dplyr::filter(!is.na(volume_aht)) %>%
  dplyr::filter(volume_aht > 1)


# Absolute difference -----------------------------------------------------

buffers <- results %>%
  sf::st_buffer(dist = -sqrt(results$volume_aht) * 5,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(sf::st_intersects(., sf::st_as_sf(region), sparse = FALSE)) %>%
  dplyr::filter(abs(volume_aht) > 0.01) %>%
  dplyr::filter(in0 & in1) %>%
  dplyr::arrange(abs(diff_relative_speed))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(data = region, color = "#000000", fill = "#21292D", size = 0.46) +
  geom_basemap(show_roads = FALSE) +
  geom_sf(mapping = aes(fill = diff_relative_speed),
          data = buffers, color = NA) +
  scale_fill_fermenter(
    type = "div",
    palette = "PiYG",
    name = "%-yks.",
    breaks = seq(-0.05, 0.05, 0.02),
    limits = c(-0.07, 0.07),
    labels = scales::label_percent(accuracy = 1, suffix = " %"),
    direction = 1,
    oob = scales::oob_squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Muutos aamuhuipputunnin ajonopeuden suhteessa vapaaseen ajonopeteen",
    subtitle = "2040 Vertailupohja \U2192 2040 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_relative-speed_2040_ve0-2040_ve1.png"))
