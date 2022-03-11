# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  dplyr::select(zone, total_wrk) %>%
  dplyr::rename(total_wrk0 = total_wrk)

results1 <- readr::read_rds(here::here("results", "zones_2040_ve1.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, total_wrk) %>%
  dplyr::rename(total_wrk1 = total_wrk)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_total_wrk = total_wrk1 - total_wrk0,
                diff_rel_total_wrk = diff_total_wrk / total_wrk0)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_rel_total_wrk),
          data = results, color = NA) +
  scale_fill_distiller(
    name = "%",
    type = "div",
    limits = c(-0.2, 0.2),
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    oob = scales::oob_squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Suhteellinen muutos työpaikoissa",
    subtitle = "2040 Vertailupohja \U2192 2040 Luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_rel_total_wrk_2040_ve0-2040_ve1.png"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_total_wrk),
          data = results, color = NA) +
  scale_fill_distiller(
    name = "työpaikkaa",
    type = "div",
    limits = c(-100, 100),
    oob = scales::oob_squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Absoluuttinen muutos työpaikoissa",
    subtitle = "2040 Vertailupohja \U2192 2040 Luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_total_wrk_2040_ve0-2040_ve1.png"))
