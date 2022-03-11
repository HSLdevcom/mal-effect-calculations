# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  dplyr::select(zone, total_pop) %>%
  dplyr::rename(total_pop0 = total_pop)

results1 <- readr::read_rds(here::here("results", "zones_2040_ve1.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, total_pop) %>%
  dplyr::rename(total_pop1 = total_pop)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_total_pop = total_pop1 - total_pop0,
                diff_rel_total_pop = diff_total_pop / total_pop0)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_rel_total_pop),
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
    title = "Suhteellinen muutos väkiluvussa",
    subtitle = "2040 Vertailupohja \U2192 2040 Luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_rel_total_pop_2040_ve0-2040_ve1.png"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_total_pop),
          data = results, color = NA) +
  scale_fill_distiller(
    name = "asukasta",
    type = "div",
    limits = c(-400, 400),
    oob = scales::oob_squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Absoluuttinen muutos väkiluvussa",
    subtitle = "2040 Vertailupohja \U2192 2040 Luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_total_pop_2040_ve0-2040_ve1.png"))
