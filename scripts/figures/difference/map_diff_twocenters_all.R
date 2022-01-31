# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2018.rds")) %>%
  dplyr::select(zone, ttime_twocenters_normal_all) %>%
  dplyr::rename(ttime_twocenters_normal_all0 = ttime_twocenters_normal_all)

results1 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, ttime_twocenters_normal_all) %>%
  dplyr::rename(ttime_twocenters_normal_all1 = ttime_twocenters_normal_all)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_ttime_twocenters_normal_all = ttime_twocenters_normal_all1 - ttime_twocenters_normal_all0,
                diff_rel_ttime_twocenters_normal_all = diff_ttime_twocenters_normal_all / ttime_twocenters_normal_all0)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_ttime_twocenters_normal_all),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "PRGn",
    name = "indeksi",
    direction = -1,
    breaks = seq(-1.75, 1.75, 0.5),
    limits = c(-2.25, 2.25),
    labels = scales::number_format(decimal.mark = ","),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuuden muutos kaikilla kulkutavoilla",
    subtitle = "2018 Nykytila \U2192 2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_twocenters_all_2020-2040_ve0.png"))
