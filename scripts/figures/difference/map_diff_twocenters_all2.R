# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2040_ve0"
scenario1 <- "2040_ve2"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, ttime_twocenters_normal_all) %>%
  dplyr::rename(ttime_twocenters_normal_all0 = ttime_twocenters_normal_all)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
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
    breaks = seq(-0.45, 0.45, 0.1),
    limits = c(-0.55, 0.55),
    labels = scales::number_format(decimal.mark = ","),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuuden muutos kaikilla kulkutavoilla",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_twocenters_all_%s-%s.png", scenario0, scenario1)))
