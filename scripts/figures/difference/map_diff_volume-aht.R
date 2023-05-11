# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2018"
scenario1 <- "2040_ve0"

results0 <- readr::read_rds(here::here("results", sprintf("links_%s.rds", scenario0))) %>%
  dplyr::select(volume_aht) %>%
  dplyr::mutate(wkt = sf::st_as_text(geometry)) %>%
  sf::st_drop_geometry()

results1 <- readr::read_rds(here::here("results", sprintf("links_%s.rds", scenario1))) %>%
  dplyr::select(volume_aht) %>%
  dplyr::mutate(wkt = sf::st_as_text(geometry)) %>%
  sf::st_drop_geometry()

results <- results0 %>%
  dplyr::full_join(results1, by = "wkt", suffix = c("0", "1")) %>%
  dplyr::rename(geometry = wkt) %>%
  sf::st_as_sf(wkt = "geometry", remove = TRUE, crs = 3879) %>%
  dplyr::mutate(in0 = !is.na(volume_aht0),
                in1 = !is.na(volume_aht1)) %>%
  dplyr::mutate(volume_aht0 = tidyr::replace_na(volume_aht0, 0.0),
                volume_aht1 = tidyr::replace_na(volume_aht1, 0.0)) %>%
  dplyr::mutate(diff_volume_aht = volume_aht1 - volume_aht0,
                diff_rel_volume_aht = diff_volume_aht / volume_aht0)


# Absolute difference -----------------------------------------------------

buffers <- results %>%
  sf::st_buffer(dist = -abs(results$diff_volume_aht) / 3,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(sf::st_intersects(., sf::st_as_sf(region), sparse = FALSE)) %>%
  dplyr::filter(abs(diff_volume_aht) > 0.01) %>%
  dplyr::filter(in0 & in1) %>%
  dplyr::arrange(diff_volume_aht)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(mapping = aes(fill = diff_volume_aht),
          data = buffers, color = NA) +
  scale_fill_fermenter(
    type = "seq",
    palette = "RdYlGn",
    name = "ajon. / h",
    breaks = seq(-700, 700, by = 200),
    # limits = c(0, 6000),
    # direction = -1,
    # oob = scales::squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Moottoriajoneuvoliikenteen liikennemäärän muutos",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_volumes-aht_%s-%s.png", scenario0, scenario1)))
