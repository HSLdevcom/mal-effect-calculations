# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "links_2018.rds")) %>%
  dplyr::select(car_aht) %>%
  dplyr::mutate(wkt = sf::st_as_text(geometry)) %>%
  sf::st_drop_geometry()

results1 <- readr::read_rds(here::here("results", "links_2040_ve0.rds")) %>%
  dplyr::select(car_aht) %>%
  dplyr::mutate(wkt = sf::st_as_text(geometry)) %>%
  sf::st_drop_geometry()

results <- results0 %>%
  dplyr::full_join(results1, by = "wkt", suffix = c("0", "1")) %>%
  dplyr::rename(geometry = wkt) %>%
  sf::st_as_sf(wkt = "geometry", remove = TRUE, crs = 3879) %>%
  dplyr::mutate(in0 = !is.na(volume_aht0),
                in1 = !is.na(volume_aht1)) %>%
  dplyr::mutate(car_aht0 = tidyr::replace_na(car_aht0, 0.0),
                car_aht1 = tidyr::replace_na(car_aht1, 0.0)) %>%
  dplyr::mutate(diff_car_aht = car_aht1 - car_aht0,
                diff_rel_car_aht = diff_car_aht / car_aht0)


# Absolute difference -----------------------------------------------------

buffers <- results %>%
  sf::st_buffer(dist = -abs(results$diff_car_aht) / 3,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(sf::st_intersects(., sf::st_as_sf(region), sparse = FALSE)) %>%
  dplyr::filter(abs(diff_car_aht) > 0.01) %>%
  dplyr::filter(in0 & in1) %>%
  dplyr::arrange(diff_car_aht)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(mapping = aes(fill = diff_car_aht),
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
    title = "Henkilöautoliikenteen liikennemäärän muutos",
    subtitle = "2018 Nykytila \U2192 2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_volumes-car-aht_2018-2040_ve0.png"))
