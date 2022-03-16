# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

ensi_ruudut <- sf::read_sf(here::here("data", "MAL2023_Ve1_data_ja_info",
                                      "MAL2023_ensisijaiset_vyöhykkeet_Ve1_ruudut.shp")) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_transform(3879) %>%
  dplyr::mutate(ensi = TRUE) %>%
  dplyr::select(xyind, ensi)

readr::write_rds(ensi_ruudut, file = here::here("results", "ensi_ruudut.rds"))

ensi <- sf::read_sf(here::here("data", "MAL2023_Ve1_data_ja_info",
                                      "MAL2023_ensisijaiset_vyohykkeet_Ve1.shp")) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_transform(3879) %>%
  dplyr::mutate(ensi = TRUE) %>%
  dplyr::select(ensi)

readr::write_rds(ensi, file = here::here("results", "ensi.rds"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap() +
  geom_sf(mapping = aes(),
          data = ensi, color = NA, fill = "#3E8606", alpha = 0.5) +
  coord_sf_mal() +
  annotate_map(
    title = "Ensisijaisesti kehitettävät vyöhykkeet",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_ensi.png"))
