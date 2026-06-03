# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

zones <- here::here("data", "aluejaot_2023_SHP", "sijoittelualueet2023.shp") %>%
  sf::read_sf(options = "ENCODING=UTF-8") %>%
  sf::st_set_crs(3879) %>%
  sf::st_zm(drop = TRUE)


zones <- zones %>%
  mutate(SIJ2023 = as.integer(SIJ2023))

hs15 <- c("Helsinki",
          "Espoo",
          "Kauniainen",
          "Vantaa",
          "Hyvinkää",
          "Järvenpää",
          "Kerava",
          "Kirkkonummi",
          "Mäntsälä",
          "Nurmijärvi",
          "Pornainen",
          "Sipoo",
          "Tuusula",
          "Vihti",
          "Siuntio")

# Save whole model area
zones %>%
  dplyr::mutate(hs15 = SIJ2023 < 16000) %>%
  readr::write_rds(here::here("results", "sijoittelualueet2023.rds"))

zones <- zones %>%
  dplyr::filter(SIJ2023 < 16000)
#  dplyr::mutate(KUNTANIMI = factor(KUNTANIMI, levels = hs15))

# Save only HS15 area
zones %>%
  readr::write_rds(here::here("results", "zones.rds"))
