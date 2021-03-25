# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

municipalities <- here::here("Basemaps", "input", "SuomenKuntajako_2021_100k") %>%
  sf::read_sf(options = "ENCODING=ISO-8859-1") %>%
  sf::st_set_crs(3067) %>%
  dplyr::mutate(across(where(is.character), ~iconv(.x, from = "ISO-8859-1", to = "UTF-8")))

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

municipalities <- municipalities %>%
  dplyr::filter(namefin %in% hs15) %>%
  sf::st_transform(crs = 3879)

municipalities %>%
  readr::write_rds(here::here("Basemaps", "municipalities.rds"))
