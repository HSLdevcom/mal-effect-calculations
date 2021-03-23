# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

zones <- here::here("data", "aluejaot_2019_SHP", "sijoittelualueet2019.shp") %>%
  sf::read_sf(options = "ENCODING=UTF-8") %>%
  sf::st_set_crs(3879) %>%
  dplyr::mutate(KUNTANIMI = dplyr::case_when(
    KUNTANIMI == "HÃ¤meenlinna" ~ "Hämeenlinna",
    KUNTANIMI == "HausjÃƒÆ’Ã‚Â¤rvi" ~ "Hausjärvi",
    KUNTANIMI == "HyvinkÃƒÆ’Ã‚Â¤ÃƒÆ" ~ "Hyvinkää",
    KUNTANIMI == "JÃƒÆ’Ã‚Â¤rvenpÃƒÆ" ~ "Järvenpää",
    KUNTANIMI == "KÃ¤rkÃ¶lÃ¤" ~ "Kärkölä",
    KUNTANIMI == "LapinjÃƒÆ’Ã‚Â¤rvi" ~ "Lapinjärvi",
    KUNTANIMI == "MÃƒÆ’Ã‚Â¤ntsÃƒÆ" ~ "Mäntsälä",
    KUNTANIMI == "MyrskylÃƒÆ’Ã‚Â¤" ~ "Myrskylä",
    KUNTANIMI == "NurmijÃƒÆ’Ã‚Â¤rvi" ~ "Nurmijärvi",
    KUNTANIMI == "RiihimÃƒÆ’Ã‚Â¤ki" ~ "Riihimäki",
    TRUE ~ KUNTANIMI
  ))

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

zones <- zones %>%
  dplyr::filter(KUNTANIMI %in% hs15) %>%
  dplyr::mutate(KUNTANIMI = factor(KUNTANIMI, levels = hs15))

zones %>%
  readr::write_rds(here::here("results", "zones.rds"))
