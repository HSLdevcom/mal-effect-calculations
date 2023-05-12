# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

zones <- readr::read_rds(here::here("results", "sijoittelualueet2019.rds"))

# Read all Uusimaa centers plus add several others.
centers <- here::here("data", "Uusimaa-kaava-2050_Pisteet.gpkg") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  dplyr::filter(grepl("^Keskustatoimintojen alue", kuvaus)) %>%
  dplyr::filter(sf::st_intersects(., sf::st_combine(zones), sparse = FALSE)) %>%
  sf::st_join(zones, join = st_within)

centers <- centers %>%
  dplyr::add_row(kohteenNimi = "Riihimäki", kuvaus = "Lisäys MAL 2023 -työhön", SIJ2019 = 23016, hs15 = FALSE, geom = st_sfc(st_point(c(1, 1)), crs = 3879)) %>%
  dplyr::add_row(kohteenNimi = "Lahti", kuvaus = "Lisäys MAL 2023 -työhön", SIJ2019 = 26003, hs15 = FALSE, geom = st_sfc(st_point(c(1, 1)), crs = 3879)) %>%
  dplyr::add_row(kohteenNimi = "Orimattila", kuvaus = "Lisäys MAL 2023 -työhön", SIJ2019 = 25002, hs15 = FALSE, geom = st_sfc(st_point(c(1, 1)), crs = 3879)) %>%
  dplyr::add_row(kohteenNimi = "Pasila", kuvaus = "Lisäys MAL 2023 -työhön", SIJ2019 = 255, hs15 = TRUE, geom = st_sfc(st_point(c(25496273, 6676350)), crs = 3879)) %>%
  dplyr::add_row(kohteenNimi = "Kalasatama", kuvaus = "Lisäys MAL 2023 -työhön", SIJ2019 = 306, hs15 = TRUE, geom = st_sfc(st_point(c(25498797, 6674913)), crs = 3879))

centers %>%
  readr::write_rds(here::here("results", "centers_uusimaa-2050.rds"))
