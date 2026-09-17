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

municipalities <- tibble::tribble(
  ~KUNTANIMI, ~min_zone, ~max_zone,
  "Helsinki", 0, 1999,
  "Espoo", 2000, 3499,
  "Kauniainen", 3500, 3999,
  "Vantaa", 4000, 5999,
  "Kirkkonummi", 6000, 6999,
  "Vihti", 7000, 7999,
  "Nurmijärvi", 8000, 8999,
  "Tuusula", 9000, 9999,
  "Kerava", 10000, 10999,
  "Järvenpää", 11000, 11999,
  "Sipoo", 12000, 12999,
  "Mäntsälä", 13000, 13999,
  "Hyvinkää", 14000, 14999,
  "Pornainen", 15000, 15499,
  "Siuntio", 15500, 15999,
  "Raasepori", 17000, 17499,
  "Hanko", 17500, 17999,
  "Inkoo", 18000, 18499,
  "Karkkila", 18500, 18999,
  "Lohja", 19000, 19999,
  "Porvoo", 20000, 20999,
  "Pukkila", 21000, 21499,
  "Askola", 21500, 21999,
  "Myrskylä", 22000, 22499,
  "Lapinjärvi", 22500, 22999,
  "Loviisa", 23000, 23999,
  "Salo", 24000, 24499,
  "Somero", 24500, 24999,
  "Hämeenlinna", 25000, 25999,
  "Janakkala", 26000, 26499,
  "Hattula", 26500, 26999,
  "Loppi", 27000, 27499,
  "Tammela", 27500, 27999,
  "Riihimäki", 28000, 28999,
  "Hausjärvi", 29000, 29499,
  "Kärkölä", 29500, 29999,
  "Orimattila", 30000, 30499,
  "Hollola", 30500, 30999,
  "Lahti", 31000, 31999
)

zones <- zones %>%
  dplyr::select(-dplyr::any_of("KUNTANIMI")) %>%
  dplyr::left_join(
    municipalities,
    by = dplyr::join_by(SIJ2023 >= min_zone, SIJ2023 <= max_zone)
  ) %>%
  dplyr::select(-min_zone, -max_zone)

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
  dplyr::filter(SIJ2023 < 16000) %>%
  dplyr::mutate(KUNTANIMI = factor(KUNTANIMI, levels = hs15))

# Save only HS15 area
zones %>%
  readr::write_rds(here::here("results", "zones.rds"))
