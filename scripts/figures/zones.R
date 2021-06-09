# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

read_tsv_helmet <- function(..., comment = "#") {
  readr::read_tsv(..., comment = comment) %>%
    dplyr::rename(zone = X1) %>%
    dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE))
}


# Read data ---------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))

pop <- read_tsv_helmet(here::here("data", "Lahtodata", "2016_zonedata", "2017.pop"), col_types = "iiddddd")
lnd <- read_tsv_helmet(here::here("data", "Lahtodata", "2016_zonedata", "2017.lnd"), col_types = "idd")
edu <- read_tsv_helmet(here::here("data", "Lahtodata", "2016_zonedata", "2017.edu"), col_types = "iiii---")
car <- read_tsv_helmet(here::here("data", "Lahtodata", "2016_zonedata", "2017.car"), col_types = "idd")
wrk <- read_tsv_helmet(here::here("data", "Lahtodata", "2016_zonedata", "2016.wrk"), col_types = "iidddd")
prk <- read_tsv_helmet(here::here("data", "Lahtodata", "2016_zonedata", "2016.prk"), col_types = "iii")

accessibility <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "accessibility.txt"), col_types = "iddddddddddddddddddddddddddddddddddddddddddddddddd")
attraction <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "attraction.txt"), col_types = "iddddddddddddd")
car_density <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "car_density.txt"), col_types = "id")
car_use <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "car_use.txt"), col_types = "id")
generation <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "generation.txt"), col_types = "iddddddddddddd")
impedance_ratio <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "impedance_ratio.txt"), col_types = "idd")
origins_demand <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "origins_demand.txt"), col_types = "idddd")
origins_shares <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "origins_shares.txt"), col_types = "idddd")
savu <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "savu.txt"), col_types = "id")
sustainable_accessibility <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "sustainable_accessibility.txt"), col_types = "iddddddddddd")
workforce_accessibility <- read_tsv_helmet(here::here("data", "helmet_4.0.4_2018_results", "workforce_accessibility.txt"), col_types = "id")


# Join data ---------------------------------------------------------------

# Rename columns to avoid name collisions
pop <- pop %>%
  dplyr::rename(total_pop = total)
wrk <- wrk %>%
  dplyr::rename(total_wrk = total)
workforce_accessibility <- workforce_accessibility %>%
  dplyr::rename(accessibility_wh = wh)

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::left_join(pop, by = "zone") %>%
  dplyr::left_join(lnd, by = "zone") %>%
  dplyr::left_join(edu, by = "zone") %>%
  dplyr::left_join(car, by = "zone") %>%
  dplyr::left_join(wrk, by = "zone") %>%
  dplyr::left_join(prk, by = "zone") %>%
  dplyr::left_join(savu, by = "zone") %>%
  dplyr::left_join(workforce_accessibility, by = "zone") %>%
  dplyr::left_join(car_density, by = "zone")


# Impact assessment columns  ----------------------------------------------

zones <- zones %>%
  dplyr::mutate(capital_region = zone %in% 1:5999)

# MAL 2019 vaikutusten arviointiselostus: "Työpaikkamäärien kohdistuminen
# pääkaupunkiseudulla SAVU-vyöhykkeille I-III ja muualla I-V. Mittarina  näille
# vyöhykkeille sijoittuvien työpaikkojen osuus kaikista työpaikoista (%)."
# https://hslfi.azureedge.net/contentassets/7352e50fa96b4f4c9d017860c4363eaf/liite2_mal_2019_vaikutusten_arviointiselostus_liitteineen.pdf
zones <- zones %>%
  dplyr::mutate(savu_goodness = dplyr::case_when(
    capital_region & savu_zone %in% 1:3 ~ "SAVU hyvä",
    !capital_region & savu_zone %in% 1:5 ~ "SAVU hyvä",
    TRUE ~ "SAVU heikko"
  )) %>%
  dplyr::mutate(savu_goodness = factor(savu_goodness, levels = c("SAVU hyvä", "SAVU heikko")))

# Change unit from "number of cars per 1 person" to "number of cars per 1000
# people".
zones <- zones %>%
  dplyr::mutate(car_density = 1000 * car_density)


# Output ------------------------------------------------------------------

readr::write_rds(zones, file = here::here("results", "zones_2018.rds"))
