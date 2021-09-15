# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

read_tsv_helmet <- function(..., comment = "#") {
  withCallingHandlers({
    readr::read_tsv(..., comment = comment) %>%
      dplyr::rename(zone = X1) %>%
      dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE))
  }, warning = function(w) {
    # Helmet results never include first column name
    if (conditionMessage(w) == "Missing column names filled in: 'X1' [1]") {
      invokeRestart("muffleWarning")
    }
  })
}


# Read data ---------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))

pop <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       config::get("forecast_zonedata")),
             pattern = ".pop$",
             full.names = TRUE),
  col_types = "iiddddd"
)
lnd <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       config::get("forecast_zonedata")),
             pattern = ".lnd$",
             full.names = TRUE),
  col_types = "idd"
)
edu <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       config::get("forecast_zonedata")),
             pattern = ".edu$",
             full.names = TRUE),
  col_types = "iiii"
)
wrk <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       config::get("forecast_zonedata")),
             pattern = ".wrk$",
             full.names = TRUE),
  col_types = "iidddd"
)
prk <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       config::get("forecast_zonedata")),
             pattern = ".prk$",
             full.names = TRUE),
  col_types = "iii"
)

accessibility <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "accessibility.txt"),
  col_types = "idddddddddddddddddddddddddddddddddddddddddddddddddd"
)
attraction <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "attraction.txt"),
  col_types = "idddddddddddd"
)
car_density <- read_tsv_helmet(

  file.path(config::get("helmet_data"),
            config::get("results"),
            "car_density.txt"),
  col_types = "id"
)
car_use <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "car_use.txt"),
  col_types = "id"
)
generation <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "generation.txt"),
  col_types = "idddddddddddd"
)
impedance_ratio <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "impedance_ratio.txt"),
  col_types = "idd"
)
origins_demand <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "origins_demand.txt"),
  col_types = "idddd"
)
origins_shares <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "origins_shares.txt"),
  col_types = "idddd"
)
savu <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "savu.txt"),
  col_types = "id"
)
sustainable_accessibility <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "sustainable_accessibility.txt"),
  col_types = "idddddddddddd"
)
workforce_accessibility <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "workforce_accessibility.txt"),
  col_types = "id"
)


# Join data ---------------------------------------------------------------

# Rename columns to avoid name collisions
pop <- pop %>%
  dplyr::rename(total_pop = total)
wrk <- wrk %>%
  dplyr::rename(total_wrk = total)
workforce_accessibility <- workforce_accessibility %>%
  dplyr::rename(workforce_accessibility = wh)
origins_shares <- origins_shares %>%
  dplyr::rename(mode_share_car = car,
                mode_share_transit = transit,
                mode_share_bike = bike,
                mode_share_walk = walk)

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::left_join(pop, by = "zone") %>%
  dplyr::left_join(lnd, by = "zone") %>%
  dplyr::left_join(edu, by = "zone") %>%
  dplyr::left_join(wrk, by = "zone") %>%
  dplyr::left_join(prk, by = "zone") %>%
  dplyr::left_join(savu, by = "zone") %>%
  dplyr::left_join(workforce_accessibility, by = "zone") %>%
  dplyr::left_join(car_density, by = "zone") %>%
  dplyr::left_join(origins_shares, by = "zone")


# Impact assessment columns  ----------------------------------------------

zones <- zones %>%
  dplyr::mutate(
    area = dplyr::case_when(
      zone %in% 0:999 ~ "helsinki_cbd",
      zone %in% 1000:1999 ~ "helsinki_other",
      zone %in% 2000:5999 ~ "espoo_vant_kau",
      zone %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~ "surround_train",
      zone %in% c(7000:9999, 12000:12999, 15000:15499) ~ "surround_other",
      TRUE ~ NA_character_
    ),
  )

levels <- sort(unique(zones$savu_zone))
labels <- as.character(as.roman(levels))
zones <- zones %>%
  dplyr::mutate(savu_zone = factor(savu_zone, levels = levels, labels = labels))

zones <- zones %>%
  dplyr::mutate(capital_region = zone %in% 1:5999)

# MAL 2019 vaikutusten arviointiselostus: "Työpaikkamäärien kohdistuminen
# pääkaupunkiseudulla SAVU-vyöhykkeille I-III ja muualla I-V. Mittarina  näille
# vyöhykkeille sijoittuvien työpaikkojen osuus kaikista työpaikoista (%)."
# https://hslfi.azureedge.net/contentassets/7352e50fa96b4f4c9d017860c4363eaf/liite2_mal_2019_vaikutusten_arviointiselostus_liitteineen.pdf
zones <- zones %>%
  dplyr::mutate(savu_goodness = dplyr::case_when(
    capital_region & savu_zone %in% c("I", "II", "III") ~ "SAVU hyvä",
    !capital_region & savu_zone %in% c("I", "II", "III", "IV", "V") ~ "SAVU hyvä",
    TRUE ~ "SAVU heikko"
  )) %>%
  dplyr::mutate(savu_goodness = factor(savu_goodness, levels = c("SAVU hyvä", "SAVU heikko")))

# Change unit from "number of cars per 1 person" to "number of cars per 1000
# people".
zones <- zones %>%
  dplyr::mutate(car_density = 1000 * car_density)

zones <- zones %>%
  dplyr::mutate(
    mode_share_sustainable = (mode_share_transit +
                                mode_share_bike +
                                mode_share_walk),
    mode_share_bike_walk = (mode_share_bike +
                              mode_share_walk)
  )


# Output ------------------------------------------------------------------

readr::write_rds(zones, file = here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))
