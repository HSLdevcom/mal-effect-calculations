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


# Join data ---------------------------------------------------------------

# Rename columns to avoid name collisions
pop <- pop %>%
  dplyr::rename(total_pop = total)
wrk <- wrk %>%
  dplyr::rename(total_wrk = total)

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::left_join(pop, by = "zone") %>%
  dplyr::left_join(lnd, by = "zone") %>%
  dplyr::left_join(edu, by = "zone") %>%
  dplyr::left_join(car, by = "zone") %>%
  dplyr::left_join(wrk, by = "zone") %>%
  dplyr::left_join(prk, by = "zone")


# Output ------------------------------------------------------------------

readr::write_rds(zones, file = here::here("results", "zones_2018.rds"))
