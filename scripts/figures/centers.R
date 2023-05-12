# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Read data ---------------------------------------------------------------

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll")

ttimes_aht <- read_helmet_omx(file.path(config::get("helmet_data"),
                                        scenario_attributes[["results"]],
                                        "Matrices",
                                        "time_aht.omx")) %>%
  dplyr::select(origin, destination, car_work, transit_work) %>%
  dplyr::filter(origin %in% centers$level & destination %in% centers$level) %>%
  dplyr::rename(ttime_car_work_aht = car_work,
                ttime_transit_work_aht = transit_work) %>%
  dplyr::mutate(ttime_ratio_aht = ttime_transit_work_aht / ttime_car_work_aht)

ttimes_pt <- read_helmet_omx(file.path(config::get("helmet_data"),
                                        scenario_attributes[["results"]],
                                        "Matrices",
                                        "time_pt.omx")) %>%
  dplyr::select(origin, destination, car_work, transit_work) %>%
  dplyr::filter(origin %in% centers$level & destination %in% centers$level) %>%
  dplyr::rename(ttime_car_work_pt = car_work,
                ttime_transit_work_pt = transit_work)

ttimes <- dplyr::left_join(ttimes_aht, ttimes_pt, by = c("origin", "destination")) %>%
  dplyr::mutate(
    delay_car_work = ttime_car_work_aht - ttime_car_work_pt,
    delay_share_car_work = delay_car_work / ttime_car_work_aht,
    origin = factor(origin, levels = centers$level, labels = centers$label),
    destination = factor(destination, levels = centers$level, labels = centers$label)
  )


# Output ------------------------------------------------------------------

readr::write_rds(ttimes, file = here::here("results", sprintf("centers_%s.rds", scenario_attributes[["scenario"]])))
