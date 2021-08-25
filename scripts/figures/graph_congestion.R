# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(omxr)

path <- here::here("data", "Tulokset", "2020", "Matrices", "time_aht.omx")
time_aht <- read_helmet_omx(path) %>%
  dplyr::rename_with(~ sprintf("%s_time_aht", .x), !c("origin", "destination"))

path <- here::here("data", "Tulokset", "2020", "Matrices", "time_pt.omx")
time_pt <- read_helmet_omx(path) %>%
  dplyr::rename_with(~ sprintf("%s_time_pt", .x), !c("origin", "destination"))

path <- here::here("data", "Tulokset", "2020", "Matrices", "time_iht.omx")
time_iht <- read_helmet_omx(path) %>%
  dplyr::rename_with(~ sprintf("%s_time_iht", .x), !c("origin", "destination"))

path <- here::here("data", "Tulokset", "2020", "Matrices", "demand_aht.omx")
demand_aht <- read_helmet_omx(path) %>%
  dplyr::rename_with(~ sprintf("%s_demand_aht", .x), !c("origin", "destination"))

path <- here::here("data", "Tulokset", "2020", "Matrices", "demand_pt.omx")
demand_pt <- read_helmet_omx(path) %>%
  dplyr::rename_with(~ sprintf("%s_demand_pt", .x), !c("origin", "destination"))

path <- here::here("data", "Tulokset", "2020", "Matrices", "demand_iht.omx")
demand_iht <- read_helmet_omx(path) %>%
  dplyr::rename_with(~ sprintf("%s_demand_iht", .x), !c("origin", "destination"))

matrices <- time_aht %>%
  dplyr::left_join(time_pt, by = c("origin", "destination")) %>%
  dplyr::left_join(time_iht, by = c("origin", "destination")) %>%
  dplyr::left_join(demand_aht, by = c("origin", "destination")) %>%
  dplyr::left_join(demand_pt, by = c("origin", "destination")) %>%
  dplyr::left_join(demand_iht, by = c("origin", "destination")) %>%
  dplyr::select(origin, destination, starts_with("car_work"), starts_with("transit_work"), starts_with("car_leisure"), starts_with("transit_leisure"), starts_with("trailer_truck"), starts_with("truck"), starts_with("van"))

results <- matrices %>%
  dplyr::mutate(
    car_work_delay_aht = pmax(car_work_time_aht - car_work_time_pt, 0),
    car_work_delay_iht = pmax(car_work_time_iht - car_work_time_pt, 0),
    transit_work_delay_aht = pmax(transit_work_time_aht - transit_work_time_pt, 0),
    transit_work_delay_iht = pmax(transit_work_time_iht - transit_work_time_pt, 0),
    car_leisure_delay_aht = pmax(car_leisure_time_aht - car_leisure_time_pt, 0),
    car_leisure_delay_iht = pmax(car_leisure_time_iht - car_leisure_time_pt, 0),
    transit_leisure_delay_aht = pmax(transit_leisure_time_aht - transit_leisure_time_pt, 0),
    transit_leisure_delay_iht = pmax(transit_leisure_time_iht - transit_leisure_time_pt, 0),
    trailer_truck_delay_aht = pmax(trailer_truck_time_aht - trailer_truck_time_pt, 0),
    trailer_truck_delay_iht = pmax(trailer_truck_time_iht - trailer_truck_time_pt, 0),
    truck_delay_aht = pmax(truck_time_aht - truck_time_pt, 0),
    truck_delay_iht = pmax(truck_time_iht - truck_time_pt, 0),
    van_delay_aht = pmax(van_time_aht - van_time_pt, 0),
    van_delay_iht = pmax(van_time_iht - van_time_pt, 0)
  ) %>%
  dplyr::mutate(
    car_work_delays = car_work_delay_aht * car_work_demand_aht + car_work_delay_iht * car_work_demand_iht,
    transit_work_delays = transit_work_delay_aht * transit_work_demand_aht + transit_work_delay_iht * transit_work_demand_iht,
    car_leisure_delays = car_leisure_delay_aht * car_leisure_demand_aht + car_leisure_delay_iht * car_leisure_demand_iht,
    transit_leisure_delays = transit_leisure_delay_aht * transit_leisure_demand_aht + transit_leisure_delay_iht * transit_leisure_demand_iht,
    trailer_truck_delays = trailer_truck_delay_aht * trailer_truck_demand_aht + trailer_truck_delay_iht * trailer_truck_demand_iht,
    truck_delays = truck_delay_aht * truck_demand_aht + truck_delay_iht * truck_demand_iht,
    van_delays = van_delay_aht * van_demand_aht + van_delay_iht * van_demand_iht
  ) %>%
  dplyr::mutate(
    delays = car_work_delays + transit_work_delays + car_leisure_delays + transit_leisure_delays + 5 * (trailer_truck_delays + truck_delays + van_delays)
  )

areas <- results %>%
  dplyr::filter(origin %in% c(0:15999) & destination %in% c(0:15999)) %>%
  dplyr::group_by(origin) %>%
  dplyr::summarise(
    delays_h = sum(delays) / 60
  ) %>%
  dplyr::mutate(
    area = dplyr::case_when(
      origin %in% 0:999 ~ "helsinki_cbd",
      origin %in% 1000:1999 ~ "helsinki_other",
      origin %in% 2000:5999 ~ "espoo_vant_kau",
      origin %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~ "surround_train",
      origin %in% c(7000:9999, 12000:12999, 15000:15499) ~ "surround_other",
      TRUE ~ NA_character_
    )
    ) %>%
  group_by(area) %>%
  dplyr::summarise(delays_h = sum(delays_h))






