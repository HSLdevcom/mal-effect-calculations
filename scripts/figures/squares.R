# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

squares <- here::here("data", "Maankaytto", "MALvrtp210624_y5_jLsa_TAB", "MALvrtp210624_y5_jLsa.TAB") %>%
  sf::read_sf() %>%
  sf::st_set_crs(3067) %>%
  sf::st_transform(3879)

squares2 <- squares %>%
  dplyr::mutate(
    area = dplyr::case_when(
      Lsijalue19 %in% 0:999 ~ "helsinki_cbd",
      Lsijalue19 %in% 1000:1999 ~ "helsinki_other",
      Lsijalue19 %in% 2000:5999 ~ "espoo_vant_kau",
      Lsijalue19 %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~ "surround_train",
      Lsijalue19 %in% c(7000:9999, 12000:12999, 15000:15499) ~ "surround_other",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(Aska40, As40_Rvah, Tp40y_Rvah, Lsijalue19, area) %>%
  dplyr::mutate(
    total_pop = pmax(As40_Rvah, 0.0),
    total_wrk = pmax(Tp40y_Rvah, 0.0),
  ) %>%
  dplyr::rename(kem2_pop = Aska40,
                zone = Lsijalue19) %>%
  dplyr::select(zone, area, total_pop, total_wrk, kem2_pop)

ensi <- readr::read_rds(here::here("results", "ensi.rds"))

squares2 <- squares2 %>%
  sf::st_join(ensi) %>%
  dplyr::mutate(ensi = tidyr::replace_na(ensi, FALSE))

readr::write_rds(squares2, file = here::here("results", "squares.rds"))
