# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

uml <- here::here("data", "UML keskusalueet", "K_yhdistetty.shp") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  dplyr::select(luokka)

readr::write_rds(uml, file = here::here("results", "uml.rds"))
