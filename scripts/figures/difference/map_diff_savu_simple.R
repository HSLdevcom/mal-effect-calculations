# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2018"
scenario1 <- "2040_ve0u"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, savu_zone) %>%
  dplyr::rename(savu_zone0 = savu_zone)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, savu_zone) %>%
  dplyr::rename(savu_zone1 = savu_zone)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_savu_zone = (as.integer(savu_zone0) - as.integer(savu_zone1))) %>%
  dplyr::mutate(diff_savu_zone = dplyr::case_when(
    diff_savu_zone <= -1 ~ "Heikompi SAVU",
    diff_savu_zone == 0 ~ "Ei muutosta",
    diff_savu_zone >= 1 ~ "Parempi SAVU",
    NA ~ NA_character_
  )) %>%
  dplyr::mutate(diff_savu_zone = factor(diff_savu_zone, levels = c("Parempi SAVU", "Ei muutosta", "Heikompi SAVU")))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_savu_zone),
          data = results, color = NA) +
  scale_fill_brewer(
    palette = "RdGy",
    name = NULL,
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Muutos SAVU-vyöhykkeessä",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_savu_simple_%s-%s.png", scenario0, scenario1)))
