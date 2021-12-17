# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2018.rds")) %>%
  dplyr::select(zone, savu_zone) %>%
  dplyr::rename(savu_zone0 = savu_zone)


results1 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, savu_zone) %>%
  dplyr::rename(savu_zone1 = savu_zone)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_savu_zone = (as.integer(savu_zone0) - as.integer(savu_zone1)))


# Plot --------------------------------------------------------------------

label_number_signed <- function(x) {
  y <- scales::label_number(accuracy = 1)(x)
  without_sign <- !grepl("-", y)
  y[without_sign] <- paste0("+", y[without_sign])
  return(y)
}

ggplot() +
  geom_sf(mapping = aes(fill = diff_savu_zone),
          data = results, color = NA) +
  scale_fill_gradient2(
    high = "#3E8606",
    low = "#7b1154",
    name = NULL,
    breaks = -3:3,
    labels = c("3+ tasoa heikompi", "2 tasoa heikompi", "1 taso heikompi", "Ei muutosta", "1 taso saavutettavampi", "2 tasoa saavutettavampi", "3+ tasoa saavutettavampi"),
    limits = c(-3, 3),
    guide = "legend",
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Muutos SAVU-vyöhykkeessä",
    subtitle = "2018 Nykytila \U2192 2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_savu_2020-2040_ve0.png"))
