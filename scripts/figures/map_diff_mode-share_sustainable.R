# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2020.rds")) %>%
  dplyr::select(zone, mode_share_sustainable) %>%
  dplyr::rename(mode_share_sustainable0 = mode_share_sustainable)


results1 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, mode_share_sustainable) %>%
  dplyr::rename(mode_share_sustainable1 = mode_share_sustainable)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_mode_share_sustainable = mode_share_sustainable1 - mode_share_sustainable0)


# Plot --------------------------------------------------------------------

label_first_last <- function(x) {
  y <- scales::label_percent(accuracy = 1, suffix = "")(x)
  n <- length(y)
  y[1] <- sprintf("Alle %s", y[1])
  y[n] <- sprintf("Yli %s", y[n])
  return(y)
}

ggplot() +
  geom_sf(mapping = aes(fill = diff_mode_share_sustainable),
          data = results, color = NA) +
  scale_fill_gradient2(
    name = "%-yksikköä",
    limits = c(-0.10, 0.10),
    labels = label_first_last,
    low = "#f092cd",
    high = "#3E8606",
    mid = "#ffffff",
    guide = "colourbar",
    oob = scales::squish
  ) +
  geom_basemap() +
  annotate_map(
    title = "Muutos kestävillä kulkutavoilla tehtyjen kiertomatkojen osuuksissa alueelta alkavista kiertomatkoista",
    subtitle = "2020 Nykytila \U2192 2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_mode-share_sustainable_2020-2040_ve0.png"))

