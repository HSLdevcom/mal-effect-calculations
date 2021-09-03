# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2018.rds")) %>%
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

breaks <- seq(from = -0.09, to = 0.09, by = 0.02)
colors <- c("#7b1154", "#ffffff", "#3E8606")
nbreaks <- length(breaks)
values <- scales::rescale(
  x = seq(from = mean(breaks[c(1, 2)]),
          to = mean(breaks[c(nbreaks - 1, nbreaks)]),
          length.out = length(colors)),
  to = c(0,1),
  from = range(breaks)
)
limits <- range(breaks) + c(-0.0001, 0.0001)
breaks <- breaks[c(-1, -length(breaks))]

label_percent_signed <- function(x) {
  y <- scales::label_percent(accuracy = 1, suffix = "")(x)
  without_sign <- !grepl("-", y)
  y[without_sign] <- paste0("+", y[without_sign])
  return(y)
}

ggplot() +
  geom_sf(mapping = aes(fill = diff_mode_share_sustainable),
          data = results, color = NA) +
  scale_fill_stepsn(
    name = "%-yks.",
    breaks = breaks,
    labels = label_percent_signed,
    limits = limits,
    colors = colors,
    values = values,
    oob = scales::squish
  ) +
  geom_basemap() +
  annotate_map(
    title = "Muutos kestävillä kulkutavoilla tehtyjen kiertomatkojen osuuksissa alueelta alkavista kiertomatkoista",
    subtitle = "2018 Nykytila \U2192 2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_mode-share_sustainable_2020-2040_ve0.png"))
