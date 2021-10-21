# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2018.rds")) %>%
  dplyr::select(zone, workforce_accessibility) %>%
  dplyr::rename(workforce_accessibility0 = workforce_accessibility)


results1 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, workforce_accessibility) %>%
  dplyr::rename(workforce_accessibility1 = workforce_accessibility)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_workforce_accessibility = workforce_accessibility1 - workforce_accessibility0,
                diff_rel_workforce_accessibility = diff_workforce_accessibility / workforce_accessibility0)


# Plot --------------------------------------------------------------------

breaks <- seq(from = 0.15, to = 0.55, by = 0.05)
colors <- c("#ffffff", "#3E8606")
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

ggplot() +
  geom_sf(mapping = aes(fill = diff_rel_workforce_accessibility),
          data = results, color = NA) +
  scale_fill_stepsn(
    name = "%",
    breaks = breaks,
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = limits,
    colors = colors,
    values = values,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Suhteellinen muutos työvoimasaavutettavuudessa",
    subtitle = "2018 Nykytila \U2192 2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_rel_workforce-accessibility_2020-2040_ve0.png"))