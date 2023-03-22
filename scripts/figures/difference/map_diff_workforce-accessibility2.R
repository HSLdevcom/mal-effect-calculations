# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2040_ve0"
scenario1 <- "2040_ve2"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, workforce_accessibility) %>%
  dplyr::rename(workforce_accessibility0 = workforce_accessibility)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, workforce_accessibility) %>%
  dplyr::rename(workforce_accessibility1 = workforce_accessibility)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_workforce_accessibility = workforce_accessibility1 - workforce_accessibility0,
                diff_rel_workforce_accessibility = diff_workforce_accessibility / workforce_accessibility0)


# Plot --------------------------------------------------------------------

breaks <- seq(from = -70000, to = 70000, by = 20000)
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

ggplot() +
  geom_sf(mapping = aes(fill = diff_workforce_accessibility),
          data = results, color = NA) +
  scale_fill_stepsn(
    name = "Henkilöä",
    breaks = breaks,
    labels = scales::label_number(accuracy = 1, suffix = ""),
    limits = limits,
    colors = colors,
    values = values,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Muutos työvoimasaavutettavuudessa",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_workforce-accessibility_%s-%s.png", scenario0, scenario1)))
