# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2040_ve0"
scenario1 <- "2040_ve2"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, accessibility_scaled) %>%
  dplyr::rename(accessibility_scaled0 = accessibility_scaled)


results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, accessibility_scaled) %>%
  dplyr::rename(accessibility_scaled1 = accessibility_scaled)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_accessibility_scaled = accessibility_scaled1 - accessibility_scaled0,
                diff_rel_accessibility_scaled = diff_accessibility_scaled / accessibility_scaled0)


# Plot --------------------------------------------------------------------

breaks <- seq(from = -2.25, to = 2.25, by = 0.5)
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
  # Hack to get NA values to legend in ggplot2 v3.3.5
  geom_sf(mapping = aes(color = ""),
          data = head(results, n = 1)) +
  geom_sf(mapping = aes(fill = diff_accessibility_scaled),
          data = results, color = NA) +
  scale_fill_stepsn(
    breaks = breaks,
    labels = scales::label_number(accuracy = 0.01, decimal.mark = ","),
    limits = limits,
    colors = colors,
    values = values,
    na.value = "#636363",
    oob = scales::squish
  ) +
  scale_colour_manual(values = NA) +
  guides(fill = guide_colorbar("indeksi",
                               order = 1,
                               ticks = FALSE),
         colour = guide_legend("Liian\nvähän\nasukkaita",
                               order = 99,
                               override.aes = list(color = "grey35",
                                                   fill = "#636363"))) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Muutos saavutettavuudessa asukkaiden näkökulmasta",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_accessibility_%s-%s.png", scenario0, scenario1)))
