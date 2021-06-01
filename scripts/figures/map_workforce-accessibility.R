# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))

results <- here::here("data",
                      "helmet_4.0.4_2018_results",
                      "workforce_accessibility.txt") %>%
  readr::read_tsv(
    col_names = c("zone", "value"),
    col_types = "id",
    skip = 1
  )

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::left_join(results, by = "zone")


# Plot --------------------------------------------------------------------

breaks <- quantile(zones$value, probs = seq(0, 1, 0.05), names = FALSE)
colors <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#000000")
nbreaks <- length(breaks)
values <- scales::rescale(
  x = seq(from = mean(breaks[c(1, 2)]),
          to = mean(breaks[c(nbreaks - 1, nbreaks)]),
          length.out = 5),
  to = c(0,1),
  from = range(zones$value)
)

ggplot() +
  geom_sf(mapping = aes(fill = value),
          data = zones, color = NA) +
  scale_fill_stepsn(
    name = "Henkilöä",
    breaks = breaks,
    colors = colors,
    guide = "colourbar",
    values = values
  ) +
  geom_basemap() +
  annotate_map(
    title = "Työvoimasaavutettavuus",
    subtitle = "helmet_4.0.4_2018_results",
    caption = paste(c("Kuntajako: Maanmittauslaitos 2021",
                      "Aineiston nimi: Lisenssin antaja 20XX"),
                    collapse = "\n")
  ) +
  theme_mal_map()

ggsave(
  here::here("figures", "map_workforce-accessibility.png"),
  width = 148,
  height = 169,
  units = "mm",
  dpi = 600
)
