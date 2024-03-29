# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = malpakka),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "YlGn",
    name = expression(e[t]),
    labels = scales::label_number(accuracy = 0.1, decimal.mark = ","),
    breaks = c(0.3, 0.6, 1, 2, 4),
    limits = c(0, 5),
    direction = 1,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kestävien kulkutapojen mahdollistama tonttitehokkuus",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_malpakka_%s.png", scenario_attributes[["scenario"]])))
