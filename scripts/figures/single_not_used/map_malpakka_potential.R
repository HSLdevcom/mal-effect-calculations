# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  # Hack to get NA values to legend in ggplot2 v3.3.5
  geom_sf(mapping = aes(color = ""),
          data = head(results, n = 1)) +
  geom_sf(mapping = aes(fill = malpakka_potential),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "YlOrBr",
    name = NULL,
    labels = scales::label_number(accuracy = 0.1, decimal.mark = ","),
    breaks = c(0.3, 0.6, 1, 2, 4),
    limits = c(0, 5),
    direction = 1,
    na.value = "#636363",
    oob = scales::oob_squish
  ) +
  scale_colour_manual(values = NA) +
  guides(fill = guide_colorsteps(order = 1),
         colour = guide_legend("Puuttuva tieto",
                               order = 99,
                               override.aes = list(color = "grey35",
                                                   fill = "#636363"))) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Tonttitehokkuuspotentiaali",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_malpakka_potential_%s.png", scenario_attributes[["scenario"]])))
