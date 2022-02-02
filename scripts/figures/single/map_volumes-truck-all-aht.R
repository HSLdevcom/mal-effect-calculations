# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("buffers-truck_all_%s.rds", config::get("scenario")))) %>%
  # Arrange for improved plotting
  dplyr::arrange(truck_all_aht)


# Plot --------------------------------------------------------------------

scale_volume <- function(binwidth, height, labels, x0, y0) {

  #             ____
  #        ____|    | h
  #   ____|    |    | h
  #  |____|____|____| h
  #    w    w    w

  n <- length(labels) - 1

  x <- rep(c(0, 1, 1, 0), times = n)
  shift <- rep(1:n, each = 4) - 1
  x <- (x + shift) * binwidth + x0

  y <- rep(c(0, 0, 1, 1), times = n)
  shift <- rep(1:n, each = 4)
  y <- (y * shift) * height + y0

  volume_bars <- data.frame(x = x, y = y, group = rep(1:n, each = 4))

  x <- (1:(n+1) - 0.5) * binwidth + x0
  y <- y0 - 1000

  volume_labels <- data.frame(x = x, y = y, label = labels)

  return(list(
    geom_polygon(mapping = aes(x = x, y = y, group = group),
                 data = volume_bars, fill = "#2a5475"),
    geom_text(mapping = aes(x = x, y = y, label = label),
              data = volume_labels, vjust = 1, hjust = 0.5,
              size = points2mm(10),
              colour = "#333333")
  ))
}

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(#mapping = aes(fill = truck_all_aht),
    data = results, color = NA, fill = "#2a5475") +
  scale_volume(binwidth = 7000, height = 300,
               labels = c("100", "200", "300", "400", "500", "ajon./h"),
               x0 = unname(bbox$xmin),
               y0 = unname(bbox$ymax) - 12000) +
  coord_sf_mal() +
  annotate_map(
    title = "Tavaraliikenteen liikennemäärä aamuhuipputuntina",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_volumes-truck-all-aht_%s.png", config::get("scenario"))))
