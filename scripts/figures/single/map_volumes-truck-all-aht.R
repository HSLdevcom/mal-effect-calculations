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

w = 7000
h = 300
startx = unname(bbox$xmin)
starty = unname(bbox$ymax) - 12000

#             ____
#        ____|    | h
#   ____|    |    | h
#  |____|____|____| h
#    w    w    w

x0 <- c(0, 1, 1, 0)
x <- c(x0 + 0, x0 + 1, x0 + 2, x0 + 3, x0 + 4) * w + startx

y0 <- c(0, 0, 1, 1)
y <- c(y0 * 1, y0 * 2, y0 * 3, y0 * 4, y0 * 5) * h + starty

pols <- data.frame(
  x = x,
  y = y,
  group = rep(c(1, 2, 3, 4, 5), each = 4))

x <- (c(0, 1, 2, 3, 4, 5) + 0.5) * w + startx
y <- starty - 1000

texts <- data.frame(
  x = x,
  y = y,
  label = c("100", "200", "300", "400", "500", "ajon./h")
)

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(#mapping = aes(fill = truck_all_aht),
          data = results, color = NA, fill = "#2a5475", alpha = 0.5) +
  geom_polygon(mapping = aes(x = x, y = y, group = group),
               data = pols, fill = "#2a5475") +
  geom_text(mapping = aes(x = x, y = y, label = label),
            data = texts, vjust = 1, hjust = 0.5,
            size = points2mm(10),
            colour = "#333333") +
  # scale_fill_fermenter(
  #   name = "ajon./h",
  #   palette = "GnBu",
  #   direction = 1,
  #   limits = c(0, 600),
  #   breaks = c(50, 150, 250, 350, 450, 550),
  #   oob = scales::squish,
  #   guide = guide_none()
  # ) +
  coord_sf_mal() +
  annotate_map(
    title = "Tavaraliikenteen liikennemäärä aamuhuipputuntina",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_volumes-truck-all-aht_%s.png", config::get("scenario"))))
