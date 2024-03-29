# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2040_ve0"
scenario1 <- "2040_suunnitelma"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, mode_share_walk) %>%
  dplyr::rename(mode_share_walk0 = mode_share_walk)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, mode_share_walk) %>%
  dplyr::rename(mode_share_walk1 = mode_share_walk)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_mode_share_walk = mode_share_walk1 - mode_share_walk0)


# Plot --------------------------------------------------------------------

breaks <- seq(from = -0.045, to = 0.045, by = 0.01)
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
  y <- scales::label_percent(accuracy = 0.1, suffix = "", decimal.mark = ",")(x)
  without_sign <- !grepl("-", y)
  y[without_sign] <- paste0("+", y[without_sign])
  return(y)
}

ggplot() +
  geom_sf(mapping = aes(fill = diff_mode_share_walk),
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
  coord_sf_mal() +
  annotate_map(
    title = "Muutos kävellen tehtyjen kiertomatkojen osuuksissa alueelta alkavista kiertomatkoista",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_mode-share_walk_%s-%s.png", scenario0, scenario1)))
