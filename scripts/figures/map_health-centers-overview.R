# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

# Use the same two groups as health-centers-accessibility.R, including
# rehabilitation facilities in the health and wellbeing group.
health_facilities_overview <- readr::read_rds(
  here::here("results", "health-centers.rds")
) %>%
  dplyr::mutate(
    facility_group = dplyr::case_when(
      .data$purpose_name %in% c("Keskussairaalat", "Muut sairaalat") ~
        "Sairaalat",
      .data$purpose_name %in% c(
        "Terveys- ja hyvinvointilaitokset", "Kuntoutuslaitokset"
      ) ~ "Terveys- ja hyvinvointilaitokset",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(
    !is.na(.data$facility_group),
    .data$status %in% c("Nykyinen", "Suunniteltu")
  ) %>%
  # Repeated buildings at one location need only one marker per group/status.
  # Retain separate existing/planned markers rather than invent a third status.
  dplyr::distinct(.data$facility_group, .data$status, geometry) %>%
  dplyr::arrange(.data$status == "Suunniteltu")

overview_zones <- readr::read_rds(here::here("results", "zones.rds"))
overview_colors <- c(
  "Sairaalat" = "#159E78",
  "Terveys- ja hyvinvointilaitokset" = "#E6AB02"
)

health_centers_overview_map <- ggplot() +
  geom_sf(
    data = overview_zones,
    fill = "#F3F1E8", color = "#D0CCC0", linewidth = 0.12
  ) +
  geom_basemap(show_roads = TRUE) +
  geom_sf(
    data = health_facilities_overview,
    aes(color = facility_group, shape = status),
    size = 1.8, stroke = 0.2
  ) +
  scale_color_manual(
    values = overview_colors,
    breaks = names(overview_colors),
    name = "Palvelutyyppi"
  ) +
  scale_shape_manual(
    values = c("Nykyinen" = 16, "Suunniteltu" = 17),
    breaks = c("Nykyinen", "Suunniteltu"),
    name = "Tila"
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(shape = 16)),
    shape = guide_legend(order = 2, override.aes = list(color = "#000000"))
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Helsingin seudun terveys- ja hyvinvointilaitokset",
    subtitle = "Nykyiset ja suunnitellut kohteet sijoittelualueilla"
  ) +
  theme_mal_map() +
  theme(
    legend.position = c(0, 0.90),
    legend.box = "vertical",
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key.height = unit(0.75, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )

# Additional portrait overview; the numbered widescreen map is saved separately.
ggsave_map(
  here::here("figures", "map_health-centers-overview.png"),
  plot = health_centers_overview_map,
  bg = "white"
)
