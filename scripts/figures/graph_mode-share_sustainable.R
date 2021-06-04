# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

translations <- here::here("data", "translations", "modes.tsv") %>%
  readr::read_tsv(col_types = "cc")

results <- here::here("data",
                      "helmet_4.0.4_2018_results",
                      "origins_demand.txt") %>%
  readr::read_tsv(
    col_names = c("zone", "car", "transit", "bike", "walk"),
    col_types = "idddd",
    skip = 1
  ) %>%
  dplyr::select(zone, car, transit, bike, walk) %>%
  tidyr::pivot_longer(cols = -zone, names_to = "mode", values_to = "demand") %>%
  dplyr::mutate(mode = factor(mode, levels = translations$level, labels = translations$label))

zones <- readr::read_rds(here::here("results", "zones.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::full_join(results, by = "zone") %>%
  dplyr::mutate(municipality = factor(KUNTANIMI)) %>%
  dplyr::group_by(municipality, mode) %>%
  dplyr::summarise(demand = sum(demand)) %>%
  dplyr::mutate(value = demand / sum(demand)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(mode != "Henkilöauto")

zones1 <- zones
zones1$scenario <- "2023"
zones2 <- zones
zones2$scenario <- "2040 Pohja"
zones3 <- zones
zones3$scenario <- "2040 Luonnos"

zones <- dplyr::bind_rows(zones1, zones2, zones3) %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))


# Plot --------------------------------------------------------------------

ggplot(zones, aes(x = scenario, y = value)) +
  facet_grid(cols = vars(municipality), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(aes(fill = mode)) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "")
  ) +
  scale_x_discrete(
    expand = expansion(mult = 0.4),
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    values = c("Henkilöauto" = "#f092cd",
               "Joukkoliikenne" = "#00b9e4",
               "Pyöräily" = "#fcb919",
               "Kävely" = "#64be1e",
               "Muu" = "#999999")
  ) +
  labs(
    title = "Kestävien kulkutapojen osuus alueelta alkavista kiertomatkoista",
    x = NULL,
    y = "%"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", colour = "#333333", size = 10),
    plot.title = element_text(colour = "#64BE1E"),
    legend.position = "bottom",
    legend.text = element_text(size = rel(1.0)),
    plot.caption = element_text(size = rel(1.0)),
    strip.placement = "outside",
    panel.grid.major.x = element_blank(),
    #strip.switch.pad.grid = unit(0, "cm"),
    panel.spacing = unit(0, "lines")
  )

ggsave_graph(here::here("figures", "graph_mode-share_sustainable.png"))
