# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

translations <- here::here("utilities", "modes.tsv") %>%
  readr::read_tsv(col_types = "cc")

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::select(scenario, area, starts_with("origin_share_")) %>%
  tidyr::pivot_longer(
    cols = starts_with("origin_share_"),
    names_to = "mode",
    names_prefix = "origin_share_"
    ) %>%
  dplyr::filter(mode != "car") %>%
  dplyr::mutate(mode = factor(mode, levels = translations$level, labels = translations$label))

results_total <- results %>%
  dplyr::group_by(scenario, area) %>%
  dplyr::summarise(value = sum(value), .groups = "drop")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = value)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(fill = "white") +
  geom_col(aes(fill = mode, alpha = forcats::fct_rev(scenario))) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, suffix = "")(value), group = mode),
    position = position_stack(vjust = 0.5),
    size = points2mm(8)
  ) +
  geom_text(data = results_total,
            aes(label = scales::label_percent(accuracy = 1, suffix = "")(value)),
            vjust = -0.5,
            size = points2mm(8),
            fontface = "bold") +
  scale_y_continuous(
    labels = scales::label_percent(suffix = ""),
    limits = c(0, 1)
  ) +
  scale_x_discrete(
    labels = NULL
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Henkilöauto" = "#f092cd",
               "Joukkoliikenne" = "#00b9e4",
               "Pyöräily" = "#fcb919",
               "Kävely" = "#64be1e",
               "Muu" = "#999999")
  ) +
  scale_alpha_discrete(
    name = NULL,
    range = c(0.333, 1),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Kestävien kulkutapojen osuus alueelta alkavista kiertomatkoista",
    x = NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_mode-share_sustainable.png"))
