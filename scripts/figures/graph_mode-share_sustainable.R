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


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = value)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(fill = "white") +
  geom_col(aes(fill = mode, alpha = forcats::fct_rev(scenario))) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "")
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
