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
    cols = !c(scenario, area, ends_with(c("lower", "upper"))),
    names_to = "mode",
    names_prefix = "origin_share_"
    ) %>%
  dplyr::filter(mode != "car") %>%
  dplyr::mutate(
    lower = dplyr::case_when(
      mode == "transit" ~ origin_share_transit_lower,
      mode == "bike" ~ origin_share_bike_lower,
      mode == "walk" ~ origin_share_walk_lower,
      mode == "sustainable" ~ origin_share_sustainable_lower,
      TRUE ~ NA_real_
    ),
    upper = dplyr::case_when(
      mode == "transit" ~ origin_share_transit_upper,
      mode == "bike" ~ origin_share_bike_upper,
      mode == "walk" ~ origin_share_walk_upper,
      mode == "sustainable" ~ origin_share_sustainable_upper,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(scenario, area, mode, value, lower, upper)

results_sustainable <- results %>%
  dplyr::filter(mode == "sustainable")

results <- results %>%
  dplyr::mutate(mode = factor(mode, levels = translations$level, labels = translations$label))

results_transit <- results %>%
  dplyr::filter(mode == "Joukkoliikenne")

results_bike <- results %>%
  dplyr::filter(mode == "Pyöräily")

results_walk <- results %>%
  dplyr::filter(mode == "Kävely")


# Plot --------------------------------------------------------------------

ggplot(results_sustainable, aes(x = area, y = value, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = lower, ymax = upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = value / 2,
      label = scales::label_percent(accuracy = 1, suffix = "")(value),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 1),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = hsl_blues_fill
  ) +
  scale_color_manual(
    guide = "none",
    values = mal_color
  ) +
  labs(
    title = "Kestävien kulkutapojen osuus alueelta alkavista kiertomatkoista",
    x = NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_mode-share_sustainable.png"))


# Transit -----------------------------------------------------------------

ggplot(results_transit, aes(x = area, y = value, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = lower, ymax = upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(y = value / 2, label = scales::label_percent(accuracy = 1, suffix = "")(value)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 0.50),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = hsl_teals_fill
  ) +
  labs(
    title = "Joukkoliikenteen osuus alueelta alkavista kiertomatkoista",
    x = NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_mode-share_transit.png"), width = 150, height = 84)


# Bike --------------------------------------------------------------------

ggplot(results_bike, aes(x = area, y = value, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = lower, ymax = upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(y = value / 2, label = scales::label_percent(accuracy = 1, suffix = "")(value)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 0.50),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = hsl_yellows_fill
  ) +
  labs(
    title = "Pyöräilyn osuus alueelta alkavista kiertomatkoista",
    x = NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_mode-share_bike.png"), width = 150, height = 84)


# Walk --------------------------------------------------------------------

ggplot(results_walk, aes(x = area, y = value, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = lower, ymax = upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(y = value / 2, label = scales::label_percent(accuracy = 1, suffix = "")(value)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 0.50),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = hsl_greens_fill
  ) +
  labs(
    title = "Kävelyn osuus alueelta alkavista kiertomatkoista",
    x = NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_mode-share_walk.png"), width = 150, height = 84)
