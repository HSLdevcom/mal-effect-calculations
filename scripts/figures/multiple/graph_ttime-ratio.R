# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(cowplot)


# Data --------------------------------------------------------------------

present <- "2018"
baseline <- "2040_ve0"
projected <- "2040_ve1"

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(center)

results0 <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", present))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results1 <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", baseline))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results2 <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", projected))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results <- results0 %>%
  dplyr::left_join(results1, by = c("origin", "destination"), suffix = c("", "1")) %>%
  dplyr::left_join(results2, by = c("origin", "destination"), suffix = c("0", "2")) %>%
  dplyr::mutate(diff_ttime_ratio_aht_01 = ttime_ratio_aht1 - ttime_ratio_aht0,
                diff_rel_ttime_ratio_aht_01 = diff_ttime_ratio_aht_01 / ttime_ratio_aht0,
                diff_ttime_ratio_aht_12 = ttime_ratio_aht2 - ttime_ratio_aht1,
                diff_rel_ttime_ratio_aht_12 = diff_ttime_ratio_aht_12 / ttime_ratio_aht1)


# Plot absolute ---------------------------------------------------------

p1 <- ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = ttime_ratio_aht2)) +
  scale_fill_fermenter(
    name = NULL,
    breaks = c(0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5),
    limits = c(0, 4.0),
    type = "div",
    palette = "RdBu",
    na.value = "#FFFFFF",
    labels = scales::label_number(accuracy = 0.1, decimal.mark = ",")
  ) +
  labs(
    title = sprintf("Matka-aikojen suhde: %d %s",
                    scenarios$year[scenarios$scenario == projected],
                    scenarios$name[scenarios$scenario == projected]),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6))


# Plot present-baseline ---------------------------------------------------

p2 <- ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = diff_rel_ttime_ratio_aht_01)) +
  scale_fill_fermenter(
    name = NULL,
    breaks = c(-0.3, -0.2, -0.1, -0.02, 0.02, 0.1, 0.2, 0.3),
    limits = c(-0.4, 0.4),
    type = "div",
    palette = "PiYG",
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = sprintf("Muutos: %d %s \U2192 %d %s",
                    scenarios$year[scenarios$scenario == present],
                    scenarios$name[scenarios$scenario == present],
                    scenarios$year[scenarios$scenario == baseline],
                    scenarios$name[scenarios$scenario == baseline]),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6))


# Plot baseline-projected -------------------------------------------------

p3 <- ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = diff_rel_ttime_ratio_aht_12)) +
  scale_x_discrete(position = "bottom",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = NULL,
    breaks = c(-0.2, -0.15, -0.05, -0.01, 0.01, 0.05, 0.15, 0.2),
    limits = c(-0.3, 0.3),
    type = "div",
    palette = "PiYG",
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = sprintf("Muutos: %d %s \U2192 %d %s",
                    scenarios$year[scenarios$scenario == baseline],
                    scenarios$name[scenarios$scenario == baseline],
                    scenarios$year[scenarios$scenario == projected],
                    scenarios$name[scenarios$scenario == projected]),
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))


# Join plots --------------------------------------------------------------

plot_grid(p1, p2, p3, ncol = 1, align = 'v', rel_widths = c(1), rel_heights = c(1, 1, 1.48))
ggsave_map(here::here("figures", sprintf("graph_ttime-ratio_%s.png", projected)))
