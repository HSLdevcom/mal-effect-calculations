# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(cowplot)


# Data --------------------------------------------------------------------

present <- "2018"
baseline <- "2040_ve0"
projected <- "2040_ve1"

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(hub)

results0 <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", present))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results1 <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", baseline))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results2 <- readr::read_rds(here::here("results", sprintf("centers_%s.rds", projected))) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results <- results0 %>%
  dplyr::left_join(results1, by = c("origin", "destination"), suffix = c("", "1")) %>%
  dplyr::left_join(results2, by = c("origin", "destination"), suffix = c("0", "2")) %>%
  dplyr::mutate(diff_ttime_car_work_aht_01 = ttime_car_work_aht1 - ttime_car_work_aht0,
                diff_rel_ttime_car_work_aht_01 = diff_ttime_car_work_aht_01 / ttime_car_work_aht0,
                diff_ttime_car_work_aht_12 = ttime_car_work_aht2 - ttime_car_work_aht1,
                diff_rel_ttime_car_work_aht_12 = diff_ttime_car_work_aht_12 / ttime_car_work_aht1)


# Plot absolute ---------------------------------------------------------

p1 <- ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = ttime_car_work_aht2)) +
  scale_fill_fermenter(
    name = NULL,
    palette = "OrRd",
    direction = 1,
    breaks = seq(10, 60, 10),
    limits = c(0, 70),
    labels = scales::number_format(suffix = " min")
  ) +
  labs(
    title = sprintf("Matka-aika henkilöautolla: %d %s",
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
        axis.ticks.x = element_blank())


# Plot present-baseline ---------------------------------------------------

p2 <- ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = diff_rel_ttime_car_work_aht_01)) +
  scale_fill_fermenter(
    name = NULL,
    palette = "PiYG",
    direction = -1,
    breaks = seq(-0.35, 0.35, 0.1),
    limits = c(-0.4, 0.4),
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
        axis.ticks.x = element_blank())


# Plot baseline-projected -------------------------------------------------

p3 <- ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = diff_rel_ttime_car_work_aht_12)) +
  scale_x_discrete(position = "bottom",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = NULL,
    palette = "PiYG",
    direction = -1,
    breaks = seq(-0.07, 0.07, 0.02),
    limits = c(-0.4, 0.4),
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
        panel.grid.major.y = element_blank())


# Join plots --------------------------------------------------------------

plot_grid(p1, p2, p3, ncol = 1, align = 'v', rel_widths = c(1), rel_heights = c(1, 1, 1.62))
ggsave_map(here::here("figures", sprintf("graph_ttime-car_%s.png", projected)))
