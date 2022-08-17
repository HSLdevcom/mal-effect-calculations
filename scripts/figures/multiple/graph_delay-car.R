# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(cowplot)


# Data --------------------------------------------------------------------

present <- "2018"
baseline <- "2040_ve0u"
projected <- "2040_ve2"

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
  dplyr::mutate(diff_delay_share_car_work_01 = delay_share_car_work1 - delay_share_car_work0,
                diff_rel_delay_share_car_work_01 = diff_delay_share_car_work_01 / delay_share_car_work0,
                diff_delay_share_car_work_12 = delay_share_car_work2 - delay_share_car_work1,
                diff_rel_delay_share_car_work_12 = diff_delay_share_car_work_12 / delay_share_car_work1)


# Plot absolute ---------------------------------------------------------

p1 <- ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = delay_share_car_work2)) +
  scale_fill_fermenter(
    name = NULL,
    palette = "Reds",
    direction = 1,
    breaks = seq(0.05, 0.25, 0.05),
    limits = c(0, 0.30),
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = sprintf("Ruuhkaviiveen osuus: %d %s",
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
                            fill = diff_delay_share_car_work_01)) +
  scale_fill_fermenter(
    name = NULL,
    palette = "PiYG",
    direction = -1,
    breaks = c(-0.15, -0.05, -0.01, 0.01, 0.05, 0.15),
    limits = c(-0.20, 0.20),
    labels = scales::percent_format(accuracy = 1, suffix = " %-yks.")
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
                            fill = diff_delay_share_car_work_12)) +
  scale_x_discrete(position = "bottom",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = NULL,
    palette = "PiYG",
    direction = -1,
    breaks = seq(-0.05, 0.05, 0.02),
    limits = c(-0.25, 0.25),
    labels = scales::percent_format(accuracy = 1, suffix = " %-yks.")
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
ggsave_map(here::here("figures", sprintf("graph_car-delay_%s.png", projected)))
