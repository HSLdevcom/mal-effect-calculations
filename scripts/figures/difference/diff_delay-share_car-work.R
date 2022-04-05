# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(hub)

results0 <- readr::read_rds(here::here("results", "centers_2040_ve0.rds")) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results1 <- readr::read_rds(here::here("results", "centers_2040_ve1.rds")) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results <- results0 %>%
  dplyr::left_join(results1, by = c("origin", "destination"), suffix = c("0", "1")) %>%
  dplyr::mutate(diff_delay_share_car_work = delay_share_car_work1 - delay_share_car_work0,
                diff_rel_delay_share_car_work = diff_delay_share_car_work / delay_share_car_work0)


# Plot --------------------------------------------------------------------

ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = diff_delay_share_car_work)) +
  scale_x_discrete(position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = "%-yks.",
    palette = "PiYG",
    direction = -1,
    breaks = seq(-0.05, 0.05, 0.02),
    limits = c(-0.25, 0.25),
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = "Ruuhkaviiveen osuuden muutos henkilöautoliikenteen\nmatka-ajasta aamuhuipputuntina",
    subtitle = "2040 Vertailupohja \U2192 2040 1. luonnos",
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

ggsave_graph(here::here("figures", "graph_diff_delay-share_car-work_2040_ve0_2040_ve1.png"))
