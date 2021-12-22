# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers <- readr::read_tsv(here::here("data", "centers.tsv"), col_types = "icll") %>%
  dplyr::filter(center)

results0 <- readr::read_rds(here::here("results", "centers_2018.rds")) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results1 <- readr::read_rds(here::here("results", "centers_2040_ve0.rds")) %>%
  dplyr::filter(origin %in% centers$label & destination %in% centers$label)

results <- results0 %>%
  dplyr::left_join(results1, by = c("origin", "destination"), suffix = c("0", "1")) %>%
  dplyr::mutate(diff_ttime_ratio_aht = ttime_ratio_aht1 - ttime_ratio_aht0,
                diff_rel_ttime_ratio_aht = diff_ttime_ratio_aht / ttime_ratio_aht0)


# Plot --------------------------------------------------------------------

ggplot(data = results) +
  geom_raster(mapping = aes(x = destination,
                            y = forcats::fct_rev(origin),
                            fill = diff_rel_ttime_ratio_aht)) +
  scale_x_discrete(position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_fill_fermenter(
    name = "%",
    breaks = seq(-0.3+0.1, 0.3-0.1, 0.1),
    limits = c(-0.3, 0.3),
    type = "div",
    palette = "PiYG",
    na.value = "#FFFFFF",
    labels = scales::percent_format(accuracy = 1, suffix = " %")
  ) +
  labs(
    title = "Joukkoliikenteen ja henkilöauton matka-aikojen suhteen suhteellinen muutos",
    subtitle = "2018 Nykytila \U2192 2040 Vertailupohja",
    x = "Määräpaikka",
    y = "Lähtöpaikka"
  ) +
  coord_equal() +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

ggsave_graph(here::here("figures", "graph_ttime-ratio_aht_2018_2040_ve0.png"))
