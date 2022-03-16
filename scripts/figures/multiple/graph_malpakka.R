# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "areas_all.rds"))

results_2017 <- results0 %>%
  # Is identical to all scenarios
  dplyr::select(scenario, area, tontin_teho_2017) %>%
  dplyr::filter(scenario == scenario[1]) %>%
  dplyr::mutate(scenario = "2017 Tilasto") %>%
  dplyr::rename(malpakka = tontin_teho_2017)

results <- results0 %>%
  dplyr::select(scenario, area, malpakka) %>%
  dplyr::bind_rows(results_2017) %>%
  dplyr::mutate(scenario = factor(scenario, levels = c("2017 Tilasto", levels(results0$scenario)))) %>%
  dplyr::arrange(scenario)


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = malpakka)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  # geom_errorbar(
  #   mapping = aes(ymin = malpakka_lower, ymax = malpakka_upper),
  #   position =  position_dodge2(width = 0.9, padding = 0.66),
  #   color = "#333333",
  #   size = 0.35
  # ) +
  geom_text(
    aes(y = malpakka / 2, label = scales::label_number(accuracy = 0.1, decimal.mark = ",")(malpakka)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1),
    limits = c(0, 5),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#e0e0e0", "#3E8606", "#7DAD58", "#BFD7AC")
  ) +
  labs(
    title = "Tonttitehokkuus",
    x =  NULL,
    y = NULL
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_malpakka.png"))