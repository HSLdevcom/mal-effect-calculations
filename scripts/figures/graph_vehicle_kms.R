# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(area != "Helsingin seutu")

results_total <- results %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(vehicle_kms_total = sum(vehicle_kms_total))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = vehicle_kms_total)) +
  geom_col(aes(fill = area), position = position_stack()) +
  geom_text(
    aes(label = scales::label_number(scale = 0.000001, accuracy = 0.1, decimal.mark = ",")(vehicle_kms_total), group = area),
    position = position_stack(vjust = 0.5),
    size = points2mm(8)
  ) +
  geom_text(data = results_total,
            aes(label = scales::label_number(scale = 0.000001, accuracy = 0.1, decimal.mark = ",")(vehicle_kms_total)),
            vjust = -0.5,
            size = points2mm(8),
            fontface = "bold") +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.000001)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#BFD7AC", "#f092cd", "#007AC9", "#AAD3ED")
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen suorite",
    x =  NULL,
    y = "milj. ajon.km / arki-vrk"
  ) +
  theme_mal_graph() +
  theme(legend.position = "right")

ggsave_graph(here::here("figures", "graph_vehicle_kms.png"))
