# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))

results_total <- results %>%
  dplyr::filter(area == "Helsingin seutu")

results <- results %>%
  dplyr::filter(area != "Helsingin seutu")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = vehicle_kms_car)) +
  geom_col(aes(fill = area), position = position_stack()) +
  geom_text(
    aes(label = scales::label_number(scale = 0.000001, accuracy = 0.1, decimal.mark = ",")(vehicle_kms_car), group = area),
    position = position_stack(vjust = 0.5),
    size = points2mm(8),
    color = "#333333"
  ) +
  geom_text(data = results_total,
            aes(label = scales::label_number(scale = 0.000001, accuracy = 0.1, decimal.mark = ",")(vehicle_kms_car)),
            vjust = -0.5,
            size = points2mm(8),
            fontface = "bold",
            color = "#333333") +
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
    title = "Henkilöautoliikenteen liikennesuorite Helsingin seudulla",
    x =  NULL,
    y = "milj. ajon.km / arki-vrk"
  ) +
  theme_mal_graph() +
  theme(legend.position = "right")

ggsave_graph(here::here("figures", "graph_vehicle_kms_car.png"))
