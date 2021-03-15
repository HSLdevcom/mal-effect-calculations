# -*- coding: utf-8-unix -*-
library(tidyverse)

theme_maps <- theme(axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.line = element_blank(),
                    panel.background = element_blank(),
                    panel.border = element_blank(),
                    plot.title = element_text(size = 20),
                    plot.subtitle = element_text(size = 12),
                    legend.title = element_text(size = 15),
                    legend.text = element_text(size = 12),
                    plot.margin = unit(c(1,1,1,1), "cm"))

theme_fig <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 10),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title = element_text(size = 15, hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"))

theme_wide <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "top",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 15, face = "bold"),
        plot.margin = unit(c(1,1,2,1), "cm"))

theme_long <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 15, face = "bold"))

dimensions_fig <- c(15, 8) # cm
dimensions_wide <- c(34, 19) # cm
dimensions_long <- c(15, 16) # cm
dimensions_map <- c(34, 19) # cm
