# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

read_tsv_helmet <- function(..., comment = "#") {
  readr::read_tsv(..., comment = comment) %>%
    dplyr::rename(area = X1) %>%
    dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE))
}

translations <- here::here("utilities", "vdfs.tsv") %>%
  readr::read_tsv(col_types = "ic")

vdfs <- read_tsv_helmet(
  here::here("data", "helmet_4.0.4_2018_results", "vehicle_kms_vdfs_areas.txt"),
  col_types = "cddddd"
)

# Discard peripheral data, transpose data frame, and calculate total vehicle
# kilometers.
vdfs <- vdfs %>%
  dplyr::filter(!area %in% "peripheral") %>%
  tidyr::pivot_longer(-area, names_to = "vdf", values_to = "vehicle_kms") %>%
  tidyr::pivot_wider(id_cols = vdf, names_from = area, values_from = vehicle_kms) %>%
  dplyr::mutate(total = rowSums(select(., -vdf)))

# Handle vdf names.
vdfs <- vdfs %>%
  dplyr::mutate(
    vdf = factor(vdf, levels = translations$level, labels = translations$label),
    vdf = forcats::fct_collapse(
      vdf,
      `Pääkadut` = c(
        "Pääkadut",
        "Useampikaistaiset pääkadut tasoliittymin valoilla"
      ),
      `Pääväylät eritasoliittymin, maantiet` = c(
        "Maantiet / Useampikaistaiset kaupunkiväylät eritasoliittymin",
        "Moottoritiet"
      )
    ),
    vdf = forcats::fct_rev(vdf)
  )

# Calculate results.
vdfs <- vdfs %>%
  dplyr::group_by(vdf) %>%
  dplyr::summarise(total = sum(total), .groups = "drop") %>%
  dplyr::mutate(share = total / sum(total))

# Placeholder for future scenarios.
vdfs1 <- vdfs
vdfs1$scenario <- "2023"
vdfs2 <- vdfs
vdfs2$scenario <- "2040 Pohja"
vdfs3 <- vdfs
vdfs3$scenario <- "2040 Luonnos"

vdfs <- dplyr::bind_rows(vdfs1, vdfs2, vdfs3) %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))


# Plot --------------------------------------------------------------------

ggplot(vdfs, aes(x = scenario, y = share)) +
  facet_grid(
    cols = vars(vdf),
    switch = "both",
    labeller = labeller(.cols = scales::label_wrap(10))
  ) +
  geom_col(aes(fill = vdf)) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 0.1, suffix = "", decimal.mark = ",")(share)),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = "")
  ) +
  scale_x_discrete(
    expand = expansion(mult = 0.4),
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    values = c("#e6b9b8", "#c0504d", "#632523")
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen kilometrisuorite väylätyypeittäin",
    x = NULL,
    y = "%"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", colour = "#333333", size = 10),
    plot.title = element_text(colour = "#64BE1E"),
    legend.position = "none",
    legend.text = element_text(size = rel(1.0)),
    plot.caption = element_text(size = rel(1.0)),
    strip.placement = "outside",
    panel.grid.major.x = element_blank(),
    #strip.switch.pad.grid = unit(0, "cm"),
    panel.spacing = unit(0, "lines")
  )

ggsave_graph(here::here("figures", "graph_vehicle-kms-vdf.png"))
