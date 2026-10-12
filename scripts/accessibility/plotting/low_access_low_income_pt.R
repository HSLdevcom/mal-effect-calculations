library(tidyverse)
library(here)
library(cowplot)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")
source(here("scripts", "accessibility", "translations.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

zones <- readr::read_rds(here("results", "zones.rds")) %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::mutate(
    area = dplyr::case_when(
      zone %in% 0:999 ~ "Helsingin kantakaupunki",
      zone %in% 1000:1999 ~ "Muu Helsinki",
      zone %in% 2000:5999 ~ "Muu pääkaupunkiseutu",
      zone %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~
        "Junaliikenteen kehyskunnat",
      zone %in% c(7000:9999, 12000:12999, 15000:15499) ~
        "Bussiliikenteen kehyskunnat",
      TRUE ~ NA_character_
    )
  )

# Parameters for plotting ----

pks <-
  c("Helsingin kantakaupunki",
    "Muu Helsinki",
    "Muu pääkaupunkiseutu")

kehys <-
  c("Junaliikenteen kehyskunnat",
    "Bussiliikenteen kehyskunnat")

public_transport_mode <- names(levels_modes)[levels_modes == "transit"]

# Calculate public transport logsum access ----

join_public_transport_access <- function(agents, tours) {
  public_transport_tours <- tours %>%
    filter(mode == public_transport_mode, purpose_name == "ho") %>%
    group_by(person_id) %>%
    summarise(
      total_access_pt = sum(total_access, na.rm = TRUE),
      nr_tours_pt = n(),
      .groups = "drop"
    )

  agents %>%
    left_join(public_transport_tours, by = c("id" = "person_id")) %>%
    filter(!is.na(nr_tours_pt), nr_tours_pt > 0) %>%
    mutate(tour_access_pt = total_access_pt / nr_tours_pt)
}

agents <- join_public_transport_access(agents, tours)
agents_0 <- join_public_transport_access(agents_0, tours_0)
agents_1 <- join_public_transport_access(agents_1, tours_1)

# Calculate low-access thresholds from the present scenario ----

limit_pks <- agents %>%
  filter(area %in% pks) %>%
  summarise(limit = quantile(tour_access_pt, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

limit_kehys <- agents %>%
  filter(area %in% kehys) %>%
  summarise(limit = quantile(tour_access_pt, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

calc_low_access <- function(df, pks, limit_pks, limit_kehys,
                            group_vars = "area") {
  df %>%
    filter(income_group == "1") %>%
    mutate(low_access = if_else(
      area %in% pks,
      tour_access_pt < limit_pks,
      tour_access_pt < limit_kehys
    )) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      nr_agents = n(),
      low_access = sum(low_access, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(share = low_access / nr_agents)
}

low_access <- calc_low_access(agents, pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("present_name"))

low_access_0 <- calc_low_access(agents_0, pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("baseline_name"))

low_access_1 <- calc_low_access(agents_1, pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("projected_name"))

low_access_1_by_zone <- calc_low_access(
  agents_1,
  pks,
  limit_pks,
  limit_kehys,
  group_vars = c("number", "area")
)

results <- bind_rows(low_access, low_access_0, low_access_1) %>%
  mutate(scenario = factor(
    scenario,
    levels = c(config::get("present_name"),
               config::get("baseline_name"),
               config::get("projected_name"))
  )) %>%
  mutate(
    area2 = if_else(area %in% pks, "Pääkaupunkiseutu", "Kehyskunnat"),
    area2 = forcats::as_factor(area2)
  )

# Plot ----

plot_agents_and_shares <- function(area2) {
  p1 <- results %>%
    filter(area2 == {{ area2 }}) %>%
    ggplot(aes(x = area, y = low_access, fill = scenario)) +
    geom_col(position = position_dodge2()) +
    scale_y_continuous(labels = scales::label_number()) +
    scale_x_discrete(labels = scales::label_wrap(5)) +
    scale_fill_manual(name = NULL, values = hsl_pal("blues")(3)) +
    geom_abline(slope = 0) +
    labs(
      y = "asukasta",
      x = NULL,
      title = "Joukkoliikenteen logsum-\nsaavutettavuusköyhät pienituloiset"
    ) +
    theme_mal_graph()

  p2 <- results %>%
    filter(area2 == {{ area2 }}) %>%
    ggplot(aes(x = area, y = share, fill = scenario)) +
    geom_col(position = position_dodge2()) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1, suffix = ""),
      limits = c(0, 0.15)
    ) +
    scale_x_discrete(labels = scales::label_wrap(5)) +
    scale_fill_manual(name = NULL, values = hsl_pal("blues")(3)) +
    geom_abline(slope = 0) +
    labs(
      y = "%",
      x = NULL,
      title = "Osuus pienituloisista, joilla\njoukkoliikenteen logsum on heikko"
    ) +
    theme_mal_graph()

  legend_b <- get_legend(
    p1 +
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )

  prow <- plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    align = "vh",
    axis = "l",
    hjust = -1,
    nrow = 1
  )

  plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1)) +
    theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"))
}

plot_agents_and_shares("Pääkaupunkiseutu")
ggsave_graph(
  here("figures", config::get("projected_scenario"), "low_access_income_pt_pks.png")
)

plot_agents_and_shares("Kehyskunnat")
ggsave_graph(
  here("figures", config::get("projected_scenario"), "low_access_income_pt_kehys.png")
)

# Map projected low-access areas ----

zones <- zones %>%
  left_join(
    dplyr::select(low_access_1_by_zone, -area),
    by = c("zone" = "number")
  )

zones_centroids <- sf::st_centroid(zones) %>%
  filter(low_access > 0)

zones_centroids_pks <- zones_centroids %>%
  filter(area %in% pks)

zones_centroids_kehys <- zones_centroids %>%
  filter(area %in% kehys)

ggplot() +
  geom_sf(
    data = sf::st_union(dplyr::filter(zones, area %in% kehys)),
    fill = "#dddddc",
    linewidth = NA
  ) +
  geom_basemap() +
  geom_sf(
    data = zones_centroids_pks,
    mapping = aes(size = low_access),
    colour = hsl_cols("red"),
    alpha = 0.5,
    shape = 16,
    stroke = 0
  ) +
  scale_size(name = "asukasta", range = c(0, 4)) +
  coord_sf_mal() +
  annotate_map(
    title = "Joukkoliikenteen logsum-saavutettavuusköyhät pienituloiset pääkaupunkiseudulla",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map()

ggsave_map(
  here(
    "figures",
    config::get("projected_scenario"),
    "zones_access_poor_low_income_pt_pks.png"
  )
)

ggplot() +
  geom_sf(
    data = sf::st_union(dplyr::filter(zones, area %in% pks)),
    fill = "#dddddc",
    linewidth = NA
  ) +
  geom_basemap() +
  geom_sf(
    data = zones_centroids_kehys,
    mapping = aes(size = low_access),
    colour = hsl_cols("red"),
    alpha = 0.5,
    shape = 16,
    stroke = 0
  ) +
  scale_size(name = "asukasta", range = c(0, 4)) +
  coord_sf_mal() +
  annotate_map(
    title = "Joukkoliikenteen logsum-saavutettavuusköyhät pienituloiset kehyskunnissa",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map()

ggsave_map(
  here(
    "figures",
    config::get("projected_scenario"),
    "zones_access_poor_low_income_pt_kehys.png"
  )
)
