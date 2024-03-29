library(tidyverse)
library(here)
library(cowplot)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Parameters for plotting ----

pks <-
  c("Helsingin kantakaupunki",
    "Muu Helsinki",
    "Muu pääkaupunkiseutu")

kehys <-
  c("Junaliikenteen kehyskunnat",
    "Bussiliikenteen kehyskunnat"
    )

# Join tours to agents data ----

agents <- agents %>%
  join_purpose_tours(tours, "sustainable_access", "ho")

agents_0 <- agents_0 %>%
  join_purpose_tours(tours_0, "sustainable_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "sustainable_access", "ho")

# Calculate tour access ----

agents <- agents %>%
  # After left_join, if an agent did not make ho tours, nr_tours_ho is NA.
  filter(!is.na(nr_tours_ho)) %>%
  filter(nr_tours_ho > 0) %>%
  mutate(tour_access_ho = sustainable_access_ho / nr_tours_ho)

agents_0 <- agents_0 %>%
  # After left_join, if an agent did not make ho tours, nr_tours_ho is NA.
  filter(!is.na(nr_tours_ho)) %>%
  filter(nr_tours_ho > 0) %>%
  mutate(tour_access_ho = sustainable_access_ho / nr_tours_ho)

agents_1 <- agents_1 %>%
  # After left_join, if an agent did not make ho tours, nr_tours_ho is NA.
  filter(!is.na(nr_tours_ho)) %>%
  filter(nr_tours_ho > 0) %>%
  mutate(tour_access_ho = sustainable_access_ho / nr_tours_ho)

# Group agents tables ----

limit_pks <- agents %>%
  filter(area %in% pks) %>%
  summarise(limit = quantile(tour_access_ho, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

limit_kehys <- agents %>%
  filter(area %in% kehys) %>%
  summarise(limit = quantile(tour_access_ho, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

# Filter persons with no car use ----

calc_low_access <- function(df, pks, limit_pks, limit_kehys) {
  df %>%
    mutate(low_access = if_else(
      area %in% pks,
      tour_access_ho < limit_pks,
      tour_access_ho < limit_kehys
    )) %>%
    group_by(area) %>%
    summarise(nr_agents = n(),
              low_access = sum(low_access, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(share = low_access / nr_agents)
}

low_access <- agents %>%
  filter(!is_car_user) %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("present_name"))

low_access_0 <- agents_0 %>%
  filter(!is_car_user) %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("baseline_name"))

low_access_1 <- agents_1 %>%
  filter(!is_car_user) %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("projected_name"))

# Calc differences ----

results <- bind_rows(low_access, low_access_0, low_access_1) %>%
  mutate(scenario = factor(scenario, levels = c(config::get("present_name"),
                                                config::get("baseline_name"),
                                                config::get("projected_name"))))

results <- results %>%
  mutate(area2 = if_else(area %in% pks, "Pääkaupunkiseutu", "Kehyskunnat"),
         area2 = forcats::as_factor(area2))

# Plot ----

plot_agents_and_shares <- function(area2) {
  p1 <- results %>%
    filter(area2 == {{ area2 }}) %>%
    ggplot(aes(x = area, y = low_access, fill = scenario)) +
    geom_col(position = position_dodge2()) +
    scale_y_continuous(
      labels = scales::label_number()
    ) +
    scale_x_discrete(
      labels = scales::label_wrap(5)
    ) +
    scale_fill_manual(name = NULL, values = hsl_pal("blues")(3)) +
    geom_abline(slope = 0) +
    labs(y = "asukasta",
         x = NULL,
         title = "Saavutettavuusköyhien autottomien\nasukkaiden määrä") +
    theme_mal_graph()

  p2 <- results %>%
    filter(area2 == {{ area2 }}) %>%
    ggplot(aes(x = area, y = share, fill = scenario)) +
    geom_col(position = position_dodge2()) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1, suffix = ""),
      limits = c(0, 0.15)
    ) +
    scale_x_discrete(
      labels = scales::label_wrap(5)
    ) +
    scale_fill_manual(name = NULL, values = hsl_pal("blues")(3)) +
    geom_abline(slope = 0) +
    labs(y = "%",
         x = NULL,
         title = "Saavutettavuusköyhien asukkaiden\nosuus autottomista asukkaista") +
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
ggsave_graph(here::here("figures", config::get("projected_scenario"), "low_access_pks.png"))

plot_agents_and_shares("Kehyskunnat")
ggsave_graph(here::here("figures", config::get("projected_scenario"), "low_access_kehys.png"))
