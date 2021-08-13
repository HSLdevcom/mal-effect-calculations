library(tidyverse)
library(config)
library(here)
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
    "Espoo, Vantaa, Kau")

kehys <-
  c("Kehyskunnat (raide)",
    "Kehyskunnat (muut)"
    )

# Join tours to agents data ----

agents <- agents %>%
  join_purpose_tours(tours, "sustainable_access", "ho")

agents_0 <- agents_0 %>%
  join_purpose_tours(tours_0, "sustainable_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "sustainable_access", "ho")

# Filter persons with no car use ----

agents <- agents %>%
  filter(is_car_user)

agents_0 <- agents_0 %>%
  filter(is_car_user)

agents_1 <- agents_1 %>%
  filter(is_car_user)

# Calculate tour access ----

agents <- agents %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = sustainable_access / nr_tours)

agents_0 <- agents_0 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = sustainable_access / nr_tours)

agents_1 <- agents_1 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = sustainable_access / nr_tours)

# Group agents tables ----

limit_pks <- agents %>%
  filter(area %in% pks) %>%
  summarise(limit = quantile(tour_access, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

limit_kehys <- agents %>%
  filter(area %in% kehys) %>%
  summarise(limit = quantile(tour_access, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

calc_low_access <- function(df, pks, limit_pks, limit_kehys) {
  df %>%
    mutate(low_access = if_else(
      area %in% pks,
      tour_access < limit_pks,
      tour_access < limit_kehys
    )) %>%
    group_by(area) %>%
    summarise(nr_agents = n(),
              low_access = sum(low_access, na.rm = TRUE)) %>%
    mutate(share = low_access / nr_agents) %>%
    ungroup()
}

low_access <- agents %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("present_name"))

low_access_0 <- agents_0 %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("baseline_name"))

low_access_1 <- agents_1 %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("projected_name"))

# Calc differences ----

results <- bind_rows(low_access, low_access_0, low_access_1)

results <- results %>%
  mutate(area2 = if_else(area %in% pks, "Pääkaupunkiseutu", "Kehyskunnat"),
         area2 = forcats::as_factor(area2))

# Plot ----

results %>%
  ggplot(aes(x = area, y = low_access, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area2, nrow = 1, drop = TRUE, scales = "free_x") +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_fig +
  geom_abline(slope = 0) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(fill = "Skenaario",
       y = "Asukkaat",
       x = NULL,
       title = "Autoriippumaton saavutettavuus alle vertailutason",
       subtitle = "Kotiperäiset muut matkat, joilla ei autoa käytössä")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "low_access_no_car.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
