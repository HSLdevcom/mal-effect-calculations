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

areas <-
  c("Helsingin kantakaupunki",
    "Muu Helsinki",
    "Espoo, Vantaa, Kau")

# Join tours to agents data ----

agents <- agents %>%
  join_purpose_tours(tours, "sustainable_access", "ho")

agents_0 <- agents_0 %>%
  join_purpose_tours(tours_0, "sustainable_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "sustainable_access", "ho")

# Filter persons with no car use ----

agents <- agents %>%
  filter(is_car_user) %>%
  filter(area %in% areas)

agents_0 <- agents_0 %>%
  filter(is_car_user) %>%
  filter(area %in% areas)

agents_1 <- agents_1 %>%
  filter(is_car_user) %>%
  filter(area %in% areas)

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

low_limit <-
  quantile(agents$tour_access, probs = 0.05, na.rm = TRUE)[[1]]

calc_low_access <- function(df, name) {
  df %>%
    mutate(low_access = (tour_access < low_limit)) %>%
    group_by(area) %>%
    summarise(total = n(),
              low_access = sum(low_access, na.rm = TRUE)) %>%
    mutate(share = low_access / total,
           scenario = name) %>%
    ungroup()
}

low_access <- agents %>%
  calc_low_access(config::get("present_name"))

low_access_0 <- agents_0 %>%
  calc_low_access(config::get("baseline_name"))

low_access_1 <- agents_1 %>%
  calc_low_access(config::get("projected_name"))

# Calc differences ----

results <- bind_rows(low_access, low_access_0, low_access_1)

# Plot ----

results %>%
  ggplot(aes(x = area, y = share, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_fig +
  labs(fill = "Skenaario",
       y = "Osuus asukkaista",
       x = NULL,
       title = "Autoriippumaton saavutettavuus alle vertailutason",
       subtitle = "Asukkaat joilla ei autoa käytössä kiertomatkoille")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "low_access_no_car.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
