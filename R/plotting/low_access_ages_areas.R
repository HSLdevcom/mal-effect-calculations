library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  file.path(config::get("helmet_data"),
            config::get("present_scenario"),
            "agents.txt")
agents <- read_delim(file_path, delim = "\t")

file_path <-
  file.path(config::get("helmet_data"),
            config::get("baseline_scenario"),
            "agents.txt")
agents0 <- read_delim(file_path, delim = "\t")

file_path <-
  file.path(config::get("helmet_data"),
            config::get("projected_scenario"),
            "agents.txt")
agents1 <- read_delim(file_path, delim = "\t")

# Calculate tour access ----

agents <- agents %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = total_access / nr_tours)

agents0 <- agents0 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = total_access / nr_tours)

agents1 <- agents1 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = total_access / nr_tours)

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

low_access0 <- agents0 %>%
  calc_low_access(config::get("baseline_name"))

low_access1 <- agents1 %>%
  calc_low_access(config::get("projected_name"))

# Calc differences ----

results <- bind_rows(low_access, low_access0, low_access1)

# As factor for plotting ----

results <- results %>%
  mutate(area = forcats::as_factor(area),
         area = forcats::fct_relevel(
           area,
           c("helsinki_cbd",
             "helsinki_other",
             "espoo_vant_kau",
             "surrounding")
         ))

# Translate factors ----

results <- results %>%
  mutate(
    area = forcats::fct_recode(area,!!!levels_areas)
  )

# Plot ----

results %>%
  ggplot(aes(x = area, y = share, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_y_continuous(limits = c(0, 0.2), labels = scales::percent) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_fig +
  labs(fill = "Skenaario",
       y = "Osuus asukkaista",
       x = NULL,
       title = "Saavutettavuus alle vertailutason")

ggsave(
  here("results",
       config::get("projected_scenario"),
       "low_access_area.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
