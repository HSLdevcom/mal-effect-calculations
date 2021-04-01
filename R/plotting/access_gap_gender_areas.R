library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", get("projected_scenario"), "agents.rds")
agents <- read_rds(file_path)

# Group agents tables ----

res_vars <- c(
  "nr_tours",
  "nr_tours0",
  "nr_tours1",
  "total_access",
  "total_access0",
  "total_access1"
)

agents <- agents %>%
  group_by(gender, area) %>%
  summarise_at(res_vars, sum, na.rm = TRUE) %>%
  ungroup()

# Calc differences ----

agents <- agents %>%
  mutate(
    present = total_access / nr_tours,
    projected = total_access1 / nr_tours1,
    baseline = total_access0 / nr_tours0
  )

# Plot ----

gap <- agents %>%
  select(gender, area, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario, area) %>%
  mutate(
    utility_dif = utility - mean(utility, na.rm = TRUE),
    scenario = case_when(
      scenario %in% "projected" ~ get("projected_name"),
      scenario %in% "baseline" ~ get("baseline_name"),
      scenario %in% "present" ~ get("present_name")
    )
  )

max_gap <- max(abs(gap$utility_dif)) + 1

gap %>%
  ggplot(aes(x = gender, y = utility_dif, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_wide +
  ylim(-max_gap, max_gap) +
  geom_abline(slope = 0) +
  labs(fill = "Scenario",
       y = "Difference in expected utility (eur) / tour",
       x = NULL,
       title = "Difference in accessibility compared to average individual in area")

ggsave(
  here("results",
       get("projected_scenario"),
       "access_gap_gender_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
