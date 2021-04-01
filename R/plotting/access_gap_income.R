library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.rds")
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
  group_by(income_group) %>%
  summarise(across(all_of(res_vars),
                   sum,
                   na.rm = TRUE)) %>%
  ungroup()

# Remove when income group not defined ----

agents <- agents %>%
  filter(!income_group %in% -1)

# Calc differences ----

agents <- agents %>%
  mutate(
    present = total_access / nr_tours,
    projected = total_access1 / nr_tours1,
    baseline = total_access0 / nr_tours0
  )

# Plot ----
income_names <- c("low 10 %", rep("", 8), "high 10 %")

# Plot ----

gap <- agents %>%
  select(income_group, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario) %>%
  mutate(
    utility_dif = utility - median(utility, na.rm = TRUE),
    scenario = case_when(
      scenario %in% "projected" ~ config::get("projected_name"),
      scenario %in% "baseline" ~ config::get("baseline_name"),
      scenario %in% "present" ~ config::get("present_name")
    )
  )

gap %>%
  ggplot(aes(x = income_group, y = utility_dif, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  scale_x_discrete(labels = income_names) +
  theme_fig +
  geom_abline(slope = 0) +
  labs(fill = "Scenario",
       y = "Difference in expected utility (eur) / tour",
       x = NULL,
       title = "Difference in accessibility compared to average user")

ggsave(
  here("results",
       config::get("projected_scenario"),
       "access_gap_income.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
