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
  group_by(area) %>%
  summarise(across(all_of(res_vars),
                   sum,
                   na.rm = TRUE)) %>%
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
  select(area, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario) %>%
  mutate(
    utility_dif = utility - mean(utility, na.rm = TRUE),
    scenario = case_when(
      scenario %in% "projected" ~ config::get("projected_name"),
      scenario %in% "baseline" ~ config::get("baseline_name"),
      scenario %in% "present" ~ config::get("present_name")
    )
  )

gap %>%
  ggplot(aes(x = area, y = utility_dif, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_fig +
  geom_abline(slope = 0) +
  labs(fill = "Skenaario",
       y = "eur / kiertomatka",
       x = NULL,
       title = "Saavutettavuusero suhteessa seudun keskiarvoon")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "area_access_gap_areas.png"
  ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
