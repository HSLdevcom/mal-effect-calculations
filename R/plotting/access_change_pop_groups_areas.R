library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.rds")
agents <- read_rds(file_path)

# Group agents tables ----

res_vars <- c("persons0", "persons1",
              "total_access0", "total_access1")

agents <- agents %>%
  group_by(age_group, gender, area) %>%
  summarise(across(all_of(res_vars),
                   sum,
                   na.rm = TRUE)) %>%
  ungroup()

# Calc differences ----

agents <- agents %>%
  mutate(
    projected = total_access1 / persons1,
    baseline = total_access0 / persons0,
    util_dif = projected - baseline
  )

# Plot ----

max_dif <- 5

agents %>%
  ggplot(aes(x = age_group, y = util_dif, fill = gender)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_fill_manual(values = hsl_cols("blue", "green")) +
  facet_wrap( ~ area, nrow = 1) +
  theme_wide +
  geom_abline(slope = 0) +
  ylim(-max_dif, max_dif) +
  labs(
    fill = "Sex",
    y = "Difference in expected utility (eur) / tour",
    x = "Age groups",
    title = paste0(
      "Change in average tours' accessibility: ",
      config::get("projected_name")
    )
  )

ggsave(
  here("results",
       config::get("projected_scenario"),
       "access_change_pop_group_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
