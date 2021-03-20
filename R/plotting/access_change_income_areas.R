library("tidyverse")
library("config")
library("here")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.rds")
agents <- read_rds(file_path)

# Group agents tables ----

res_vars <- c("nr_tours0",
              "nr_tours1",
              "total_access0",
              "total_access1",
              "persons1",
              "persons0")

agents <- agents %>%
  group_by(income_group, area) %>%
  summarise_at(res_vars, sum, na.rm = TRUE) %>%
  filter(!income_group %in% -1)

# Calc differences ----

agents <- agents %>%
  mutate(
    projected = total_access1 / nr_tours1,
    baseline = total_access0 / nr_tours0,
    util_dif = projected - baseline
  )

# Plot ----

income_name <- c("low 10 %", rep("", 8), "high 10 %")
max_dif <- 1

agents %>%
  ggplot(aes(x = income_group, y = util_dif)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    fill = hsl_cols("blue"),
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  theme_wide +
  geom_abline(slope = 0) +
  scale_x_discrete(labels = income_name) +
  ylim(-max_dif, max_dif) +
  labs(
    y = "Difference in expected utility (eur) / tour",
    x = "Income deciles",
    title = paste0(
      "Change in average tours' accessibility: ",
      config::get("projected_name")
    )
  ) +
  ggsave(
    here(
      "results",
      config::get("projected_scenario"),
      "access_change_income_areas.png"
    ),
    width = dimensions_wide[1],
    height = dimensions_wide[2],
    units = "cm"
  )
