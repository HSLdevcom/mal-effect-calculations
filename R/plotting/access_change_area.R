library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.rds")
agents <- read_rds(file_path)

# Group agents tables ----

res_vars <- c("nr_tours0", "nr_tours1",
              "total_access0", "total_access1")

agents <- agents %>%
  group_by(area) %>%
  summarise_at(res_vars, sum, na.rm = TRUE)

# Calc differences ----

agents <- agents %>%
  mutate(
    projected = total_access1 / nr_tours1,
    baseline = total_access0 / nr_tours0,
    util_dif = projected - baseline
  )

# Plot ----
max_dif <- 1

agents %>%
  ggplot(aes(x = area, y = util_dif)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    fill = hsl_cols("blue"),
    width = 0.8
  ) +
  ylim(-max_dif, max_dif) +
  theme_fig +
  geom_abline(slope = 0) +
  labs(
    y = "Difference in expected utility (eur) / tour",
    x = NULL,
    title = paste0(
      "Change in average tours' accessibility: ",
      config::get("projected_name")
    )
  )

ggsave(
  here("results",
       config::get("projected_scenario"),
       "access_change_area.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
