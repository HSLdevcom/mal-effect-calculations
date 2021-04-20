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
  group_by(area) %>%
  summarise(across(all_of(res_vars),
                   sum,
                   na.rm = TRUE))

# Calc differences ----

agents <- agents %>%
  mutate(
    projected = total_access1 / persons1,
    baseline = total_access0 / persons0,
    util_dif = projected - baseline
  )

# Plot ----
max_dif <- 3

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
    y = "eur / asukas",
    x = NULL,
    title = paste0(
      "Muutos asukkaan matkojen saavutettavuudessa: ",
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
