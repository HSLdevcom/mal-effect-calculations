library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Join tours to agents data ----

join_tours <- function(agents, tours, res_var){
  agents %>%
    left_join(
      tours %>%
        group_by(person_id) %>%
        summarise(across(all_of(res_var), sum, na.rm = TRUE)),
      by = c("id" = "person_id")
    )
}

agents <- agents %>%
  join_tours(tours, "total_access")

agents_0 <- agents_0 %>%
  join_tours(tours_0, "total_access")

agents_1 <- agents_1 %>%
  join_tours(tours_1, "total_access")

# Group agents tables ----

res_var <- c("total_access")
group_var <- c("age_group",
               "gender",
               "area")

group_mean <- function(df){
  df <- df %>%
    group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(res_var), mean, na.rm = TRUE)) %>%
    ungroup()
}

agents <- agents %>%
  group_mean()

agents_0 <- agents_0 %>%
  group_mean()

agents_1 <- agents_1 %>%
  group_mean()

# Join tables ----

agents <- full_join(agents,
                    agents_0,
                    by = group_var,
                    suffix = c("", "0"))

agents <- full_join(agents,
                    agents_1,
                    by = group_var,
                    suffix = c("", "1"))

# Calc differences ----

agents <- agents %>%
  mutate(
    projected = total_access1,
    baseline = total_access0,
    util_dif = projected - baseline
  )

# Plot ----

max_dif <- 3

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
    fill = "Sukupuoli",
    y = "eur / asukas",
    x = "Ikäryhmät",
    title = paste0(
      "Muutos asukkaan tekemien matkojen saavutettavuudessa: ",
      config::get("projected_name")
    )
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_change_pop_group_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
