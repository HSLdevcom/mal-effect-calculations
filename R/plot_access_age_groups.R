library("tidyverse")
library("config")
library("here")

# Read files ----

file_path <- here("results", get("projected_scenario"), "agents.txt")
agents <- read_delim(file_path, delim = "\t", col_names = TRUE) 

# Factorize grouping variable for plotting ----

agents <- agents %>%
  mutate(age_group = forcats::as_factor(age_group),
         age_group = forcats::fct_relevel(age_group, c("age_7-17",
                                                       "age_18-29",
                                                       "age_30-49",
                                                       "age_50-64",
                                                       "age_65-99")))

# Group agents tables ----

res_vars <- c("nr_tours0", "nr_tours1",
              "sustainable_access0", "sustainable_access1",
              "car_access0", "car_access1",
              "total_access0", "total_access1",
              "persons1", "persons0")

agents <- agents %>%
  group_by(age_group, gender) %>%
  summarise_at(res_vars, sum, na.rm = TRUE)

# Calc differences ----

agents <- agents %>%
  mutate(projected = total_access1 / nr_tours1, 
         baseline = total_access0 / nr_tours0,
         util_dif = projected - baseline)  

# Plot ----

results <- agents %>%
  select(age_group, gender, projected, baseline) %>%
  ungroup() %>%
  gather("scenario", "utility", projected, baseline) %>%
  mutate(scenario = case_when(scenario %in% "projected" ~ get("projected_name"),
                              scenario %in% "baseline" ~ get("baseline_name")))
  
results %>%
  ggplot(aes(x = age_group, y = utility, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.8) +
  scale_fill_manual(values = hsl_pal("blues")(2)) +
  facet_wrap(~ gender, nrow = 1) +
  theme_fig +
  labs(fill = "Scenario",
       y = "Expected utility / tour",
       x = NULL,
       title = "Accessibility of tours") +
  ggsave(here("results", get("projected_scenario"), "age_group_access.png"),
         width = dimensions_fig[1], 
         height = dimensions_fig[2], 
         units = "cm")

# Plot ----

max_dif <- max(abs(agents$util_dif)) + 0.1

agents %>%
  ggplot(aes(x = age_group, y = util_dif, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.8) +
  scale_fill_manual(values = hsl_cols("blue", "pink")) +
  theme_fig +
  geom_abline(slope = 0) +
  ylim(-max_dif, max_dif) +
  labs(y = "Expected utility (eur) / tour",
       x = "Age groups",
       title = "Change in average tours' accessibility") +
  ggsave(here("results", get("projected_scenario"), "age_group_access_dif.png"),
         width = dimensions_fig[1], 
         height = dimensions_fig[2], 
         units = "cm")

