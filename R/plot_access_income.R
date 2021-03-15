library("tidyverse")
library("config")
library("here")

# Read files ----

file_path <- here("results", get("projected_scenario"), "agents.txt")
agents <- read_delim(file_path, delim = "\t", col_names = TRUE) 

# Group agents tables ----

res_vars <- c("nr_tours0", "nr_tours1",
              "sustainable_access0", "sustainable_access1",
              "car_access0", "car_access1",
              "total_access0", "total_access1",
              "persons1", "persons0")

agents <- agents %>%
  group_by(income_max) %>%
  summarise_at(res_vars, sum, na.rm = TRUE) %>%
  filter(income_max > 0)

agents <- agents %>%
  mutate(income_group = c("low 10 %", rep("", 8), "high 10 %")) 

# Calc differences ----

agents <- agents %>%
  mutate(projected = total_access1 / nr_tours1, 
         baseline = total_access0 / nr_tours0,
         util_dif = projected - baseline)  

# Plot ----

results <- agents %>%
  select(income_max, income_group, projected, baseline) %>%
  ungroup() %>%
  gather("scenario", "utility", projected, baseline) %>%
  mutate(scenario = case_when(scenario %in% "projected" ~ get("projected_name"),
                              scenario %in% "baseline" ~ get("baseline_name")))
  
results %>%
  ggplot(aes(x = as.factor(income_max), y = utility, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.8) +
  scale_fill_manual(values = hsl_pal("blues")(2)) +
  scale_x_discrete(labels = results$income_group) + 
  theme_fig +
  labs(fill = "Scenario",
       y = "Expected utility (eur) / tour",
       x = "Income deciles (eur / month)",
       title = "Accessibility of tours") +
  ggsave(here("results", get("projected_scenario"), "income_decile_access.png"),
         width = dimensions_fig[1], 
         height = dimensions_fig[2], 
         units = "cm")

# Plot ----

max_dif <- max(abs(agents$util_dif)) + 0.1

agents %>%
  ggplot(aes(x = as.factor(income_max), y = util_dif)) +
  geom_bar(stat = "identity", position = "dodge", 
           color = "white", fill = hsl_cols("blue"), width = 0.8) +
  scale_x_discrete(labels = results$income_group) + 
  ylim(-max_dif, max_dif) +
  theme_fig +
  geom_abline(slope = 0) +
  labs(y = "Expected utility (eur) / tour",
       x = "Income deciles (eur / month)",
       title = "Change in average tours' accessibility") +
  ggsave(here("results", get("projected_scenario"), "income_decile_access_dif.png"),
         width = dimensions_fig[1], 
         height = dimensions_fig[2], 
         units = "cm")

