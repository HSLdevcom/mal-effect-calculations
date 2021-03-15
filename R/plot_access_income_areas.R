library("tidyverse")
library("config")
library("here")

# Read files ----

file_path <- here("results", get("projected_scenario"), "agents.txt")
agents <- read_delim(file_path, delim = "\t", col_names = TRUE) 

# Factorize grouping variable for plotting ----

agents <- agents %>%
  mutate(area = forcats::as_factor(area),
         area = forcats::fct_relevel(area, c("helsinki_cbd",
                                             "helsinki_other",
                                             "espoo_vant_kau",
                                             "surrounding")))

# Group agents tables ----

res_vars <- c("nr_tours0", "nr_tours1",
              "sustainable_access0", "sustainable_access1",
              "car_access0", "car_access1",
              "total_access0", "total_access1",
              "persons1", "persons0")

agents <- agents %>%
  group_by(income_max, area) %>%
  summarise_at(res_vars, sum, na.rm = TRUE) %>%
  filter(income_max > 0)

income_group <- c("low 10 %", rep("", 8), "high 10 %")

# Calc differences ----

agents <- agents %>%
  mutate(projected = total_access1 / nr_tours1, 
         baseline = total_access0 / nr_tours0,
         util_dif = projected - baseline)  

# Plot ----

max_dif <- max(abs(agents$util_dif)) + 0.1

agents %>%
  ggplot(aes(x = as.factor(income_max), y = util_dif)) +
  geom_bar(stat = "identity", position = "dodge", 
           fill = hsl_cols("blue"), color = "white", width = 0.8) +
  facet_wrap(~ area, nrow = 1) +
  theme_wide +
  geom_abline(slope = 0) +
  scale_x_discrete(labels = income_group) + 
  ylim(-max_dif, max_dif) +
  labs(fill = "Scenario",
       y = "Expected utility (eur) / tour",
       x = "Income deciles",
       title = paste0("Change in utily (min): ", get("projected_name"))) +
  ggsave(here("results", get("projected_scenario"), "income_access_dif_areas.png"),
         width = dimensions_wide[1], 
         height = dimensions_wide[2], 
         units = "cm")
