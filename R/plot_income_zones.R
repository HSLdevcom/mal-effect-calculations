library("tidyverse")
library("config")
library("here")
library("sf")

# Read files ----

file_path <- here("results", get("projected_scenario"), "agents.txt")
agents <- read_delim(file_path, delim = "\t", col_names = TRUE) 
zones <- st_read(here("data", "helmet_zones_map.shp"))

# Parameters for plotting ----

limit_persons <- 100
limit_txt <- paste0("Zones with over ", limit_persons, " persons")
plot_areas <- c("helsinki_cbd", "helsinki_other", "espoo_vant_kau")

# Join agents tables ----

res_vars <- c("persons1", "persons0")

agents <- agents %>%
  group_by(number, income_max) %>%
  summarise_at(res_vars, sum, na.rm = TRUE) %>%
  mutate(share = persons1 / sum(persons1)) %>%
  filter(persons1 > limit_persons) %>%
  arrange(number)

# Filter only 7-17 and combine to shape ----

low_income <- agents %>%
  filter(income_max > 0) %>%
  filter(income_max %in% min(income_max))

zones <- zones %>%
  left_join(low_income, by=c("zone"="number"))

# Plot ----

zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = share)) +
  geom_sf(size = 0.1, color = "gray") +
  scale_fill_gradient(high = hsl_cols("red"),
                       low = hsl_cols("white"),
                       na.value = hsl_cols("lightgray")) +
  labs(fill = "Share of total (%)",
       title = "Percent of low income residents (%)",
       subtitle = limit_txt) +
  theme_maps +
  ggsave(here("results", get("projected_scenario"), "zones_peple_low_income.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")
