library("tidyverse")
library("config")
library("here")
library("sf")

# Read files ----

file_path <- file.path(get("helmet_data"), get("present_scenario"), "agents.txt")
agents <- read_delim(file_path, delim = "\t")

file_path <- file.path(get("helmet_data"), get("baseline_scenario"), "agents.txt")
agents0 <- read_delim(file_path, delim = "\t")

file_path <- file.path(get("helmet_data"), get("projected_scenario"), "agents.txt")
agents1 <- read_delim(file_path, delim = "\t")

zones <- st_read(here("data", "helmet_zones_map.shp"))

# Parameters for plotting ----

limit_persons <- 30
limit_txt <- paste0("Zones with over ", limit_persons, " persons")
plot_areas <- c("helsinki_cbd", "helsinki_other", "espoo_vant_kau", "surrounding")

# Calculate tour access ----

agents <- agents %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = total_access / nr_tours)

agents0 <- agents0 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = total_access / nr_tours)

agents1 <- agents1 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = total_access / nr_tours)

# Calc differences ----

low_limit <- quantile(agents0$tour_access, probs = 0.05, na.rm = TRUE)[[1]]

calc_low_access <- function(df, name){
  df <- df %>%
    mutate(low_access = (tour_access < low_limit)) %>%
    group_by(number) %>%
    summarise(!!name := sum(low_access, na.rm = TRUE),
              nr_tours = sum(nr_tours, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(nr_tours > 100) %>%
    select(-nr_tours)
}

low_access0 <- agents0 %>%
  calc_low_access("baseline")

low_access1 <- agents1 %>%
  calc_low_access("projected")

# Combine to shapefile ----

zones <- zones %>%
  left_join(low_access0, by = c("zone"="number")) %>%
  left_join(low_access1, by = c("zone"="number"))

zones <- zones %>%
  mutate(change = projected - baseline) %>%
  mutate(change = if_else(projected - baseline > 500, 0, as.numeric(change)))

# Plot accessibility ----

zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = change)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient2(high = hsl_cols("red"),
                       low = hsl_cols("blue"),
                       mid = hsl_cols("white"),
                       na.value = hsl_cols("lightgray")) +
  labs(fill = "Change of people with low accessibility",
       title = paste("Projected:", get("projected_name"),
                     "\nBaseline:", get("baseline_name"),
                     sep = " "),
       subtitle = paste("All tours.", limit_txt)) +
  ggsave(here("results", get("projected_scenario"), "zones_mobility_poor.png"),
         width = dimensions_map[1],
         height = dimensions_map[2],
         units = "cm")

