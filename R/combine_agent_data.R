library("tidyverse")
library("config")
library("here")

# Helper functions ----

# Read files wrapper for agent data
read <- function(name){
  read_delim(file.path(get("helmet_data"), get(name), "agents.txt"), 
             delim = "\t", 
             col_names = TRUE) %>%
    select(-"X1")
}

# Grouping wrapper for agent data
group <- function(df, grouping_vars){
  df %>%
    mutate(persons = 1) %>%
    group_by(.dots = grouping_vars) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()
}

# Add deciles for agents
add_inc_group <- function(df, deciles){
  df$income_max <- as.numeric(0) 
  deciles <- c(0, deciles)
  for (i in 1:(length(deciles)-1))
    df <- df %>%
        mutate(income_max = if_else(income > deciles[i] & income < deciles[i+1] ,
                                    deciles[i+1], 
                                    income_max))
  return(df)
}
  
# Load data ----

agents_0 <- read("baseline_scenario")
agents_1 <- read("projected_scenario")

# Calculate income group ----

deciles_present <- agents_0 %>% 
  filter(age_group != "age_7-17") %>%
  summarise(deciles = quantile(income,
                               probs = 1:10/10,
                               names = FALSE),
            deciles = round(deciles, -2)) %>%
  pull()

agents_0 <- agents_0 %>%
  add_inc_group(deciles)

agents_1 <- agents_1 %>%
  add_inc_group(deciles)

# Group data for join ----
grouping_vars <- c("number", "area", "municipality", "age_group", "gender", "income_max")

agents_0 <- agents_0 %>%
  group(grouping_vars)

agents_1 <- agents_1 %>%
  group(grouping_vars)

# Join agents tables ----
agents <- left_join(agents_0, agents_1, 
                    by = grouping_vars, suffix = c("0", "1"))

# Write to file ----
agents %>% 
  write_delim(here("results", get("projected_scenario"), "agents.txt"), 
              delim = "\t", 
              col_names = TRUE) 
