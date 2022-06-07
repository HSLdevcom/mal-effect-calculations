# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Read data ---------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))

land_area <- readxl::read_xlsx(here::here("data", "maapinta_ala_ja_asutut_ruudut.xlsx"),
                               sheet = "pinta_alat")

tontin_teho <- readxl::read_xlsx(here::here("data", "Data_analyysiin_22102021.xlsx"),
                               sheet = "Taul1")

pop <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       scenario_attributes[["forecast"]]),
             pattern = ".pop$",
             full.names = TRUE),
  col_types = "iiddddd",
  first_col_name = "zone"
)
lnd <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       scenario_attributes[["forecast"]]),
             pattern = ".lnd$",
             full.names = TRUE),
  col_types = "idd",
  first_col_name = "zone"
)
edu <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       scenario_attributes[["forecast"]]),
             pattern = ".edu$",
             full.names = TRUE),
  col_types = "iiii",
  first_col_name = "zone"
)
wrk <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       scenario_attributes[["forecast"]]),
             pattern = ".wrk$",
             full.names = TRUE),
  col_types = "iidddd",
  first_col_name = "zone"
)
prk <- read_tsv_helmet(
  list.files(file.path(config::get("forecast_zonedata_path"),
                       scenario_attributes[["forecast"]]),
             pattern = ".prk$",
             full.names = TRUE),
  col_types = "iii",
  first_col_name = "zone"
)

accessibility <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "accessibility.txt"),
  col_types = "idddddddddddddddddddddddddddddddddddddddddddddddddd",
  first_col_name = "zone"
)
attraction <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "attraction.txt"),
  col_types = "idddddddddddd",
  first_col_name = "zone"
)
car_density <- read_tsv_helmet(

  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "car_density.txt"),
  col_types = "id",
  first_col_name = "zone"
)
car_use <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "car_use.txt"),
  col_types = "id",
  first_col_name = "zone"
)
generation <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "generation.txt"),
  col_types = "iddddddddddd",
  first_col_name = "zone"
)
impedance_ratio <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "impedance_ratio.txt"),
  col_types = "idd",
  first_col_name = "zone"
)
origins_demand <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "origins_demand.txt"),
  col_types = "idddd",
  first_col_name = "zone"
)
origins_shares <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "origins_shares.txt"),
  col_types = "idddd",
  first_col_name = "zone"
)
savu <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "savu.txt"),
  col_types = "id",
  first_col_name = "zone"
)
sustainable_accessibility <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "sustainable_accessibility.txt"),
  col_types = "idddddddddddd",
  first_col_name = "zone"
)
workplace_accessibility <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            scenario_attributes[["results"]],
            "workplace_accessibility.txt"),
  col_types = "idd",
  first_col_name = "zone"
)

centers <- readr::read_rds(here::here("results", "centers_uusimaa-2050.rds"))

ttimes_pt <- read_helmet_omx(file.path(config::get("helmet_data"),
                                       scenario_attributes[["results"]],
                                       "Matrices",
                                       "time_pt.omx"))

# TODO: Maybe fix scenarios$scenario[...
if (scenario_attributes[["projected"]]) {
  cba <- read_tsv_helmet(
    file.path(config::get("helmet_data"),
              scenario_attributes[["results"]],
              sprintf("cba_%s_%s.txt", scenario_attributes[["scenario"]], scenarios$scenario[scenarios$baseline])),
    col_types = "iddddddddddddddddddddddddddddd",
    first_col_name = "zone")
}


# Join data ---------------------------------------------------------------

# Rename columns to avoid name collisions
pop <- pop %>%
  dplyr::rename(total_pop = total)
wrk <- wrk %>%
  dplyr::rename(total_wrk = total)
accessibility <- accessibility %>%
  dplyr::rename(accessibility = all) %>%
  dplyr::select(zone, accessibility)
sustainable_accessibility <- sustainable_accessibility %>%
  dplyr::rename(sustainable_accessibility = all) %>%
  dplyr::select(zone, sustainable_accessibility)
workplace_accessibility <- workplace_accessibility %>%
  dplyr::rename(workplace_accessibility = hw,
                workforce_accessibility = wh)
origins_shares <- origins_shares %>%
  dplyr::rename(mode_share_car = car,
                mode_share_transit = transit,
                mode_share_bike = bike,
                mode_share_walk = walk)
origins_demand <- origins_demand %>%
  dplyr::rename(demand_car = car,
                demand_transit = transit,
                demand_bike = bike,
                demand_walk = walk)
if (scenario_attributes[["projected"]]) {
  cba <- cba %>%
  dplyr::rename_with(~ sprintf("cba_%s", .x), -zone)
}

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::left_join(pop, by = "zone") %>%
  dplyr::left_join(lnd, by = "zone") %>%
  dplyr::left_join(edu, by = "zone") %>%
  dplyr::left_join(wrk, by = "zone") %>%
  dplyr::left_join(prk, by = "zone") %>%
  dplyr::left_join(savu, by = "zone") %>%
  dplyr::left_join(accessibility, by = "zone") %>%
  dplyr::left_join(sustainable_accessibility, by = "zone") %>%
  dplyr::left_join(workplace_accessibility, by = "zone") %>%
  dplyr::left_join(car_density, by = "zone") %>%
  dplyr::left_join(origins_shares, by = "zone") %>%
  dplyr::left_join(origins_demand, by = "zone")

if (scenario_attributes[["projected"]]) {
  zones <- zones %>%
  dplyr::left_join(cba, by = "zone")
}


# Impact assessment columns  ----------------------------------------------

zones <- zones %>%
  dplyr::mutate(
    area = dplyr::case_when(
      zone %in% 0:999 ~ "helsinki_cbd",
      zone %in% 1000:1999 ~ "helsinki_other",
      zone %in% 2000:5999 ~ "espoo_vant_kau",
      zone %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~ "surround_train",
      zone %in% c(7000:9999, 12000:12999, 15000:15499) ~ "surround_other",
      TRUE ~ NA_character_
    ),
  )

levels <- sort(unique(zones$savu_zone))
labels <- as.character(as.roman(levels))
zones <- zones %>%
  dplyr::mutate(savu_zone = factor(savu_zone, levels = levels, labels = labels))

zones <- zones %>%
  dplyr::mutate(capital_region = zone %in% 1:5999)

# MAL 2019 vaikutusten arviointiselostus: "Työpaikkamäärien kohdistuminen
# pääkaupunkiseudulla SAVU-vyöhykkeille I-III ja muualla I-V. Mittarina  näille
# vyöhykkeille sijoittuvien työpaikkojen osuus kaikista työpaikoista (%)."
# https://hslfi.azureedge.net/contentassets/7352e50fa96b4f4c9d017860c4363eaf/liite2_mal_2019_vaikutusten_arviointiselostus_liitteineen.pdf
zones <- zones %>%
  dplyr::mutate(savu_goodness = dplyr::case_when(
    capital_region & savu_zone %in% c("I", "II", "III") ~ "SAVU hyvä",
    !capital_region & savu_zone %in% c("I", "II", "III", "IV", "V") ~ "SAVU hyvä",
    TRUE ~ "SAVU heikko"
  )) %>%
  dplyr::mutate(savu_goodness = factor(savu_goodness, levels = c("SAVU hyvä", "SAVU heikko")))

# Change unit from "number of cars per 1 person" to "number of cars per 1000
# people".
zones <- zones %>%
  dplyr::mutate(car_density = 1000 * car_density)

# Scale from 0 to 100 using 2018 ranges
if (scenario_attributes[["present"]]) {
  xrange <- range(zones$workplace_accessibility[zones$zone != 1531])
  readr::write_rds(xrange, file = here::here("results", "workplace-accessibility_range.rds"))
} else {
  message("workplace_accessibility: read ranges...")
  xrange <- readr::read_rds(here::here("results", "workplace-accessibility_range.rds"))
}
zones$workplace_accessibility_scaled = scale_to_range(
  zones$workplace_accessibility, xmin = xrange[1], xmax = xrange[2], a = 0, b = 100)

# Change sign and scale from 0 to 100 using 2018 ranges
zones <- zones %>%
  dplyr::mutate(sustainable_accessibility = -sustainable_accessibility)
# Scale from 0 to 100 using 2018 ranges
if (scenario_attributes[["present"]]) {
  xrange <- range(zones$sustainable_accessibility)
  readr::write_rds(xrange, file = here::here("results", "sustainable-accessibility_range.rds"))
} else {
  message("sustainable_accessibility: read ranges...")
  xrange <- readr::read_rds(here::here("results", "sustainable-accessibility_range.rds"))
}
zones$sustainable_accessibility_scaled = scale_to_range(
  zones$sustainable_accessibility, xmin = xrange[1], xmax = xrange[2], a = 0, b = 100)

# Change sign and scale from 0 to 100 using 2018 ranges
zones <- zones %>%
  dplyr::mutate(accessibility = -accessibility)
zones$accessibility_scaled = scale_accessibility(zones)

zones <- zones %>%
  dplyr::mutate(
    mode_share_sustainable = (mode_share_transit +
                                mode_share_bike +
                                mode_share_walk),
    mode_share_bike_walk = (mode_share_bike +
                              mode_share_walk)
  )

# Calculate MALPAKKA model results
zones <- zones %>%
  dplyr::mutate(malpakka = exp(17.67998 * log(abs(sustainable_accessibility)) + 0.59672 + (-90.97805)))
tontin_teho <- tontin_teho %>%
  dplyr::select(Sij2018, tontin_teho_lroik) %>%
  dplyr::rename(zone = Sij2018,
                tontin_teho_2017 = tontin_teho_lroik)
zones <- zones %>%
  dplyr::left_join(tontin_teho, by = "zone") %>%
  dplyr::mutate(malpakka_potential = malpakka - tontin_teho_2017)

# Calculate workplace densities as number of workplaces per land area (km2)
land_area <- land_area %>%
  dplyr::select(SIJ2019, Maa_ala) %>%
  dplyr::rename(zone = SIJ2019,
                land_area = Maa_ala)
zones <- zones %>%
  dplyr::left_join(land_area, by = "zone") %>%
  dplyr::mutate(total_wrk_density = total_wrk / land_area)

# Sum of travel times to two temporally closest centers
ttimes <- ttimes_pt %>%
  dplyr::filter(origin %in% zones$zone & destination %in% centers$SIJ2019) %>%
  dplyr::rename(car = car_work,
                transit = transit_work,
                bike = bike,
                walk = walk) %>%
  dplyr::select(origin, destination, car, transit, bike, walk) %>%
  tidyr::pivot_longer(
    cols = car:walk,
    values_to = "ttime",
    names_to = "mode"
  ) %>%
  dplyr::group_by(origin, mode) %>%
  dplyr::arrange(ttime) %>%
  dplyr::summarise(ttime_twocenters = sum(ttime[1:2]), .groups = "drop") %>%
  tidyr::pivot_wider(
    id_cols = origin,
    names_from = mode,
    values_from = ttime_twocenters,
    names_prefix = "ttime_twocenters_"
  ) %>%
  dplyr::rename(zone = origin)

zones <- zones %>%
  dplyr::left_join(ttimes, by = "zone")

# Accessibility to two centers
car <- twocenters(zones, mode = "car")
transit <- twocenters(zones, mode = "transit")
bike <- twocenters(zones, mode = "bike")
walk <- twocenters(zones, mode = "walk")

if (scenario_attributes[["present"]]) {
  message("twocenters: use current mode shares...")
  mode_shares <- zones
} else {
  message("twocenters: read mode shares...")
  # TODO: Maybe fix scenarios$scenario[...
  mode_shares <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenarios$scenario[scenarios$present])))
}

zones <- zones %>%
  dplyr::mutate(
    ttime_twocenters_normal_car = car$ttime_twocenters_normal,
    ttime_twocenters_normal_transit = transit$ttime_twocenters_normal,
    ttime_twocenters_normal_bike = bike$ttime_twocenters_normal,
    ttime_twocenters_normal_walk = walk$ttime_twocenters_normal,
    bins_twocenters_car = car$bins_twocenters,
    bins_twocenters_transit = transit$bins_twocenters,
    bins_twocenters_bike = bike$bins_twocenters,
    bins_twocenters_walk = walk$bins_twocenters) %>%
  dplyr::mutate(
    ttime_twocenters_all = mode_shares$mode_share_car * ttime_twocenters_normal_car +
                  mode_shares$mode_share_transit * ttime_twocenters_normal_transit +
                  mode_shares$mode_share_bike * ttime_twocenters_normal_bike +
                  mode_shares$mode_share_walk * ttime_twocenters_normal_walk
  )

# Now, we are handling already normalized travel times but I do not think that
# is an issue. They are normalized again to fit [1, 100].
all <- twocenters(zones, mode = "all")

zones <- zones %>%
  dplyr::mutate(
    ttime_twocenters_normal_all = all$ttime_twocenters_normal,
    bins_twocenters_all = all$bins_twocenters
  ) %>%
  dplyr::select(!ttime_twocenters_all)

if (scenario_attributes[["projected"]]) {
  # Calculate travel time changes with CBA data
  zones <- zones %>%
    dplyr::mutate(cba_car_time_per_tour = (cba_car_work_time + cba_car_leisure_time) / demand_car,
                  cba_transit_time_per_tour = (cba_transit_work_time + cba_transit_leisure_time) / demand_transit)
} else {
  zones <- zones %>%
    dplyr::mutate(cba_car_work_time = 0,
                  cba_car_leisure_time = 0,
                  cba_transit_work_time = 0,
                  cba_transit_leisure_time = 0,
                  cba_car_time_per_tour = 0,
                  cba_transit_time_per_tour = 0)
}


# Clean data --------------------------------------------------------------

zones <- zones %>%
  dplyr::select(!c(FID_1, WSP_SIJ, WSP_ENN, SIJ2016, SIJ_MAARA, SIJ_ID, ENN2016)) %>%
  dplyr::relocate(zone) %>%
  dplyr::relocate(area, .after = zone) %>%
  dplyr::relocate(KUNTANIMI, .after = area)


# Output ------------------------------------------------------------------

readr::write_rds(zones, file = here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))
sf::write_sf(zones, here::here("results", sprintf("zones_%s.gpkg", scenario_attributes[["scenario"]])), append = FALSE, delete_dsn = TRUE)
