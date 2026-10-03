# PROJECT: Exchange Rate Pass-Through in CEE Countries
# 2nd Version

rm(list = ls())
cat("\014")
graphics.off()


packages <- c(
  "readr",
  "readxl",
  "ggplot2",
  "tidyverse",
  "writexl",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "purrr"
)

new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)



data_folder <- "data"
output_folder <- "results"
figure_folder <- file.path(output_folder, "figures")

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_folder,
  recursive = TRUE,
  showWarnings = FALSE
)



# To makes the script work whether the Eurostat downloads were
# saved as .csv or .csv.gz, 

find_data_file <- function(pattern, folder = data_folder){

  candidates <- list.files(
    folder,
    full.names = TRUE,
    recursive = FALSE
  )

  matches <- candidates[
    str_detect(
      basename(candidates),
      fixed(pattern)
    )
  ]

  if(length(matches) == 0){
    stop(
      paste0(
        "Could not find a file containing: ",
        pattern,
        " in ",
        folder
      )
    )
  }

  matches[1]
}


# READ DATA

data1 <- read_csv(
  find_data_file("ert_bil_eur_m"),
  show_col_types = FALSE
)

data2 <- read_csv(
  find_data_file("ert_eff_ic_m"),
  show_col_types = FALSE
)

data3 <- read_csv(
  find_data_file("prc_hicp_iw"),
  show_col_types = FALSE
)

# HICP
data4 <- read_csv(
  find_data_file("prc_hicp_minr__custom_22915512"),
  show_col_types = FALSE
)

# The second HICP file was previously checked and found to be a
# duplicate of the first one. We therefore do not use it again.

data6 <- read_csv(
  find_data_file("sts_inpi_m"),
  show_col_types = FALSE
)

data7 <- read_csv(
  find_data_file("sts_inpp_m"),
  show_col_types = FALSE
)

data8 <- read_csv(
  find_data_file("sts_inpr_m"),
  show_col_types = FALSE
)

data9 <- read_csv(
  find_data_file("une_rt_m"),
  show_col_types = FALSE
)

# Historical former-euro-area national currencies.
# This is required for the Slovak koruna / euro bilateral rate.

data10 <- read_csv(
  find_data_file("ert_h_eur_m"),
  show_col_types = FALSE
)

# World Bank Pink Sheet
pink_file <- find_data_file(
  "CMO-Historical-Data-Monthly"
)


# HICP: COUNTRY-SPECIFIC PRICE INDICES + INFLATION
# We keep the 2015=100 HICP indices and rename them clearly.
# Inflation is then constructed from log differences:
#
#   monthly inflation = 100 * [log(P_t) - log(P_t-1)]
#   annual inflation  = 100 * [log(P_t) - log(P_t-12)]


build_hicp_country <- function(country_name){

  hicp <- data4 %>%
    filter(
      geo == country_name,
      unit == "Index, 2015=100",
      coicop18 %in% c(
        "Food and non-alcoholic beverages",
        "Food",
        "Cereals and cereal products (ND)",
        "Cereals (ND)",
        "Total"
      )
    ) %>%
    transmute(
      TIME_PERIOD,
      coicop18,
      value = OBS_VALUE
    ) %>%
    pivot_wider(
      names_from = coicop18,
      values_from = value
    ) %>%
    rename(
      HICP_FoodNonAlcoholic_Index =
        `Food and non-alcoholic beverages`,
      HICP_Food_Index =
        Food,
      HICP_CerealsCerealProducts_Index =
        `Cereals and cereal products (ND)`,
      HICP_Cereals_Index =
        `Cereals (ND)`,
      HICP_Total_Index =
        Total
    ) %>%
    mutate(
      date = as.Date(
        paste0(TIME_PERIOD, "-01")
      )
    ) %>%
    arrange(date) %>%

    
    # Construct inflation rates AFTER creating the index series
    
    mutate(
      Inflation_Total_MoM =
        100 * (
          log(HICP_Total_Index) -
            lag(log(HICP_Total_Index), 1)
        ),

      Inflation_Total_YoY =
        100 * (
          log(HICP_Total_Index) -
            lag(log(HICP_Total_Index), 12)
        ),

      Inflation_FoodNonAlcoholic_MoM =
        100 * (
          log(HICP_FoodNonAlcoholic_Index) -
            lag(log(HICP_FoodNonAlcoholic_Index), 1)
        ),

      Inflation_FoodNonAlcoholic_YoY =
        100 * (
          log(HICP_FoodNonAlcoholic_Index) -
            lag(log(HICP_FoodNonAlcoholic_Index), 12)
        ),

      Inflation_Food_MoM =
        100 * (
          log(HICP_Food_Index) -
            lag(log(HICP_Food_Index), 1)
        ),

      Inflation_Food_YoY =
        100 * (
          log(HICP_Food_Index) -
            lag(log(HICP_Food_Index), 12)
        ),

      Inflation_CerealsCerealProducts_MoM =
        100 * (
          log(HICP_CerealsCerealProducts_Index) -
            lag(log(HICP_CerealsCerealProducts_Index), 1)
        ),

      Inflation_CerealsCerealProducts_YoY =
        100 * (
          log(HICP_CerealsCerealProducts_Index) -
            lag(log(HICP_CerealsCerealProducts_Index), 12)
        ),

      Inflation_Cereals_MoM =
        100 * (
          log(HICP_Cereals_Index) -
            lag(log(HICP_Cereals_Index), 1)
        ),

      Inflation_Cereals_YoY =
        100 * (
          log(HICP_Cereals_Index) -
            lag(log(HICP_Cereals_Index), 12)
        )
    ) %>%
    select(
      date,
      TIME_PERIOD,
      everything()
    )

  hicp
}


data4_HU_filtered_final <- build_hicp_country(
  "Hungary"
)

data4_SK_filtered_final <- build_hicp_country(
  "Slovakia"
)



#  PRODUCER PRICE INDEX
# The original script used 2015=100.
#
# Eurostat's current STS vintage is 2021=100. We therefore use
# 2021=100 where it is available, because it continues further
# into the recent period.
#
# If a country unexpectedly has no 2021=100 series, the code falls
# back to 2015=100 and prints a warning.


build_ppi_country <- function(country_name){

  available_units <- data7 %>%
    filter(
      geo == country_name
    ) %>%
    distinct(unit) %>%
    pull(unit)

  ppi_unit <- if(
    "Index, 2021=100" %in% available_units
  ){
    "Index, 2021=100"
  } else {
    warning(
      paste0(
        country_name,
        ": PPI 2021=100 not found. Using 2015=100."
      )
    )
    "Index, 2015=100"
  }

  ppi <- data7 %>%
    filter(
      geo == country_name,
      unit == ppi_unit
    ) %>%
    transmute(
      date = as.Date(
        paste0(TIME_PERIOD, "-01")
      ),
      ProducerPriceIndex = OBS_VALUE
    ) %>%
    arrange(date)

  # Safety check:
  # There should be only one observation per month after filtering.
  if(any(duplicated(ppi$date))){
    warning(
      paste0(
        country_name,
        ": multiple PPI observations per month remain after filtering. ",
        "Please inspect the PPI dimensions before estimation."
      )
    )
  }

  ppi
}


data7_HU_filtered <- build_ppi_country(
  "Hungary"
)

data7_SK_filtered <- build_ppi_country(
  "Slovakia"
)



# INDUSTRIAL PRODUCTION
# 2021=100 instead of 2015=100
# Seasonally AND calendar adjusted data instead of unadjusted


build_ip_country <- function(country_name){

  ip <- data8 %>%
    filter(
      geo == country_name,
      unit == "Index, 2021=100",
      s_adj == "Seasonally and calendar adjusted data",
      nace_r2 %in% c(
        "Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply",
        "Manufacturing",
        "Electricity, gas, steam and air conditioning supply"
      )
    ) %>%
    transmute(
      TIME_PERIOD,
      nace_r2,
      value = OBS_VALUE
    ) %>%
    pivot_wider(
      names_from = nace_r2,
      values_from = value
    ) %>%
    rename(
      ProductionVolume_SA_BroadIndustry =
        `Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply`,
      ProductionVolume_SA_Manufacturing =
        Manufacturing,
      ProductionVolume_SA_ElectricityGasSteamAC =
        `Electricity, gas, steam and air conditioning supply`
    ) %>%
    mutate(
      date = as.Date(
        paste0(TIME_PERIOD, "-01")
      )
    ) %>%
    arrange(date) %>%
    select(
      date,
      everything()
    )

  ip
}


data8_HU_filtered_final <- build_ip_country(
  "Hungary"
)

data8_SK_filtered_final <- build_ip_country(
  "Slovakia"
)



# UNEMPLOYMENT
#  seasonally adjusted monthly unemployment
#   - retain both percentage and thousand persons, as before

build_unemployment_country <- function(country_name){

  unemp <- data9 %>%
    filter(
      geo == country_name,
      age == "Total",
      sex == "Total",
      s_adj ==
        "Seasonally adjusted data, not calendar adjusted data",
      unit %in% c(
        "Percentage of population in the labour force",
        "Thousand persons"
      )
    ) %>%
    transmute(
      TIME_PERIOD,
      unit,
      value = OBS_VALUE
    ) %>%
    pivot_wider(
      names_from = unit,
      values_from = value
    ) %>%
    rename(
      UnemploymentPercentage_SA =
        `Percentage of population in the labour force`,
      UnemploymentThousandPersons_SA =
        `Thousand persons`
    ) %>%
    mutate(
      date = as.Date(
        paste0(TIME_PERIOD, "-01")
      )
    ) %>%
    arrange(date) %>%
    select(
      date,
      everything()
    )

  unemp
}


data9_HU_filtered_final <- build_unemployment_country(
  "Hungary"
)

data9_SK_filtered_final <- build_unemployment_country(
  "Slovakia"
)



# NEER
# We retain all five definitions already collected by the group,
# but define the 42-trading-partner NEER as the baseline measure.
#
#   positive NEER_Depreciation = domestic currency depreciation
# --------------------------------------------------------------

build_neer_country <- function(
    country_name,
    prefix
){

  neer <- data2 %>%
    filter(
      geo == country_name,
      unit == "Index, 2015=100",
      exch_rt %in% c(
        "Nominal effective exchange rate - 20 trading partners (euro area 2023-2025)",
        "Nominal effective exchange rate - 21 trading partners (euro area from 2026)",
        "Nominal effective exchange rate - 27 trading partners (European Union from 2020)",
        "Nominal effective exchange rate - 37 trading partners (industrial countries)",
        "Nominal effective exchange rate - 42 trading partners (industrial countries)"
      )
    ) %>%
    mutate(
      neer_name = case_when(
        str_detect(exch_rt, "20 trading") ~
          paste0("NEER_", prefix, "20"),
        str_detect(exch_rt, "21 trading") ~
          paste0("NEER_", prefix, "21"),
        str_detect(exch_rt, "27 trading") ~
          paste0("NEER_", prefix, "27"),
        str_detect(exch_rt, "37 trading") ~
          paste0("NEER_", prefix, "37"),
        str_detect(exch_rt, "42 trading") ~
          paste0("NEER_", prefix, "42")
      )
    ) %>%
    transmute(
      TIME_PERIOD,
      neer_name,
      value = OBS_VALUE
    ) %>%
    pivot_wider(
      names_from = neer_name,
      values_from = value
    ) %>%
    mutate(
      date = as.Date(
        paste0(TIME_PERIOD, "-01")
      )
    ) %>%
    arrange(date)

  baseline_name <- paste0(
    "NEER_",
    prefix,
    "42"
  )

  neer <- neer %>%
    mutate(
      NEER42_Depreciation_MoM =
        -100 * (
          log(.data[[baseline_name]]) -
            lag(log(.data[[baseline_name]]), 1)
        ),

      NEER42_Depreciation_YoY =
        -100 * (
          log(.data[[baseline_name]]) -
            lag(log(.data[[baseline_name]]), 12)
        )
    ) %>%
    select(
      date,
      -TIME_PERIOD,
      everything()
    )

  neer
}


data2_HU_filtered_final <- build_neer_country(
  "Hungary",
  "HUF"
)

data2_SK_filtered_final <- build_neer_country(
  "Slovakia",
  "SK"
)



# Bilateral HUF EUR 

# The monthly average is the preferred baseline bilateral measure.# --------------------------------------------------------------

data1_HU_filtered_final <- data1 %>%
  filter(
    currency == "Hungarian forint",
    unit == "National currency",
    statinfo %in% c(
      "Average",
      "Value at the end of the period"
    )
  ) %>%
  mutate(
    series_name = case_when(
      statinfo == "Average" ~
        "EURHUF_Average",
      statinfo == "Value at the end of the period" ~
        "EURHUF_EndOfPeriod"
    )
  ) %>%
  transmute(
    TIME_PERIOD,
    series_name,
    value = OBS_VALUE
  ) %>%
  pivot_wider(
    names_from = series_name,
    values_from = value
  ) %>%
  mutate(
    date = as.Date(
      paste0(TIME_PERIOD, "-01")
    )
  ) %>%
  arrange(date) %>%
  mutate(
    EURHUF_Depreciation_MoM =
      100 * (
        log(EURHUF_Average) -
          lag(log(EURHUF_Average))
      )
  ) %>%
  select(
    date,
    -TIME_PERIOD,
    everything()
  )



# HISTORICAL EUR/SKK

# Slovakia adopted the euro in January 2009.
#
# The historical Eurostat file continues to display the irrevocable
# conversion rate AFTER 2009 (THE VALUE IS not a market rate).Therefore:
#   - keep the average EUR/SKK series only before 2009-01
#   - post-2009 bilateral EUR/SKK remains missing in the combined panel
#   - NEER remains the main full-sample exchange-rate variable for Slovakia


data10_SK_filtered_final <- data10 %>%
  filter(
    currency == "Slovak koruna",
    statinfo == "Average",
    TIME_PERIOD < "2009-01"
  ) %>%
  transmute(
    date = as.Date(
      paste0(TIME_PERIOD, "-01")
    ),
    EURSKK_Average = OBS_VALUE
  ) %>%
  arrange(date) %>%
  mutate(
    EURSKK_Depreciation_MoM =
      100 * (
        log(EURSKK_Average) -
          lag(log(EURSKK_Average))
      )
  )


# EURO-AREA HICP
# Thats the Foreign-price control.
#
# We use the changing-composition euro-area aggregate so that the series
# follows the euro area as membership changes over time.

EA_HICP <- data4 %>%
  filter(
    str_starts(
      geo,
      fixed("Euro area (EA11-1999")
    ),
    coicop18 == "Total",
    unit == "Index, 2015=100"
  ) %>%
  transmute(
    date = as.Date(
      paste0(TIME_PERIOD, "-01")
    ),
    EA_HICP_Index = OBS_VALUE
  ) %>%
  arrange(date) %>%
  mutate(
    EA_Inflation_MoM =
      100 * (
        log(EA_HICP_Index) -
          lag(log(EA_HICP_Index))
      ),

    EA_Inflation_YoY =
      100 * (
        log(EA_HICP_Index) -
          lag(log(EA_HICP_Index), 12)
      )
  )



#  WORLD BANK PINK SHEET:
#     WORLD ENERGY AND FOOD PRICE INDICES
#   column 1 = month
#   column 3 = Energy index
#   column 7 = Food index
#   data begin in row 10

pink_indices_raw <- read_excel(
  pink_file,
  sheet = "Monthly Indices",
  col_names = FALSE
)

global_prices <- pink_indices_raw %>%
  slice(10:n()) %>%
  select(
    date_raw = ...1,
    World_Energy_Index = ...3,
    World_Food_Index = ...7
  ) %>%
  mutate(
    year = as.integer(
      str_sub(date_raw, 1, 4)
    ),

    month = as.integer(
      str_sub(date_raw, 6, 7)
    ),

    date = as.Date(
      paste(
        year,
        month,
        "01",
        sep = "-"
      )
    ),

    World_Energy_Index =
      parse_number(
        as.character(World_Energy_Index)
      ),

    World_Food_Index =
      parse_number(
        as.character(World_Food_Index)
      )
  ) %>%
  arrange(date) %>%
  mutate(
    World_Energy_Growth_MoM =
      100 * (
        log(World_Energy_Index) -
          lag(log(World_Energy_Index))
      ),

    World_Food_Growth_MoM =
      100 * (
        log(World_Food_Index) -
          lag(log(World_Food_Index))
      )
  ) %>%
  select(
    date,
    World_Energy_Index,
    World_Food_Index,
    World_Energy_Growth_MoM,
    World_Food_Growth_MoM
  )



# HUNGARIAN DATASET

data_underconstruction <- data4_HU_filtered_final %>%
  select(
    -TIME_PERIOD
  ) %>%
  full_join(
    data7_HU_filtered,
    by = "date"
  ) %>%
  full_join(
    data8_HU_filtered_final %>%
      select(-TIME_PERIOD),
    by = "date"
  ) %>%
  full_join(
    data9_HU_filtered_final %>%
      select(-TIME_PERIOD),
    by = "date"
  ) %>%
  full_join(
    data2_HU_filtered_final,
    by = "date"
  ) %>%
  full_join(
    data1_HU_filtered_final,
    by = "date"
  ) %>%
  left_join(
    EA_HICP,
    by = "date"
  ) %>%
  left_join(
    global_prices,
    by = "date"
  ) %>%
  mutate(
    country = "Hungary"
  ) %>%
  filter(
    date >= as.Date("1996-01-01")
  ) %>%
  arrange(date) %>%
  relocate(
    country,
    date
  )


# CONSTRUCT SLOVAK DATASET

data_ucs_SK <- data4_SK_filtered_final %>%
  select(
    -TIME_PERIOD
  ) %>%
  full_join(
    data7_SK_filtered,
    by = "date"
  ) %>%
  full_join(
    data8_SK_filtered_final %>%
      select(-TIME_PERIOD),
    by = "date"
  ) %>%
  full_join(
    data9_SK_filtered_final %>%
      select(-TIME_PERIOD),
    by = "date"
  ) %>%
  full_join(
    data2_SK_filtered_final,
    by = "date"
  ) %>%
  full_join(
    data10_SK_filtered_final,
    by = "date"
  ) %>%
  left_join(
    EA_HICP,
    by = "date"
  ) %>%
  left_join(
    global_prices,
    by = "date"
  ) %>%
  mutate(
    country = "Slovakia"
  ) %>%
  filter(
    date >= as.Date("1996-01-01")
  ) %>%
  arrange(date) %>%
  relocate(
    country,
    date
  )


# COMBINED HU-SK DATASETS

data_combined <- bind_rows(
  data_underconstruction,
  data_ucs_SK
) %>%
  arrange(
    country,
    date
  )



# BASIC DATA-QUALITY CHECKS


cat("\n")
cat("============================================================\n")
cat("FINAL DATASET CHECKS\n")
cat("============================================================\n\n")

# Dimensions
cat("Hungary dimensions:\n")
print(
  dim(data_underconstruction)
)

cat("\nSlovakia dimensions:\n")
print(
  dim(data_ucs_SK)
)

cat("\nCombined panel dimensions:\n")
print(
  dim(data_combined)
)

# Duplicate country-month observations
duplicates <- data_combined %>%
  count(
    country,
    date
  ) %>%
  filter(
    n > 1
  )

cat("\nDuplicate country-month observations:\n")
print(
  duplicates
)

# Time coverage
coverage <- data_combined %>%
  group_by(country) %>%
  summarise(
    first_date = min(
      date,
      na.rm = TRUE
    ),
    last_date = max(
      date,
      na.rm = TRUE
    ),
    observations = n(),
    .groups = "drop"
  )

cat("\nTime coverage:\n")
print(
  coverage
)


#. DESCRIPTIVE STATISTICS

descriptive_variables <- c(
  "HICP_Total_Index",
  "Inflation_Total_MoM",
  "Inflation_Total_YoY",
  "NEER_HUF42",
  "NEER_SK42",
  "NEER42_Depreciation_MoM",
  "ProductionVolume_SA_BroadIndustry",
  "UnemploymentPercentage_SA",
  "ProducerPriceIndex",
  "EA_HICP_Index",
  "EA_Inflation_YoY",
  "World_Energy_Index",
  "World_Food_Index"
)

descriptive_stats <- data_combined %>%
  select(
    country,
    any_of(descriptive_variables)
  ) %>%
  pivot_longer(
    cols = -country,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(
    country,
    variable
  ) %>%
  summarise(
    N = sum(
      !is.na(value)
    ),
    Missing = sum(
      is.na(value)
    ),
    Mean = mean(
      value,
      na.rm = TRUE
    ),
    SD = sd(
      value,
      na.rm = TRUE
    ),
    Min = min(
      value,
      na.rm = TRUE
    ),
    P25 = quantile(
      value,
      0.25,
      na.rm = TRUE
    ),
    Median = median(
      value,
      na.rm = TRUE
    ),
    P75 = quantile(
      value,
      0.75,
      na.rm = TRUE
    ),
    Max = max(
      value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(
  descriptive_stats,
  n = Inf
)



#  MISSINGNESS TABLE

missingness <- data_combined %>%
  group_by(country) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        missing = ~ sum(is.na(.x)),
        nonmissing = ~ sum(!is.na(.x))
      )
    ),
    .groups = "drop"
  )



# PLOTS


# PLOT 1: HEADLINE INFLATION (YoY)

p1 <- ggplot(
  data_combined,
  aes(
    x = date,
    y = Inflation_Total_YoY,
    group = country
  )
) +
  geom_line(
    aes(
      linetype = country
    ),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  labs(
    title = "Headline HICP inflation",
    subtitle = "Year-on-year log change",
    x = NULL,
    y = "Percent",
    linetype = "Country"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    figure_folder,
    "01_headline_inflation_yoy.png"
  ),
  p1,
  width = 9,
  height = 5,
  dpi = 300
)



# PLOT 2: BASELINE NEER (42 TRADING PARTNERS)

neer_plot_data <- data_combined %>%
  mutate(
    NEER42 = case_when(
      country == "Hungary" ~
        NEER_HUF42,
      country == "Slovakia" ~
        NEER_SK42
    )
  )

p2 <- ggplot(
  neer_plot_data,
  aes(
    x = date,
    y = NEER42,
    group = country
  )
) +
  geom_line(
    aes(
      linetype = country
    ),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  labs(
    title = "Nominal effective exchange rate",
    subtitle = "42 trading partners; higher values indicate appreciation",
    x = NULL,
    y = "Index",
    linetype = "Country"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    figure_folder,
    "02_neer_42_partners.png"
  ),
  p2,
  width = 9,
  height = 5,
  dpi = 300
)



# PLOT 3: INDUSTRIAL PRODUCTION


p3 <- ggplot(
  data_combined,
  aes(
    x = date,
    y = ProductionVolume_SA_BroadIndustry,
    group = country
  )
) +
  geom_line(
    aes(
      linetype = country
    ),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  labs(
    title = "Industrial production",
    subtitle = "Broad industry aggregate, seasonally and calendar adjusted",
    x = NULL,
    y = "Index, 2021=100",
    linetype = "Country"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    figure_folder,
    "03_industrial_production.png"
  ),
  p3,
  width = 9,
  height = 5,
  dpi = 300
)



# PLOT 4: UNEMPLOYMENT

p4 <- ggplot(
  data_combined,
  aes(
    x = date,
    y = UnemploymentPercentage_SA,
    group = country
  )
) +
  geom_line(
    aes(
      linetype = country
    ),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  labs(
    title = "Unemployment rate",
    subtitle = "Seasonally adjusted",
    x = NULL,
    y = "Percent of labour force",
    linetype = "Country"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    figure_folder,
    "04_unemployment_rate.png"
  ),
  p4,
  width = 9,
  height = 5,
  dpi = 300
)


# PLOT 5: WORLD ENERGY AND FOOD PRICE INDICES


global_plot_data <- global_prices %>%
  select(
    date,
    World_Energy_Index,
    World_Food_Index
  ) %>%
  pivot_longer(
    cols = c(
      World_Energy_Index,
      World_Food_Index
    ),
    names_to = "series",
    values_to = "index"
  ) %>%
  mutate(
    series = recode(
      series,
      World_Energy_Index =
        "Energy",
      World_Food_Index =
        "Food"
    )
  )

p5 <- ggplot(
  global_plot_data,
  aes(
    x = date,
    y = index,
    group = series
  )
) +
  geom_line(
    aes(
      linetype = series
    ),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  labs(
    title = "World commodity price indices",
    subtitle = "World Bank Pink Sheet, 2010=100",
    x = NULL,
    y = "Index, 2010=100",
    linetype = "Series"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    figure_folder,
    "05_world_energy_food.png"
  ),
  p5,
  width = 9,
  height = 5,
  dpi = 300
)



# PLOT 6: NATIONAL VS EURO-AREA INFLATION


ea_comparison <- data_combined %>%
  select(
    country,
    date,
    Inflation_Total_YoY,
    EA_Inflation_YoY
  ) %>%
  pivot_longer(
    cols = c(
      Inflation_Total_YoY,
      EA_Inflation_YoY
    ),
    names_to = "series",
    values_to = "inflation"
  ) %>%
  mutate(
    series = recode(
      series,
      Inflation_Total_YoY =
        "National HICP",
      EA_Inflation_YoY =
        "Euro-area HICP"
    )
  )

p6 <- ggplot(
  ea_comparison,
  aes(
    x = date,
    y = inflation,
    group = interaction(
      country,
      series
    )
  )
) +
  geom_line(
    aes(
      linetype = series
    ),
    linewidth = 0.65,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ country,
    ncol = 1
  ) +
  labs(
    title = "National and euro-area inflation",
    subtitle = "Year-on-year HICP inflation",
    x = NULL,
    y = "Percent",
    linetype = "Series"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    figure_folder,
    "06_national_vs_ea_inflation.png"
  ),
  p6,
  width = 9,
  height = 7,
  dpi = 300
)


# SAVE FINAL DATASETS AND SUMMARY OUTPUTS

# Country-specific Excel files

write_xlsx(
  data_underconstruction,
  file.path(
    output_folder,
    "MacroPolicyAnalysis_Data_HUN_Temp.xlsx"
  )
)

write_xlsx(
  data_ucs_SK,
  file.path(
    output_folder,
    "MacroPolicyAnalysis_Data_SK_Temp.xlsx"
  )
)

# Combined panel

write_xlsx(
  data_combined,
  file.path(
    output_folder,
    "MacroPolicyAnalysis_Data_HU_SK_Merged.xlsx"
  )
)

write_csv(
  data_combined,
  file.path(
    output_folder,
    "MacroPolicyAnalysis_Data_HU_SK_Merged.csv"
  )
)

# One compact workbook containing the main outputs

write_xlsx(
  list(
    Hungary = data_underconstruction,
    Slovakia = data_ucs_SK,
    Combined = data_combined,
    Coverage = coverage,
    Descriptive_Statistics = descriptive_stats,
    Missingness = missingness
  ),
  file.path(
    output_folder,
    "MacroPolicyAnalysis_Descriptive_Statistics.xlsx"
  )
)

######
