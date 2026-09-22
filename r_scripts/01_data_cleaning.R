# =============================================================================
# Script: 01_data_cleaning.R
# Author: Riley Ramos
# Date: 2025
# Description: Imports all raw data used in the thesis, cleans and transforms
#              each dataset, and exports a single Excel workbook containing
#              one processed sheet per dataset.
# Output:      data/processed/thesis_data.xlsx
# =============================================================================

# --- Dependencies ------------------------------------------------------------

library(here)        # portable file paths
library(openxlsx)    # read/write Excel workbooks
library(tidyverse)   # dplyr, tidyr, stringr, readr, ggplot2


# --- Global options ----------------------------------------------------------

options(scipen = 999)                        # disable scientific notation
Sys.setenv(VROOM_CONNECTION_SIZE = 1048576)  # increase buffer for large CSVs

# --- Helper functions ---------------------------------------------------------

# Regex pattern for extracting raw census tract IDs from messy label strings.
# Matches: "1.01", "21 01", "6", "20"
TRACT_PATTERN <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"

# Converts a raw census tract string into a standardized 6-digit zero-padded
# format used consistently across all datasets (ex: "1.01" -> "000101").
transform_census_tract <- function(value) {
  # Check for digit.digit digit (ex: "1.01")
  if (grepl("^\\d\\.\\d{2}$", value)) {
    parts <- unlist(strsplit(value, " "))
    main_part <- substring(parts, 1, 1)
    digit_part <- substring(parts, 3, 4)
    return(sprintf("000%s%s", main_part, digit_part))
  } 
  # Check for digit digit.digit digit (ex: "21.01")
  else if (grepl("^\\d{2}\\.\\d{2}$", value)) {
    parts <- unlist(strsplit(value, " "))
    main_part <- substring(parts, 1, 2)
    digit_part <- substring(parts, 4, 5)
    return(sprintf("00%s%s", main_part, digit_part))
  } 
  # Check for single digit (ex: "6")
  else if (grepl("^\\d$", value)) {
    return(sprintf("000%s00", value))
  } 
  # Check for double digit (ex: "20")
  else if (grepl("^\\d{2}$", value)) {
    return(sprintf("00%s00", value))
  } 
  # Return the original value if it doesn't match
  else {
    return(value) 
  }
}

# Census tracts outside the Las Vegas Valley proper (Laughlin, Moapa, etc.)
# that are excluded from all analyses per the thesis study area definition.
EXCLUDE_TRACTS <- c("005704", "005702", "005903", "005905",
                    "005614", "005607", "005612", "005615")

# =============================================================================
# Section 1: Urban Sprawl Index
# > Filters for LV Valley census tracts 
# > Reverses sprawl indices so that higher values indicate greater sprawl
# =============================================================================

spr_index_raw <- read.xlsx(here("data", "raw", "sprawl_indices.xlsx"))

spr_index_final <- spr_index_raw %>%
  filter(msaname == "Las Vegas-Paradise, NV Metro Area") %>% 
  mutate(census_tract = substring(fips, 6)) %>% 
  select(census_tract, compositeindex2010) %>%
  rename(sprawl_index = compositeindex2010) %>%
  mutate(sprawl_index = max(sprawl_index) - sprawl_index) %>%
  filter(!census_tract %in% EXCLUDE_TRACTS)

STUDY_TRACTS <- list(unique(spr_index_final["census_tract"]))


# =============================================================================
# Section 2: Demographics Data
# =============================================================================

demog_raw <- read_csv(here("data", "raw", "demographics_DP05.csv"))

d_transposed <- as.data.frame(t(demog_raw))
colnames(d_transposed) <- d_transposed[1, ]
d_transposed <- d_transposed[-1, ]
d_transposed <- cbind(census_tract = rownames(d_transposed), d_transposed)
rownames(d_transposed) <- NULL

d_total <- d_transposed %>%
  select(-47) %>%
  filter(grepl("Total!!Estimate", d_transposed[,1])) %>%
  rename_with(str_trim) %>%
  # extract and standardize census tract with helper function
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>% 
  mutate(census_tract = sapply(census_tract, transform_census_tract)) %>%
  select_if(~ !all(is.na(.))) %>%
  # selecting only the columns needed for analysis
  select(
    census_tract,
    `Population 1 year and over`,
    `White alone, not Hispanic or Latino`,
    `Median income (dollars)`
  ) %>%
  rename(total_population = `Population 1 year and over`,
         white_population = `White alone, not Hispanic or Latino`,
         median_income    = `Median income (dollars)`) %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .)))) %>%
  # calculate POC population for analysis
  mutate(poc_population = total_population - white_population)

# =============================================================================
# Section 3: Travel Time to Work Data
# =============================================================================
travel_time_raw <- read_csv(here("data", "raw",'transportation_commute_B08303.csv'))

tt_transposed <- as.data.frame(t(travel_time_raw[, -1]))
colnames(tt_transposed) <- travel_time_raw$`Label (Grouping)`
tt_transposed <- cbind(CensusTract = rownames(tt_transposed), tt_transposed)
rownames(tt_transposed) <- NULL

tt_final <- tt_transposed %>%
  # pivot to long format, separating transportation type and time interval
  pivot_longer(
    cols      = -CensusTract,
    names_to  = c("TransportationType", "TimeInterval"),
    names_pattern = "(.*?):? ?(.*)",
    values_to = "Count"
  ) %>%
  # extract transportation type from the TimeInterval column
  mutate(
    TransportationType = str_extract(TimeInterval, "(.*?):"),
    TimeInterval = ifelse(str_detect(TimeInterval, "(.*?):"), "Total", TimeInterval),
    TransportationType = gsub(":", "", TransportationType),
    TransportationType = ifelse(TransportationType == "Total", "All Types", TransportationType)
  )  %>%
  # standardize labels
  mutate(
    TimeInterval       = ifelse(str_detect(TimeInterval, "Total"), "All time intervals", TimeInterval),
    TransportationType = ifelse(TransportationType == "All Types", "All transportation methods", TransportationType)
  ) %>%
  # forward-fill transportation type for rows where it is missing
  fill(TransportationType, .direction = "down") %>%
  mutate(TransportationType = str_trim(TransportationType),
         TimeInterval       = str_trim(TimeInterval)) %>%
  filter(!grepl("Margin of Error", CensusTract)) %>%
  # extract and standardize census tract format using helper function
  mutate(CensusTract = str_extract(CensusTract, TRACT_PATTERN)) %>%
  mutate(Count = as.numeric(str_trim(Count))) %>%
  mutate(CensusTract = sapply(CensusTract, transform_census_tract)) %>%
  rename(
    census_tract        = CensusTract,
    transportation_type = TransportationType,
    time_interval       = TimeInterval,
    count               = Count
  )


# =============================================================================
# Section 4: Employment & Economic Characteristics (ACS DP03, 2010)
# =============================================================================
# Two sub-tables extracted from the same raw file:
#   4a. Mean travel time to work (single row, row 27)
#   4b. Occupation counts by type (rows 29-34)

econ_raw <- read_csv(
  here("data", "raw", "economics_DP03.csv")
)

# --- 4a. Mean travel time to work --------------------------------------------
mtt_raw <- econ_raw[27, ]

# convert to long
mtt_transposed <- as.data.frame(t(mtt_raw[, -1]))
colnames(mtt_transposed) <- "Mean travel time to work (minutes)"
mtt_transposed <- cbind(census_tract = rownames(mtt_transposed), mtt_transposed)
rownames(mtt_transposed) <- NULL

mean_travel_time_final <- mtt_transposed %>%
  # remove margin of error rows
  filter(!grepl("Margin of Error|Percent", census_tract)) %>%
  rename_with(str_trim)  %>%
  rename(avg_work_commute_mins = "Mean travel time to work (minutes)") %>%
  # extract census tract 
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))

# --- 4b. Occupation ----------------------------------------------------------
occupation_raw <- econ_raw[28:49, ][2:7, ]

# convert to long
occ_transposed <- as.data.frame(t(occupation_raw))
colnames(occ_transposed) <- occ_transposed[1, ]
occ_transposed <- occ_transposed[-1, ]
occ_transposed <- cbind(census_tract = rownames(occ_transposed), occ_transposed)
rownames(occ_transposed) <- NULL

occupation_final <- occ_transposed %>%
  rename_with(str_trim) %>%
  # remove margin of error rows
  filter(!grepl("Margin of Error|Percent", census_tract)) %>%
  # extract census tract 
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract)) %>%
  # rename columns
  rename(employed_pop = `Civilian employed population 16 years and over`,
         mgmt_pop = `Management, business, science, and arts occupations`,
         service_pop = `Service occupations`,
         sales_office_pop = `Sales and office occupations`,
         construction_pop = `Natural resources, construction, and maintenance occupations`,
         production_pop = `Production, transportation, and material moving occupations`)



# =============================================================================
# Section 5: Transportation by Vehicle Availability (ACS B08141, 2010)
# =============================================================================
vehicle_raw <- read_csv(
  here("data", "raw", "transportation_vehicles_B08141.csv")
)

# transpose
v_transposed <- as.data.frame(t(vehicle_raw[, -1]))
colnames(v_transposed) <- vehicle_raw$`Label (Grouping)`
v_transposed <- cbind(census_tract = rownames(v_transposed), v_transposed)
rownames(v_transposed) <- NULL
colnames(v_transposed) <- str_trim(colnames(v_transposed))

vehicle_final <- v_transposed %>%
  # convert to long 
  pivot_longer(
    cols          = -census_tract,
    names_to      = c("transportation_type", "vehicle_availability"),
    names_pattern = "(.*?):? ?(.*)",
    values_to     = "count"
  ) %>%
  # create aggregate columns for vehicle availability and transportation type
  mutate(
    transportation_type  = str_extract(vehicle_availability, "(.*?):"),
    vehicle_availability = ifelse(
      str_detect(vehicle_availability, "(.*?):"),
      "All vehicle availabilities",
      vehicle_availability
    ),
    transportation_type = ifelse(
      str_detect(transportation_type, "Total:"),
      "All transportation methods",
      transportation_type
    )
  ) %>%
  fill(transportation_type, .direction = "down") %>%
  # shorten transportation_type values
  mutate(transportation_type = case_when(
    grepl("Car, truck, or van", transportation_type) ~ "Car",
    grepl("Public transportation", transportation_type) ~ "Public transportation",
    grepl("Walked", transportation_type) ~ "Walked",
    grepl("Taxicab|motorcycle|bicycle|other", transportation_type, ignore.case = TRUE) ~ "Other",
    grepl("Worked at home", transportation_type) ~ "Worked at home",
    TRUE ~ transportation_type
  )) %>%
  # remove margin of error rows
  filter(!grepl("Margin of Error", census_tract)) %>%
  # extract census tract
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract)) %>%
  mutate(count = as.numeric(count))



# =============================================================================
# Section 6: Occupation by Median Earnings (ACS B24011, 2010)
# =============================================================================
# Median earnings by occupation type. Only the 5 major occupation categories
# used in the thesis are retained.

salary_raw <- read_csv(
    here("data", "raw", "occupation_median_earnings_B24011.csv")
  ) %>%
  rename(occupation = `Label (Grouping)`)

# transpose 
salary_transposed <- as.data.frame(t(salary_raw))
colnames(salary_transposed) <- salary_transposed[1, ]
salary_transposed <- salary_transposed[-1, ]
salary_transposed <- cbind(census_tract = rownames(salary_transposed), salary_transposed)
rownames(salary_transposed) <- NULL

occupation_salary_final <- salary_transposed %>%
  rename_with(str_trim) %>%
  # remove margin of error rows
  filter(!grepl("Margin of Error|Percent", census_tract)) %>%
  # extract census tract
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract)) %>%
  # rename columns
  select(
    census_tract,
    total_med_salary        = `Total:`,
    mgmt_med_salary         = `Management, business, science, and arts occupations:`,
    service_med_salary      = `Service occupations:`,
    sales_office_med_salary = `Sales and office occupations:`,
    construction_med_salary = `Natural resources, construction, and maintenance occupations:`,
    production_med_salary   = `Production, transportation, and material moving occupations:`
  ) %>%
  mutate(across(-census_tract, ~ as.numeric(gsub(",", "", .))))



# =============================================================================
# Section 7: PM2.5 Air Pollution (CDC, 2006–2010)
# =============================================================================
# Daily predicted PM2.5 concentrations at census tract level.
# The full CDC file is several GB and not stored in this repo.
# The Clark County extract (pm25_clark_county.txt) was generated once from
# the full dataset and saved locally — that file is what we read here.
#
# To regenerate pm25_clark_county.txt from the full CDC dataset, download
# it from: https://data.cdc.gov and uncomment the block below:
#
# library(data.table)
# full_data <- fread("path/to/Daily_Census_Tract-Level_PM2.5_Concentrations_2006-2010.csv")
# clark_county <- full_data %>% filter(statefips == 32, countyfips == 32003)
# write.table(clark_county, here("data", "raw", "pm25_clark_county.txt"),
#             sep = "\t", row.names = FALSE)

pm_raw <- read.csv(
  here("data", "raw", "pm25_clark_county.txt"),
  header = TRUE,
  sep    = "\t"
)

# Average daily PM2.5 over the 2010 study period, by census tract
pm25_final <- pm_raw %>%
  select(ctfips, DS_PM_pred) %>%
  group_by(ctfips) %>%
  summarise(avg_PM_pred = mean(DS_PM_pred, na.rm = TRUE), .groups = "drop") %>%
  # extract census tract
  mutate(census_tract = substr(as.character(ctfips), 6, nchar(as.character(ctfips)))) %>%
  select(census_tract, avg_PM_pred)

# =============================================================================
# Export: Write all cleaned datasets to a single Excel workbook
# =============================================================================
# Sheets used directly in thesis analyses are listed first.
# Supplementary sheets (migration, health, ej_screen, educational_attainment)
# are included for completeness but do not feed into the published results.

wb <- createWorkbook()

datasets <- list(
  # --- Core thesis datasets ---
  sprawl_index                 = spr_index_final,
  demographics                 = d_total,
  work_commute_time            = travel_time_final,
  mean_travel_time             = mean_travel_time_final,
  occupation                   = occupation_final,
  industry                     = industry_final,
  occupation_salary            = occupation_salary_final,
  transportation_vehicle_avail = vehicle_final,
  PM25_concentrations          = pm25_final
)

for (sheet_name in names(datasets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = datasets[[sheet_name]])
}

saveWorkbook(wb, file = here("data", "processed", "thesis_data.xlsx"), overwrite = TRUE)

message("✔ thesis_data.xlsx saved to data/processed/")
