
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
# --- 4c. Industry ------------------------------------------------------------
econ_raw <- read_csv(
  here("data", "raw", "economics_DP03.csv")
)

industry_raw <- econ_raw[28:49, ][9:22, ]

ind_transposed <- as.data.frame(t(industry_raw))
colnames(ind_transposed) <- ind_transposed[1, ]
ind_transposed <- ind_transposed[-1, ]
ind_transposed <- cbind(census_tract = rownames(ind_transposed), ind_transposed)
rownames(ind_transposed) <- NULL

industry_final <- ind_transposed %>%
  rename_with(str_trim) %>%
  filter(!grepl("Margin of Error|Percent", census_tract)) %>%
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))

# =============================================================================
# Section 5: Work Status in the Past 12 Months (ACS S2303, 2010)
# =============================================================================
# Two sub-tables: weeks worked per year (5a) and hours worked per week (5b),
# plus a summary of population base and non-workers (5c).

status_raw <- read_csv(
  here("data", "raw", "S2303 WORK STATUS IN THE PAST 12 MONTHS.csv")
)

st_transposed <- as.data.frame(t(status_raw))
colnames(st_transposed) <- st_transposed[1, ]
st_transposed <- st_transposed[-1, ]
st_transposed <- cbind(census_tract = rownames(st_transposed), st_transposed)
rownames(st_transposed) <- NULL

st_upd <- st_transposed %>%
  filter(!grepl("Margin of Error", census_tract)) %>%
  filter(grepl("Total!!Estimate", census_tract))

# --- 5a. Weeks worked per year -----------------------------------------------
weeks_worked_final <- st_upd %>%
  select(1:9) %>%
  select(-`WEEKS WORKED`) %>%
  rename_with(str_trim) %>%
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))

# --- 5b. Hours worked per week -----------------------------------------------
# The raw columns interleave three hours-per-week categories with two
# weeks-per-year categories each (suffixed .1, .2 to disambiguate).
# We pivot both dimensions and filter to keep only valid combinations.

hrs_weekly <- c(
  "Usually worked 35 or more hours per week",
  "Usually worked 15 to 34 hours per week",
  "Usually worked 1 to 14 hours per week"
)
wks_yearly <- c(
  "40 or more weeks",   "50 to 52 weeks",
  "40 or more weeks.1", "50 to 52 weeks.1",
  "40 or more weeks.2", "50 to 52 weeks.2"
)

hours_base <- st_upd %>%
  select(c(1:2, 11:21)) %>%
  rename_with(str_trim)

hours_worked_final <- hours_base %>%
  pivot_longer(cols = all_of(hrs_weekly), names_to = "hrs_weekly", values_to = "hrs_weekly_perc") %>%
  pivot_longer(cols = all_of(wks_yearly), names_to = "wks_yearly",  values_to = "wks_yearly_perc") %>%
  # Keep only the valid hrs_weekly / wks_yearly pairings (matched by suffix)
  mutate(hrs_weekly = case_when(
    hrs_weekly == "Usually worked 35 or more hours per week" &
      wks_yearly %in% c("40 or more weeks",   "50 to 52 weeks")   ~ hrs_weekly,
    hrs_weekly == "Usually worked 15 to 34 hours per week"  &
      wks_yearly %in% c("40 or more weeks.1", "50 to 52 weeks.1") ~ hrs_weekly,
    hrs_weekly == "Usually worked 1 to 14 hours per week"   &
      wks_yearly %in% c("40 or more weeks.2", "50 to 52 weeks.2") ~ hrs_weekly,
    TRUE ~ "Delete"
  )) %>%
  filter(hrs_weekly != "Delete") %>%
  mutate(wks_yearly = gsub("\\.1|\\.2", "", wks_yearly)) %>%  # remove disambiguation suffixes
  select(-`Mean usual hours worked for workers`, -`Did not work`) %>%
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))

# --- 5c. Population base, mean hours, and non-workers -----------------------
other_work_final <- hours_base %>%
  select(census_tract,
         `Population 16 to 64 years`,
         `Mean usual hours worked for workers`,
         `Did not work`) %>%
  mutate(census_tract = str_extract(census_tract, TRACT_PATTERN)) %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))

