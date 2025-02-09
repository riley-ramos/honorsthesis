# libraries
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringr)

# disable scientific notation
options(scipen = 999)

# set wd
setwd('/Users/rileyramos/Desktop/git/honorsthesis/data')

# Function to transform the census_tract value
transform_census_tract <- function(value) {
  # Check for digit.digit digit (e.g., "1.01")
  if (grepl("^\\d\\.\\d{2}$", value)) {
    parts <- unlist(strsplit(value, " "))
    main_part <- substring(parts, 1, 1)
    digit_part <- substring(parts, 3, 4)
    return(sprintf("000%s%s", main_part, digit_part))
  } 
  # Check for digit digit.digit digit (e.g., "21.01")
  else if (grepl("^\\d{2}\\.\\d{2}$", value)) {
    parts <- unlist(strsplit(value, " "))
    main_part <- substring(parts, 1, 2)
    digit_part <- substring(parts, 4, 5)
    return(sprintf("00%s%s", main_part, digit_part))
  } 
  # Check for single digit (e.g., "6")
  else if (grepl("^\\d$", value)) {
    return(sprintf("000%s00", value))
  } 
  # Check for double digit (e.g., "20")
  else if (grepl("^\\d{2}$", value)) {
    return(sprintf("00%s00", value))
  } 
  # Return the original value if it doesn't match
  else {
    return(value) 
  }
}

# open file 
sprIndex <- read.xlsx("sprawl indices2010_censustract.xlsx")

# remove last column 
sprIndex <- sprIndex[, -ncol(sprIndex)]

# filter for Las Vegas Metro Area
sprIndex <- sprIndex %>%
  filter(msaname == "Las Vegas-Paradise, NV Metro Area")

# separate fips into state (2), county (3), census tract (6)
sprIndex <- sprIndex %>%
  mutate(county = substring(fips, 3, 5),
         tract = substring(fips, 6))

# remove fips col
sprIndex <- sprIndex[, -1]

# read travel time data
travelTime <- read_csv('Means of Transportation to Work by Travel Time to Work.csv')

# transpose data 
tt_transpose <- as.data.frame(t(travelTime[, -1]))
colnames(tt_transpose) <- travelTime$`Label (Grouping)`
tt_transpose <- cbind(CensusTract = rownames(tt_transpose), tt_transpose)
rownames(tt_transpose) <- NULL

# remove margin of error rows 
tt_updated <- tt_transpose[!grepl("Margin of Error", tt_transpose[, 1]), ]

# remove leading white space
names(tt_updated) <- str_trim(names(tt_updated))

# Create a regular expression to extract the transportation method and time interval from the column names
tt_long <- tt_updated %>%
  pivot_longer(
    cols = -CensusTract,
    names_to =c("TransportationType", "TimeInterval"),
    names_pattern = "(.*?):? ?(.*)",
    values_to = "Value"
  )

# Put transportation methods into TransportationType 
tt_long <- tt_long %>%
  mutate(TransportationType = str_extract(TimeInterval, "(.*?):"))

# Replace "Total:" with "Total" in TimeInterval
tt_long <- tt_long %>%
  mutate(TimeInterval = ifelse(str_detect(TimeInterval, "(.*?):"), "Total", TimeInterval))

# Remove ":" from TransportationType values
tt_long <- tt_long %>%
  mutate(TransportationType = ifelse(str_detect(TransportationType, ":"), gsub(":", "", TransportationType), TransportationType))

# Replace "Total" with "All Types" in TransportationType
tt_long <- tt_long %>%
  mutate(TransportationType = ifelse(TransportationType == "Total", "All Types", TransportationType))

# Just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
test <- tt_long %>%
  mutate(CensusTract_Updated = str_extract(CensusTract, pattern))
test <- test %>%
  mutate(CensusTract = CensusTract_Updated)
test <- test[, -ncol(test)]
# check that there are no null values in CensusTract
any(is.na(test$CensusTract)) 
# if false, update df
tt_long <- test

# Fill in missing TransportationType values 
test <- tt_long %>%
  fill(TransportationType, .direction = "down")
tt_long <- test

# Change Value to Count
names(tt_long)[names(tt_long) == "Value"] <- "Count"

# Remove leading whitespace in Count values
test <- tt_long %>%
  mutate(Count = str_trim(Count))
test$Count <- as.numeric(test$Count)
tt_long <- test

# Change col names
names(tt_long)[names(tt_long) == "CensusTract"] <- "census_tract"
names(tt_long)[names(tt_long) == "TransportationType"] <- "transportation_type"
names(tt_long)[names(tt_long) == "TimeInterval"] <- "time_interval"
names(tt_long)[names(tt_long) == "Count"] <- "count"

# Transform the census_tract column
test <- tt_long %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
tt_long <- test

# Save in new df
travel_time_final <- tt_long

# Change Total to All time intervals 
test <- travel_time_final %>%
  mutate(time_interval = ifelse(str_detect(time_interval, "Total"), "All time intervals", time_interval))

test <- travel_time_final %>%
  mutate(transportation_type = ifelse(str_detect(transportation_type, "All Types"), "All transportation methods", transportation_type))

travel_time_final <- test

# Create df with just urban sprawl indices 
spr_index_upd <- sprIndex %>%
  select(tract, compositeindex2010)

# Rename cols
names(spr_index_upd)[names(spr_index_upd) == "tract"] <- "census_tract"
names(spr_index_upd)[names(spr_index_upd) == "compositeindex2010"] <- "sprawl_index"

# Read demographics data
Sys.setenv(VROOM_CONNECTION_SIZE = 1048576)  # 1 MB = 1048576 bytes
demog <- read_csv("2010 Demographics.csv")

# Transpose data
d_transposed <- as.data.frame(t(demog))
colnames(d_transposed) <- d_transposed[1, ]
d_transposed <- d_transposed[-1, ]
d_transposed <- cbind(census_tract = rownames(d_transposed), d_transposed)
rownames(d_transposed) <- NULL

# Remove margin of error rows 
d_upd <- d_transposed[!grepl("Margin of Error", d_transposed[, 1]), ]

# Separate into total and migrated
d_total <- d_upd[grepl("Total!!Estimate", d_upd$census_tract), ]
d_migr <- d_upd[!grepl("Total!!Estimate", d_upd$census_tract), ]

################ d_total dataframe ################

# Remove leading white space in col names
names(d_total) <- str_trim(names(d_total))

# Remove duplicated column
test <- d_total[, -47]
d_total <- test

# Just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
test <- d_total %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(test$CensusTract)) 
# if false, update df
d_total <- test

# Convert to universal census tract format
test <- d_total %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
d_total <- test

# Remove cols with only NA values 
test <- d_total %>% 
  select_if(~ !all(is.na(.)))
d_total <- test

################ d_migr dataframe ################
names(d_migr)[names(d_migr) == "census_tract"] <- "tract_id"

# Remove duplicated column
test <- d_migr[, -47]
d_migr <- test

# Remove leading white space
names(d_migr) <- str_trim(names(d_migr))

# Just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
test <- d_migr %>%
  mutate(census_tract = str_extract(tract_id, pattern))
# check that there are no null values in CensusTract
any(is.na(test$census_tract)) 
# if false, update df
d_migr <- test

# Convert to universal census tract format
test <- d_migr %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
d_migr <- test

# Create moved_from column 
test <- d_migr %>%
  mutate(moved_from = sub(".*Moved;\\s*", "\\1", tract_id))

# Remove "!!Estimate" from moved_from values
test <- test %>%
  mutate(moved_from = ifelse(str_detect(moved_from, "!!Estimate"), gsub("!!Estimate", "", moved_from), moved_from))

# Remove extra space
test <- test %>%
  mutate(moved_from = ifelse(str_detect(moved_from, "  "), gsub("  ", " ", moved_from), moved_from))
d_migr <- test

# Remove NA columns 
test <- d_migr %>% 
  select_if(~ !all(is.na(.)))
d_migr <- test

# Remove tract_id
d_migr <- d_migr %>%
  select(-tract_id)

# Reorder columns 
test <- d_migr[, c("census_tract", "moved_from", setdiff(names(d_migr), c("census_tract", "moved_from")))]
d_migr <- test

# Read health csv 
health <- read_csv("500_Cities__Local_Data_for_Better_Health__2016_release.csv")

# Get just clark county
health_nv <- health %>%
  filter(StateAbbr == "NV") %>%
  filter(!CityName == "Reno")

# Convert census tract ids
health_nv <- health_nv %>%
  mutate(TractFIPS = substring(TractFIPS, 6))

# Read EJScreen data
ej_screen <- read_csv("EJScreen_2024_Tract_StatePct_with_AS_CNMI_GU_VI.csv")

# Get just LVMA
ej_lvma <- ej_screen %>%
  filter(ST_ABBREV == "NV", CNTY_NAME == "Clark County") %>%
  mutate(ID = substring(ID, 6)) %>%
  select(-OID_)

# Rename ID to census_tract
names(ej_lvma)[names(ej_lvma) == "ID"] <- "census_tract"

# Educational Attainment dataset 
educ_att <- read_csv("S1501 EDUCATIONAL ATTAINMENT.csv")

# Transform data
ea_transposed <- as.data.frame(t(educ_att))
colnames(ea_transposed) <- ea_transposed[1, ]
ea_transposed <- ea_transposed[-1, ]
ea_transposed <- cbind(census_tract = rownames(ea_transposed), ea_transposed)
rownames(ea_transposed) <- NULL

# Remove margin of error rows
ea_upd <- ea_transposed[!grepl("Margin of Error", ea_transposed[, 1]), ]

# Get rid of gender rows, just grab total
test <- ea_upd[grepl("Total", ea_upd[, 1]),]
ea_upd <- test

# Read Vehicle Data 
vehicle <- read_csv('B08141 MEANS OF TRANSPORTATION TO WORK BY VEHICLES.csv')

# Transpose data 
v_transpose <- as.data.frame(t(vehicle[, -1]))
colnames(v_transpose) <- vehicle$`Label (Grouping)`
v_transpose <- cbind(census_tract = rownames(v_transpose), v_transpose)
rownames(v_transpose) <- NULL

# Remove leading white space
names(v_transpose) <- str_trim(names(v_transpose))
v_upd <- v_transpose

# Create a regular expression to extract the transportation method and time interval from the column names
test <- v_upd %>%
  pivot_longer(
    cols = -census_tract,
    names_to = c("transportation_type", "vehicle_availability"),
    names_pattern = "(.*?):? ?(.*)",
    values_to = "count"
  )

# Move transportation methods to transportation_type 
# and change "Total:" to "All" in vehicle_availability
test <- test %>%
  mutate(transportation_type = 
           str_extract(vehicle_availability, "(.*?):"), 
         vehicle_availability = 
           ifelse(str_detect(vehicle_availability, "(.*?):"), "All vehicle availabilities", vehicle_availability))
v_upd <- test

# Change "Total:" to "All transportation methods" in transportation_type
test <- v_upd %>%
  mutate(transportation_type = ifelse(str_detect(transportation_type, "Total:"), "All transportation methods", transportation_type))
v_upd <- test

# Fill in missing values
test <- v_upd %>%
  fill(transportation_type, .direction = "down")
# Remove ":"
test <- test %>%
  mutate(transportation_type = ifelse(str_detect(transportation_type, ":"), gsub(":", "", transportation_type), transportation_type))
v_upd <- test

# Remove margin of error rows 
test <- v_upd[!grepl("Margin of Error", v_upd$census_tract), ]
v_upd <- test

# Just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
test <- v_upd %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(test$census_tract)) 
# if false, update df
v_upd <- test

# Convert to universal census tract format
test <- v_upd %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
v_upd <- test

# Convert count to numeric
v_upd$count <- as.numeric(v_upd$count)
vehicle_final <- v_upd


# Import employment/economic dataset
econ <- read_csv("DP03 SELECTED ECONOMIC CHARACTERISTICS.csv")

# Subset employment 
employment <- econ[1:18,]

# Transpose data 
employ_transp <- as.data.frame(t(employment[, -1]))
colnames(employ_transp) <- employment$`Label (Grouping)`
employ_transp <- cbind(census_tract = rownames(employ_transp), employ_transp)
rownames(employ_transp) <- NULL

# Remove leading white space
names(v_transpose) <- str_trim(names(v_transpose))
v_upd <- v_transpose




# Import work status from the prev 12 months datset
status <- read_csv("S2303 WORK STATUS IN THE PAST 12 MONTHS.csv")

# Transpose data
st_transposed <- as.data.frame(t(status))
colnames(st_transposed) <- st_transposed[1, ]
st_transposed <- st_transposed[-1, ]
st_transposed <- cbind(census_tract = rownames(st_transposed), st_transposed)
rownames(st_transposed) <- NULL

# Remove margin of error rows
st_upd <- st_transposed[!grepl("Margin of Error", st_transposed[, 1]), ]

# Remove gender rows
st_upd <- st_upd[grepl("Total!!Estimate", st_upd$census_tract), ]

# Get weeks worked 
weeks_worked <- st_upd[, 1:9]
weeks_worked <- weeks_worked %>%
  select(-`WEEKS WORKED`)

# Remove leading white space
names(weeks_worked) <- str_trim(names(weeks_worked))

# Just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
test <- weeks_worked %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(test$census_tract)) 
# if false, update df
weeks_worked <- test

# Convert to universal census tract format
test <- weeks_worked %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
weeks_worked <- test

# Get hours worked
hours_worked <- st_upd[, c(1:2, 11:21)]

# Remove leading white space
names(hours_worked) <- str_trim(names(hours_worked))

# Create list of col names 
hrs_weekly <- c("Usually worked 35 or more hours per week", 
                "Usually worked 15 to 34 hours per week",
                "Usually worked 1 to 14 hours per week")
wks_yearly <- c("40 or more weeks", "50 to 52 weeks",
                "40 or more weeks.1", "50 to 52 weeks.1",
                "40 or more weeks.2", "50 to 52 weeks.2")


# Create a regular expression to extract the hours per week and weeks per year from the column names
test <- hours_worked %>%
  pivot_longer(
    cols = hrs_weekly,
    names_to = "hrs_weekly",
    values_to = "hrs_weekly_perc"
  )

test <- test %>%
  pivot_longer(
    cols = wks_yearly,
    names_to = "wks_yearly",
    values_to = "wks_yearly_perc"
  )

# Remove rows with "." in wks_yearly
test2 <- test %>%
  mutate(hrs_weekly = ifelse(
    (hrs_weekly == "Usually worked 35 or more hours per week" & 
       (wks_yearly == "40 or more weeks" | wks_yearly == "50 to 52 weeks")) 
    | 
    (hrs_weekly == "Usually worked 15 to 34 hours per week" & 
       (wks_yearly == "40 or more weeks.1" | wks_yearly == "50 to 52 weeks.1"))
    |
    (hrs_weekly == "Usually worked 1 to 14 hours per week" & 
       (wks_yearly == "40 or more weeks.2" | wks_yearly == "50 to 52 weeks.2")),
    hrs_weekly, "Delete"))
test2 <- test2[!grepl("Delete", test2$hrs_weekly), ]

# update df
hours_worked_upd <- test2 %>%
  select(-`Mean usual hours worked for workers`, -`Did not work`)

# remove ":"
test <- hours_worked_upd %>%
  mutate(wks_yearly = ifelse(str_detect(wks_yearly, "\\.1|\\.2"), gsub("\\.1|\\.2", "", wks_yearly), wks_yearly))
hours_worked_upd <- test

# just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
test <- hours_worked_upd %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(test$census_tract)) 
# if false, update df
hours_worked_upd <- test

# Convert to universal census tract format
test <- hours_worked_upd %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
hours_worked_final <- test

# Create other work df
other_work <- hours_worked %>%
  select(census_tract, `Population 16 to 64 years`, `Mean usual hours worked for workers`, `Did not work`)

# just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
test <- other_work %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(test$census_tract)) 
# if false, update df
other_work <- test

# Convert to universal census tract format
test <- other_work %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
other_work <- test

# Create mean travel time to work 
mean_travel_time <- econ[27, ]

# Transpose data
mtt_transposed <- as.data.frame(t(mean_travel_time))
colnames(mtt_transposed) <- mtt_transposed[1, ]
mtt_transposed <- cbind(census_tract = rownames(mtt_transposed), mtt_transposed)
rownames(mtt_transposed) <- NULL
mtt_transposed <- mtt_transposed[-1, ]

# Remove margin of error and percent rows
mtt_transposed <- mtt_transposed[!grepl("Margin of Error", mtt_transposed[, 1]), ]
mtt_transposed <- mtt_transposed[!grepl("Percent", mtt_transposed[, 1]), ]

# Remove leading white space
names(mtt_transposed) <- str_trim(names(mtt_transposed))

# just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
mtt_transposed <- mtt_transposed %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(mtt_transposed$census_tract)) 

# Convert to universal census tract format
mtt_transposed <- mtt_transposed %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
mean_travel_time <- mtt_transposed

# Industry and occupation
employ_cc <- econ[28:49,]
occupation <- employ_cc[2:7, ]
industry <- employ_cc[9:22,]

# Occupation
# Transpose
occ_transposed <- as.data.frame(t(occupation))
colnames(occ_transposed) <- occ_transposed[1, ]
occ_transposed <- occ_transposed[-1, ]
occ_transposed <- cbind(census_tract = rownames(occ_transposed), occ_transposed)
rownames(occ_transposed) <- NULL

# remove leading white space
names(occ_transposed) <- str_trim(names(occ_transposed))

# remove margin of error rows 
occ_transposed <- occ_transposed[!grepl("Margin of Error", occ_transposed[, 1]), ]

# remove percent rows 
occ_transposed <- occ_transposed[!grepl("Percent", occ_transposed[, 1]), ]

# just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
occ_transposed <- occ_transposed %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(occ_transposed$census_tract)) 

# Convert to universal census tract format
occ_transposed <- occ_transposed %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
occupation <- occ_transposed


# Industry
# Transpose
ind_transposed <- as.data.frame(t(industry))
colnames(ind_transposed) <- ind_transposed[1, ]
ind_transposed <- ind_transposed[-1, ]
ind_transposed <- cbind(census_tract = rownames(ind_transposed), ind_transposed)
rownames(ind_transposed) <- NULL

# remove leading white space
names(ind_transposed) <- str_trim(names(ind_transposed))

# remove margin of error rows 
ind_transposed <- ind_transposed[!grepl("Margin of Error", ind_transposed[, 1]), ]

# remove percent rows 
ind_transposed <- ind_transposed[!grepl("Percent", ind_transposed[, 1]), ]

# just grab census tract ids
pattern <- "\\b(\\d+\\.\\d+|\\d+\\s\\d+\\.\\d+|\\d+)\\b"
ind_transposed <- ind_transposed %>%
  mutate(census_tract = str_extract(census_tract, pattern))
# check that there are no null values in CensusTract
any(is.na(ind_transposed$census_tract)) 

# Convert to universal census tract format
ind_transposed <- ind_transposed %>%
  mutate(census_tract = sapply(census_tract, transform_census_tract))
industry <- ind_transposed


# Export datasets
# Create a new workbook
wb <- createWorkbook()

# Add data frame 1 as the first sheet
addWorksheet(wb, "demographics")
writeData(wb, sheet = "demographics", d_total)

# Add data frame 2 as the second sheet
addWorksheet(wb, "work_commute_time")
writeData(wb, sheet = "work_commute_time", travel_time_final)

# Add data frame 2 as the second sheet
addWorksheet(wb, "sprawl_index")
writeData(wb, sheet = "sprawl_index", spr_index_upd)

# Add data frame 2 as the second sheet
addWorksheet(wb, "transportation_vehicle_avail")
writeData(wb, sheet = "transportation_vehicle_avail", vehicle_final)

# Add data frame 2 as the second sheet
addWorksheet(wb, "weeks_worked")
writeData(wb, sheet = "weeks_worked", weeks_worked)

# Add data frame 2 as the second sheet
addWorksheet(wb, "hours_worked")
writeData(wb, sheet = "hours_worked", hours_worked_final)

# Add data frame 2 as the second sheet
addWorksheet(wb, "other_work")
writeData(wb, sheet = "other_work", other_work)

# Add data frame 2 as the second sheet
addWorksheet(wb, "mean_travel_time")
writeData(wb, sheet = "mean_travel_time", mean_travel_time)

# Add data frame 2 as the second sheet
addWorksheet(wb, "occupation")
writeData(wb, sheet = "occupation", occupation)

# Add data frame 2 as the second sheet
addWorksheet(wb, "industry")
writeData(wb, sheet = "industry", industry)

# Save the workbook to an Excel file
saveWorkbook(wb, file = "thesis_data.xlsx", overwrite = TRUE)
