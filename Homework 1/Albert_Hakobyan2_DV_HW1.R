library(dplyr)

#Part 1: task 1

data <- read.csv('crime_data.csv')
head(data, 5)

missing_counts <- colSums(is.na(data))
print(missing_counts)
missing_cols <- missing_counts[missing_counts > 0]  # storing only those columns where at least 1 value is missing
print(missing_cols)
cols_to_drop <- names(missing_counts[missing_counts > 0.5 * nrow(data)])

# Checking if more than 50% of LAT/LON values are 0. If so, then I add them to drop list 
# (These null values were described to be treated as missing values in the HW document)
if(sum(data$LAT == 0) > 0.5 * nrow(data)) {
  cols_to_drop <- c(cols_to_drop, "LAT")
}

if(sum(data$LON == 0) > 0.5 * nrow(data)) {
  cols_to_drop <- c(cols_to_drop, "LON")
}

data_filtered <- data %>% select(-all_of(cols_to_drop)) # Dropping the selected columns

print("Dropped columns:")
print(cols_to_drop)

head(data_filtered, 5)


# task 3
library(lubridate)

# Converting DATE.OCC to datetime format (as.Date method removes the time component, so I chose not to use it, instead I use time series library: lubridate for this task)
data_filtered <- data_filtered %>%
  mutate(
    DATE.OCC = mdy_hms(DATE.OCC),
    YEAR = year(DATE.OCC),
    MONTH = month(DATE.OCC),
    DAY = day(DATE.OCC))

df <- data_filtered %>%
  mutate(HOUR = as.numeric(substr(TIME.OCC, 1, 2)))  #Extracting the first 2 digits as the hour

head(df, 5)
str(df)

# task 4:

df <- df %>%
  filter(YEAR == 2023, Crm.Cd.Desc == "BURGLARY")

head(df, 5)

# task 5: Grouping the data by AREA NAME and calculate the total number of crimes and the average victim age. 
#         Sorting the results by total crimes in descending order.

df %>%
  group_by(AREA.NAME) %>% 
    summarize(
      total_crimes = n(),
      avg_vict_age = floor(mean(Vict.Age, na.rm = TRUE))) %>%  # Getting the average victim age, ignoring missing values
  arrange(desc(total_crimes))

# PART 3:
df %>%
  group_by(MONTH) %>%
  summarize(total_crimes = n())

data %>%
  filter(!is.na(Weapon.Used.Cd)) %>%
  count()

df %>%
  group_by(Premis.Desc) %>%
  summarize(number_of_crimes = n()) # I don't understand why there are misaligned or shifted values in the Premis.Desc column in the output. 

#Part 4: Advanced Analysis (Python & R) 20 points per problem: 10 for Python and 10 for R 
#1. Create a new column, Severity Score, based on the following rules: 
#   • Assign 5 points if a weapon was used (Weapon Used Cd is not null). 
#   • Assign 3 points for crimes under BURGLARY. 
#   • Assign 1 point for all other crimes. 
#   • Group by AREA NAME and find the total severity score for each area. 

df <- df %>%
  left_join(data %>% select(DR_NO, Weapon.Used.Cd), by = "DR_NO") %>% # Merging back the dropped column 'Weapon.Used.Cd' based on 'DR_NO'
  mutate(
    Severity_Score = case_when(
      !is.na(Weapon.Used.Cd) ~ 5,
      Crm.Cd.Desc == "BURGLARY" ~ 3,
      TRUE ~ 1)) 

df %>%
  group_by(AREA.NAME) %>%
  summarize(total_severity_score = sum(Severity_Score)) 

head(df, 5)


#Bonus Part: 5 points Use the LAT and LON columns to identify crimes that occurred within a specific latitude-longitude bounding box (e.g., downtown area).

# Bellow I picked some random ranges for Latitude and longitude (max and min values according to the data's row information) 
# because there was no specific instruction provided. I don't know if this bonus question must be this easy or not, anyway, here is my solution:
min_lat <- 34.01
min_lon <- -118.5
max_lat <- 34.3
max_lon <- -118.02

df_bonus <- df %>%
  filter(LAT >= min_lat & LAT <= max_lat, LON >= min_lon & LON <= max_lon)

head(df_bonus, 5)
# tail(df_bonus, 5)