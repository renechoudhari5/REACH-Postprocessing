# V2 of this script only cleans EMA data, not Saliva data, from Curious export

rm(list=ls())
library(tidyverse)
library(dplyr)
library(stringr)

------- # IMPORTING DATA 

# Reading in all uncleaned flow data to date - UPDATE DATE EACH TIME!
master <- read_csv("~/NIMH EMA Data/Output Files/12-1-25/flow_final.csv")
  
# Reading in uncleaned flow data from previous export - UPDATE DATE EACH TIME!
prev_export <- read_csv("~/NIMH EMA Data/Output Files/9-9-25/flow_final.csv")

# Removing admin accounts
master = master[!(master$secret_user_id %in% c("9999-999", "[admin account] (2628b895-18fa-4cf3-a2b4-ec7253aabde4)",
                                               "[admin account] (a97ac66e-d8da-4427-bfd5-7e4df4059f05)",
                                               "[admin account] (53254279-8d3b-444a-9d36-c6ba11063d61)")),]

# Isolating entries in new export (removing entries that were in previous export)
current_export = anti_join(master, prev_export, by = c("userId", "activity_schedule_id", "secret_user_id", "schedule_Time"))

# Identify regular EMA vs. saliva
current_export$saliva = ifelse(str_detect(current_export$activity_flow, "saliva"),1,0)

# Removing saliva entries
current_ema = current_export %>% filter(saliva == 0)

------- # METADATA VARIABLE CREATION

# Identify missed entries (no start time)
current_ema$missed = ifelse(is.na(current_ema$start_Time),1,0)

# Identify completed entries (response to now_pain)
current_ema$completed = ifelse(is.na(current_ema$now_pain),0,1)

# Extract time of day and date
current_ema$tod = word(current_ema$activity_flow)
current_ema$date = as.Date(current_ema$schedule_Time)
current_ema$dow = weekdays(as.Date(current_ema$schedule_Time))

# Extract survey duration (secs)
current_ema$duration = as.numeric(current_ema$end_Time - current_ema$start_Time)

------- # SETTING UP CLEANING FUNCTIONS

# Empty dates: check for a full day of missed data
get_empty_dates <- function(dat, user, date, missed){
    empty_dates <- dat %>%
      group_by({{ user }}, {{ date }}) %>%
      mutate(
        day_empty = sum({{ missed }}) == length({{ missed }})
      ) %>%
      filter(day_empty) %>%
      mutate(empty_day = "yes") %>%
      ungroup()
    View(empty_dates)
    return(empty_dates)
  }

# Duplicate assessments: check that no assessment appears more than once per day
get_duplicates <- function(dat, user, date, activity) {
  duplicates <- dat %>%
    group_by({{ user }}, {{ date }}, {{ activity }}) %>%
    mutate(repeats = n()) %>%
    filter(repeats != 1) %>%
    mutate(duplicate_assessments = "yes") %>%
    ungroup()
  
  View(duplicates)
  return(duplicates)
}

# Unscheduled assessments: check that all assessments have scheduled dates
get_unscheduled <- function(dat, user, date) {
  unscheduled <- dat %>%
    group_by({{ user }}) %>%
    filter(is.na({{ date }})) %>%
    mutate(unscheduled_assessments = "yes") %>%
    ungroup()
  
  View(unscheduled)
  return(unscheduled)
}

# Random and missing assessments: checks that on each day all assessments appear once

EMA_mandatory_assessments <- c("Morning Assessment", "Mid-day Assessment", "Afternoon Assessment")
EMA_evening_options <- c("Evening Assessment", "Evening Assessment (Female)")
# saliva_mandatory_assessments <- c("Morning Assessment (with saliva)",
#                                   "Mid-day Assessment (with saliva)",
#                                   "Afternoon Assessment (with saliva)")
# saliva_evening_options <- c("Evening Assessment (with saliva)",
#                             "Evening Assessment (Female; with saliva)")

get_missing_random <- function(dat, saliva_value, mandatory_assessments, evening_options){
  missing_random <- dat %>%
    filter(saliva == saliva_value & !is.na(date)) %>%
    group_by(secret_user_id, date) %>% 
    mutate(
      present = list(unique(activity_flow)),
      absent = list(setdiff(mandatory_assessments, present[[1]])),
      has_evening = any(evening_options %in% present[[1]]),
      mult_evening = sum(evening_options %in% present[[1]]) > 1) %>%
    mutate(absent = ifelse(
      !has_evening,
      lapply(absent, function(x) c(x, "Evening")),
      absent
    )) %>%
    filter(lengths(absent) > 0 | mult_evening) %>%
    mutate(missing_random = "yes") %>%
    select(-c("has_evening","mult_evening")) %>%
    ungroup()
  
  View(missing_random)
  return(missing_random)
}

# Long assessments: check that all assessments were finished in <= 1hour
get_long <- function(dat, duration){
  long <- dat %>% filter(duration >3600)
  View(long)
  return(long)
}

------- # PX REVIEW
  
## To reference a participant's full uncleaned dataset: 
this_id <- master %>% filter(secret_user_id == "2289-001") # focusing on one subject

------- # CLEANING

--- # Empty dates
  
# 1. Find empty dates and manually review
empty_dates <- get_empty_dates(current_ema, secret_user_id, date, missed)

# 2. Identify which dates should be removed (do not include dates of true non-compliance)
incorrect_dates <- empty_dates %>% filter(
  (secret_user_id == "2136-001" & date == "2024-08-05") |
    (secret_user_id == "2133-001" & between(date, as.Date("2025-02-22"),as.Date("2025-02-25"))) |
    (secret_user_id == "2137-001" & date == "2024-08-16") |
    (secret_user_id == "2143-002" & date == "2024-09-11") |
    (secret_user_id == "2146-001" & date == "2024-08-05") |
    (secret_user_id == "2146-012" & date == "2024-08-05") |
    (secret_user_id == "2157-033" & between(date, as.Date("2025-01-03"),as.Date("2025-01-15"))) |
    (secret_user_id == "2157-034" & between(date, as.Date("2024-12-28"),as.Date("2025-01-11"))) |
    (secret_user_id == "2161-001" & date == "2024-10-31") |
    (secret_user_id == "2161-001" & date == "2024-12-21") |
    (secret_user_id == "2184-048" & date == "2024-12-12") |
    (secret_user_id == "2190-001" & between(date, as.Date("2025-02-07"),as.Date("2025-02-10"))) |
    (secret_user_id == "2192-001" & date == "2024-08-30") |
    (secret_user_id == "2192-002" & date == "2024-08-30") |
    (secret_user_id == "2195-001" & between(date, as.Date("2025-07-04"),as.Date("2025-07-07"))) |
    (secret_user_id == "2200-001" & between(date, as.Date("2024-09-21"),as.Date("2024-10-03"))) |
    (secret_user_id == "2205-001" & date == "2025-01-18") |
    (secret_user_id == "2208-002" & date == "2024-08-28") |
    (secret_user_id == "2212-001" & between(date, as.Date("2024-09-04"),as.Date("2024-09-09"))) |
    (secret_user_id == "2214-001" & between(date, as.Date("2025-02-28"),as.Date("2025-03-04"))) |
    (secret_user_id == "2217-001" & between(date, as.Date("2024-11-06"),as.Date("2024-11-20"))) |
    (secret_user_id == "2217-001" & between(date, as.Date("2024-12-28"),as.Date("2024-12-31"))) |
    (secret_user_id == "2239-001" & between(date, as.Date("2025-05-06"),as.Date("2025-05-07"))) |
    (secret_user_id == "2243-001" & between(date, as.Date("2025-04-01"),as.Date("2025-04-03"))) |
    (secret_user_id == "2248-001" & date == "2025-02-06") |
    (secret_user_id == "2190-002" & between(date, as.Date("2025-06-10"),as.Date("2025-06-14"))) |
    (secret_user_id == "2208-033" & between(date, as.Date("2025-06-13"),as.Date("2025-06-16"))) |
    (secret_user_id == "2228-001" & between(date, as.Date("2025-06-27"),as.Date("2025-06-29"))) |
    (secret_user_id == "2261-001" & between(date, as.Date("2025-06-12"),as.Date("2025-06-15"))) |
    (secret_user_id == "2232-017" & between(date, as.Date("2025-10-08"),as.Date("2025-10-22")))
)

length(unique(incorrect_dates$secret_user_id)) # num of participants w incorrect dates in current EMA export

# 3. Remove incorrect dates from the cleaned dataset
current_ema_clean <- current_ema %>% anti_join(incorrect_dates)

--- # Duplicate assessments
  
# 1. Find duplicate assessments and manually review
duplicates <- get_duplicates(current_ema_clean, secret_user_id, date, activity_flow)

# 2. Identify which assessments should be removed
duplicates_to_remove <- duplicates %>% filter(
  (secret_user_id == "2124-001" & schedule_Time == as.POSIXct("2024-08-15 08:00:00", tz = "UTC")) |
    (secret_user_id == "2124-001" & schedule_Time == as.POSIXct("2024-08-16 08:00:00", tz = "UTC")) |
    (secret_user_id == "2124-001" & schedule_Time == as.POSIXct("2024-08-19 08:00:00", tz = "UTC")) |
    (secret_user_id == "2124-001" & schedule_Time == as.POSIXct("2024-08-20 08:00:00", tz = "UTC")) |
    (secret_user_id == "2124-001" & schedule_Time == as.POSIXct("2024-08-21 08:00:00", tz = "UTC")) |
    (secret_user_id == "2124-001" & schedule_Time == as.POSIXct("2024-08-23 08:00:00", tz = "UTC")) |
    (secret_user_id == "2190-001" & schedule_Time == as.POSIXct("2024-10-02 07:00:00", tz = "UTC")) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-10-30 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-10-31 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-01 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-02 00:00:00", tz = "UTC")) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-04 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-05 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-07 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-08 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-09 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-10 23:00:00", tz = "UTC") & activity_schedule_id == "82c22b15-a3f4-41e7-ae6b-7ebe12fa720c") |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-11 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-12 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-13 23:00:00", tz = "UTC") & is.na(start_Time)) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-13 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-14 10:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-15 10:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-16 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-18 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-19 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-20 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-21 10:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-22 10:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-23 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-25 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-26 08:00:00", tz = "UTC")) |
    (secret_user_id == "2217-001" & schedule_Time == as.POSIXct("2024-12-27 08:00:00", tz = "UTC")) |
    (secret_user_id == "2232-001" & schedule_Time == as.POSIXct("2024-10-10 21:00:00", tz = "UTC")) |
    (secret_user_id == "2248-001" & schedule_Time == as.POSIXct("2024-11-26 23:00:00", tz = "UTC")) |
    (secret_user_id == "2248-001" & schedule_Time == as.POSIXct("2024-11-27 23:00:00", tz = "UTC")) |
    (secret_user_id == "2248-001" & schedule_Time == as.POSIXct("2024-11-28 17:30:00", tz = "UTC")) |
    (secret_user_id == "2260-001" & schedule_Time == as.POSIXct("2025-03-17 11:00:00", tz = "UTC")) |
    (secret_user_id == "2127-002" & schedule_Time == as.POSIXct("2025-04-18 16:30:00", tz = "UTC")) |
    (secret_user_id == "2127-002" & schedule_Time == as.POSIXct("2025-04-18 20:30:00", tz = "UTC")) |
    (secret_user_id == "2208-001" & schedule_Time == as.POSIXct("2025-07-11 14:00:00", tz = "UTC")) |
    (secret_user_id == "2208-001" & schedule_Time == as.POSIXct("2025-07-12 14:00:00", tz = "UTC")) |
    (secret_user_id == "2208-001" & schedule_Time == as.POSIXct("2025-07-13 14:00:00", tz = "UTC")) |
    (secret_user_id == "2208-001" & schedule_Time == as.POSIXct("2025-07-14 14:00:00", tz = "UTC")) |
    (secret_user_id == "2208-001" & schedule_Time == as.POSIXct("2025-07-15 14:00:00", tz = "UTC")) |
    (secret_user_id == "2208-001" & schedule_Time == as.POSIXct("2025-07-17 14:00:00", tz = "UTC")) |
    (secret_user_id == "2208-032" & schedule_Time == as.POSIXct("2025-06-25 11:00:00", tz = "UTC")) |
    (secret_user_id == "2208-032" & schedule_Time == as.POSIXct("2025-06-26 22:00:00", tz = "UTC")) |
    (secret_user_id == "2208-032" & schedule_Time == as.POSIXct("2025-06-28 22:00:00", tz = "UTC")) |
    (secret_user_id == "2208-033" & schedule_Time == as.POSIXct("2025-06-18 22:00:00", tz = "UTC"))
  )

length(unique(duplicates_to_remove$secret_user_id)) # num of participants w duplicates in current EMA export

# 3. Removing duplicate assessments from the cleaned datset
current_ema_clean <- current_ema_clean %>% anti_join(duplicates_to_remove)

--- # Unscheduled assessments
  
# 1. Find unscheduled assessments and manually review
unscheduled <- get_unscheduled(current_ema_clean, secret_user_id, date)

length(unique(unscheduled$secret_user_id)) # num of participants w unscheduled assessments

# 2. Remove unscheduled assessments from the cleaned dataset
current_ema_clean <- current_ema_clean %>% anti_join(unscheduled)

--- # Random and missing assessments
  
# 1. Find random and missing assessments and manually review
missing_random <- get_missing_random(current_ema_clean, 0, EMA_mandatory_assessments, EMA_evening_options)

# affected_assessments <- missing_random %>% filter(secret_user_id != "2286-001")
# write_csv(affected_assessments, "affected_assessments.csv")
# 
# affected_assessments <- left_join(affected_assessments, prev_export, by = c("userId", "activity_schedule_id", "secret_user_id", "activity_flow", "schedule_Time", "start_Time", "end_Time"))
# affected_assessments$activity_submission_id_new <- affected_assessments$activity_submission_id.x
# affected_assessments$activity_submission_id_old <- affected_assessments$activity_submission_id.y
# affected_assessments <- affected_assessments %>% select("userId", "activity_schedule_id", "activity_submission_id_new", "activity_submission_id_old", "secret_user_id", "activity_flow", "schedule_Time", "start_Time", "end_Time")


# Combining EMA and saliva missing_random if needed
# missing_random <- bind_rows(EMA_missing_random, saliva_missing_random) %>%
#   arrange(secret_user_id, date)

length(unique(missing_random$secret_user_id)) # num of participants w missing and random

# 2. Identify which random assessments should be removed (do not remove any existing assessments on days with missing assessments)
random_to_remove <- missing_random %>% filter(
  (secret_user_id == "2214-001" & date == "2025-02-08") |
    (secret_user_id == "2216-001" & date == "2025-01-04") |
    (secret_user_id == "2216-001" & date == "2025-01-11") |
    (secret_user_id == "2217-001" & date == "2024-12-12") |
    (secret_user_id == "2243-001" & between(date, as.Date("2025-04-19"),as.Date("2025-05-05"))) |
    (secret_user_id == "2261-001" & date == "2025-05-24") |
    (secret_user_id == "2264-001" & between(date, as.Date("2025-04-21"),as.Date("2025-05-02"))) |
    (secret_user_id == "2264-001" & date == "2025-05-05") |
    (secret_user_id == "2190-002" & date == "2025-06-10")
)

# 3. Remove random assessments from the cleaned dataset
current_ema_clean <- current_ema_clean %>% anti_join(random_to_remove)

# 4. Flag days that have missing assessments
current_ema_clean <- current_ema_clean %>%
  mutate(flag_missing_assessments = ifelse(
    (secret_user_id == "2146-012" & date == "2025-02-28") |
    (secret_user_id == "2216-001" & date == "2024-12-21") |
    (secret_user_id == "2216-001" & date == "2024-12-22") |
    (secret_user_id == "2216-001" & date == "2024-12-29") |
    (secret_user_id == "2220-001" & between(date, as.Date("2025-03-06"),as.Date("2025-03-09"))) |
    (secret_user_id == "2260-001" & date == "2025-03-13") |
    (secret_user_id == "2264-001" & date == "2025-04-06") |
    (secret_user_id == "2289-001" & date == "2025-09-16"), 1, 0))

--- # Long assessments
# 1. Find long assessments (after other cleaning is done) and check that nrow=0

get_long(current_ema_clean, duration) %>% nrow()

------- # TECHNICAL ISSUES AND DROP-OUT FLAGS
  
--- # Technical Issues
# 1. Flag participants who were affected by technical issues, bugs, etc.

current_ema_clean <- current_ema_clean %>%
  mutate(flag_tech = ifelse((secret_user_id == "2211-001") |
                         (secret_user_id == "2215-001") |
                         (secret_user_id == "2216-001") |
                         (secret_user_id == "2217-001"), 1, 0))

--- # Drop-outs
# 1. Flag participants who officially dropped out

current_ema_clean <- current_ema_clean %>%
  mutate(flag_dropout = ifelse((secret_user_id == "2161-001") |
                                  (secret_user_id == "2163-001") |
                                  (secret_user_id == "2207-001"), 1, 0))

--- # Other
# 1. Flag participants for any other reason (to still be included in compliance calculations)
## As of now this includes the government shutdown 2025, and Curious bug version 2025.09.2
  
current_ema_clean <- current_ema_clean %>%
  mutate(flag_other = ifelse((secret_user_id == "2289-001") |
                               (secret_user_id == "2305-001") |
                               (secret_user_id == "2231-001") |
                               (secret_user_id == "2300-001") |
                               (secret_user_id == "2306-001") |
                               (secret_user_id == "2233-001") |
                               (secret_user_id == "2290-001") |
                               (secret_user_id == "2291-001") |
                               (secret_user_id == "2197-020") |
                               (secret_user_id == "2197-037") |
                               (secret_user_id == "2286-001") |
                               (secret_user_id == "2272-001"), 1, 0))

------- # CLEANED DATA SETS
  
# Reading in previously cleaned EMA dataset (previous export's cleaned file)
prev_ema_cleaned <- read_csv("~/NIMH EMA Data/Cleaned data/8-1-25/ema_clean_080125.csv")

# Aligning previous export's variable types to conform to current export

# prev_ema_cleaned$since_food1 <- as.character(prev_ema_cleaned$since_food1)
# prev_ema_cleaned$since_food2 <- as.character(prev_ema_cleaned$since_food2)
# prev_ema_cleaned$since_food3 <- as.character(prev_ema_cleaned$since_food3)
# prev_ema_cleaned$since_food4 <- as.character(prev_ema_cleaned$since_food4)
# prev_ema_cleaned$since_food5 <- as.character(prev_ema_cleaned$since_food5)
prev_ema_cleaned$since_had_drink_alcohol_type <- as.character(prev_ema_cleaned$since_had_drink_alcohol_type)
prev_ema_cleaned$substances_tobacco <- as.double(prev_ema_cleaned$substances_tobacco)
prev_ema_cleaned$since_cannabis_type <- as.character(prev_ema_cleaned$since_cannabis_type)
prev_ema_cleaned$headache_location <- as.character(prev_ema_cleaned$headache_location)
prev_ema_cleaned$headache_vision_change_time <- as.character(prev_ema_cleaned$headache_vision_change_time)
# 
prev_ema_cleaned$headache_numbing_time <- as.character(prev_ema_cleaned$headache_numbing_time)
prev_ema_cleaned$headache_medication <- as.character(prev_ema_cleaned$headache_medication)
# prev_ema_cleaned$day_over_medication_why <- as.character(prev_ema_cleaned$day_over_medication_why)
# prev_ema_cleaned$day_problems_belly_symptoms <- as.character(prev_ema_cleaned$day_problems_belly_symptoms)
# 
# prev_ema_cleaned$saliva_label <- as.character(prev_ema_cleaned$saliva_label)

# Checking for observations that straddled exports (present in both prev_ema_cleaned and current_ema_cleaned) and
# removing these obs from prev_ema_cleaned 
key_cols <- c("secret_user_id", "date", "activity_flow")
in_both <- prev_ema_cleaned %>% semi_join(current_ema_clean, by = key_cols)

prev_ema_cleaned <- prev_ema_cleaned %>% anti_join(in_both)

# Merging current export's cleaned EMA data with previous export's cleaned EMA data
ema_clean <- bind_rows(current_ema_clean, prev_ema_cleaned) %>% arrange(secret_user_id, schedule_Time)

# View full EMA cleaned dataset to date
View(ema_clean)

------- # STRADDLE CHECK
  
# Manual final cleaning
to_remove <- ema_clean %>% filter(
    (secret_user_id == "2217-001" & date == "2025-04-03")
  )
ema_clean <- ema_clean %>% anti_join(to_remove)

# Now run ema_clean through the cleaning functions again to make sure there's no straddle issues

get_empty_dates(ema_clean, secret_user_id, date, missed)
get_duplicates(ema_clean, secret_user_id, date, activity_flow)
get_unscheduled(ema_clean, secret_user_id, date)
get_missing_random(ema_clean, 0, EMA_mandatory_assessments, EMA_evening_options)
get_long(ema_clean, duration)


------- # EXPORTING CLEANED EMA DATA

# Export csv of cleaned EMA data - UPDATE NAME EACH TIME! check where saving.
write_csv(ema_clean, "ema_clean_120125.csv")

------- # COMPLIANCE

# Create sample to be used for compliance calculations, excluding those with tech issues and drop-outs
ema_clean_forcomp <- ema_clean %>% filter(flag_tech == 0 & flag_dropout == 0) 

# Create sample of fully complete EMA responses
ema_complete_forcomp <- ema_clean_forcomp %>% filter(completed == 1)

--- # Sample-wide summary

## How many participants have completed the EMA to date?
number_completed = length(unique(ema_clean$secret_user_id))

## How many participants are considered in the compliance calculations? (excl tech issues and drop-outs)
number_forcomp = length(unique(ema_clean_forcomp$secret_user_id))

## How many participants have fully complete EMA responses? (excl incomplete responses)
number_complete = length(unique(ema_complete_forcomp$secret_user_id))


## What is the total compliance of the sample?
total_compliance = 1 - sum(ema_clean_forcomp$missed)/nrow(ema_clean_forcomp)

## What is the total compliance for completed assessments?/What percent of assessments are fully completed?
complete_compliance = nrow(ema_complete_forcomp)/nrow(ema_clean_forcomp)


## On average, how long does it take participants to complete the EMA, in minutes?
summary(ema_clean_forcomp$duration/60) # all surveys
sd(ema_clean_forcomp$duration, na.rm = TRUE)/60
summary(ema_complete_forcomp$duration/60) # completed surveys
sd(ema_complete_forcomp$duration, na.rm = TRUE)/60

--- # Subject-level compliance
  
# Obtaining percent compliance and # assessments completed for each participant
compliance_subj = ema_clean_forcomp %>%
  group_by(secret_user_id) %>% 
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  arrange(secret_user_id) %>%
   ungroup()
View(compliance_subj)

# Export table of compliance for each participant
write_csv(compliance_subj, "px_compliance_120125.csv")


## How many participants have 75% compliance or above?
compliance_subj %>% filter(pct_compliance >= 75) %>% nrow()

## How many participants have below 25% compliance?
compliance_subj %>% filter(pct_compliance < 25) %>% nrow()

## What does the compliance distribution look like?
hist(compliance_subj$pct_compliance)

## Which participants have low compliance (<25%)?
low_compliance <- compliance_subj %>% filter(pct_compliance < 25) %>% arrange(secret_user_id)
View(low_compliance)


## Does compliance vary by time of day and day of week?
compliance_subj_tod = ema_clean_forcomp %>% ## by subject x time of day
  group_by(secret_user_id, activity_flow) %>% 
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  # filter(secret_user_id == "2146-012") %>% ## for a specific subject
  ungroup()
View(compliance_subj_tod)

compliance_tod = ema_clean_forcomp %>% ## by time of day
  group_by(tod) %>%
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  ungroup()
View(compliance_tod)

compliance_dow = ema_clean_forcomp %>% ## by day of week
  group_by(dow) %>%
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  ungroup()
View(compliance_dow)


------- # De-identifying raw data

# Set the working directory to where your CSVs live
setwd("NIMH EMA Data/Input Files/9:9:25/EMA_applet_data")

# List all CSV files in the folder
files <- list.files(pattern = "\\.csv$")

# Names of the columns you want to remove
cols_to_remove <- c("secret_user_id", "target_secret_id", "source_secret_id", "input_secret_id", "item_response", "rawScore")

# Loop through each file
for (f in files) {
  dat <- read.csv(f, stringsAsFactors = FALSE)
  
  # Remove columns if they exist
  dat <- dat[, !(names(dat) %in% cols_to_remove)]
  
  # Write back to CSV (example: prefix with "cleaned_")
  write.csv(dat, paste0("cleaned_", f), row.names = FALSE)
}

------- # PURGATORY: cleaning
  
empty_dates <- current_ema %>%
  group_by(secret_user_id, date) %>%
  mutate(
    day_empty = sum(missed) == length(missed)
  ) %>%
  filter(day_empty) %>%
  mutate(empty_day = "yes")

# 2. Manually review dates that are incorrect
View(empty_dates)


duplicates <- current_ema_clean %>% 
  group_by(secret_user_id, date, activity_flow) %>%
  mutate(repeats = n()) %>%
  filter(repeats != 1) %>%
  mutate(duplicate_assessments = "yes")

unscheduled <- dat_clean %>%
  group_by(secret_user_id) %>%
  filter(is.na(date)) %>%
  mutate(unscheduled_assessments = "yes")


------- # PURGATORY: # of assessments
  
  # NUMBER OF ASSESSMENTS
  # Expected total number of assessments for EMA: 60
  # Expected total number of assessments for EMA + saliva: 76
  # Expected number of normal Morning, Mid-Day, Afternoon, and Evening: 15
  # Expected number of each assessment with saliva: 4
  
assessments <- dat_flow %>% # number of each type of assessment for each participant
  group_by(secret_user_id, activity_flow) %>% 
  summarise(n=n()) %>%
  arrange(n) %>%
  ungroup()
View(assessments)

## Which subjects don't have the expected number of assessments?

unexpected <- assessments %>% filter(activity_flow == "Morning Assessment" & n != 15 |
                                       activity_flow == "Mid-day Assessment" & n != 15 |
                                       activity_flow == "Afternoon Assessment" & n != 15 |
                                       activity_flow == "Evening Assessment" & n != 15 |
                                       activity_flow == "Evening Assessment (Female)" & n != 15 |
                                       activity_flow == "Morning Assessment (with saliva)" & n != 4 |
                                       activity_flow == "Mid-day Assessment (with saliva)" & n != 4 |
                                       activity_flow == "Afternoon Assessment (with saliva)" & n != 4 |
                                       activity_flow == "Evening Assessment (with saliva)" & n != 4 |
                                       activity_flow == "Evening Assessment (Female; with saliva)" & n != 4) %>%
  arrange(secret_user_id)
View(unexpected) # all assessments with unexpected # of occurrences

# to_review_ids <- unique(unexpected$secret_user_id) # participant IDs to review
# 
# to_review <- dat_flow %>% filter(secret_user_id %in% to_review_ids) %>%
#   select(-c("userId", "event_id", "id")) # full data for the participants to review
# View(to_review)

  # ## How many participants have too few assessments?
  # dat_ema_few <- assessments %>% filter(n < 60) %>% arrange(desc(n))
  # View(dat_ema_few)
  # 
  # ## How many participants have between 60 and 76 assessments?
  # dat_ema_med <- dat_ema_compliance_subj %>% filter(n > 60 & n < 76) %>% arrange(desc(n))
  # View(dat_ema_med)
  # 
  # ## How many participants have > 76 assessments?
  # dat_ema_many <- dat_ema_compliance_subj %>% filter(n > 76) %>% arrange(desc(n))
  # View(dat_ema_many)
  