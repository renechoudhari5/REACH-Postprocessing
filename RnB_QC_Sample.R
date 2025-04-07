rm(list=ls())
library(tidyverse)
library(dplyr)
library(stringr)

------- # IMPORTING DATA 
## Reading in flow data
dat_flow <- read_csv("NIMH EMA Data/Output Files/flow_final.csv")

## Remove admin entries
dat_flow = dat_flow[!(dat_flow$secret_user_id %in% c("9999-999", "[admin account] (2628b895-18fa-4cf3-a2b4-ec7253aabde4)",
                                                                 "[admin account] (a97ac66e-d8da-4427-bfd5-7e4df4059f05)")),]
------- # METADATA VARIABLE CREATION

## Identify missed entries (no start time)
dat_flow$missed = ifelse(is.na(dat_flow$start_Time),1,0)

## Identify completed entries (response to now_pain)
dat_flow$completed = ifelse(is.na(dat_flow$now_pain),0,1)

## Extract time of day and date
dat_flow$tod = word(dat_flow$activity_flow)
dat_flow$date = as.Date(dat_flow$schedule_Time)
dat_flow$dow = weekdays(as.Date(dat_flow$schedule_Time))

## Extract survey duration (secs)
dat_flow$duration = as.numeric(dat_flow$end_Time - dat_flow$start_Time)

## Identify regular EMA vs. saliva
dat_flow$saliva = ifelse(str_detect(dat_flow$activity_flow, "saliva"),1,0)

View(dat_flow)
glimpse(dat_flow)

------- # CLEANING
  
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
# # this_id <- to_review %>% filter(secret_user_id == "2217-001") # focusing on one subject

# CATCHING ERRORS

# 1. Finding empty dates: checks for a full day of missed data

empty_dates <- dat_flow %>%
  group_by(secret_user_id, date) %>%
  mutate(
    day_empty = sum(missed) == length(missed)
  ) %>%
  filter(day_empty) %>%
  mutate(empty_day = "yes")
View(empty_dates)

# manual review of dates that are truly incorrect
incorrect_dates <- empty_dates %>% filter(
  (secret_user_id == "2136-001" & date == "2024-08-05") |
    (secret_user_id == "2137-001" & date == "2024-08-16") |
    (secret_user_id == "2143-002" & date == "2024-09-11") |
    (secret_user_id == "2146-001" & date == "2024-08-05") |
    (secret_user_id == "2146-012" & date == "2024-08-05") |
    (secret_user_id == "2157-033" & between(date, as.Date("2025-01-03"),as.Date("2025-01-15"))) |
    (secret_user_id == "2157-034" & between(date, as.Date("2024-12-28"),as.Date("2025-01-11"))) |
    (secret_user_id == "2161-001" & date == "2024-10-31") |
    (secret_user_id == "2161-001" & date == "2024-12-21") |
    (secret_user_id == "2184-048" & date == "2024-12-12") |
    #(secret_user_id == "2190-001" & between(date, as.Date("2025-02-07"),as.Date("2025-02-10"))) |
    (secret_user_id == "2192-001" & date == "2024-08-30") |
    (secret_user_id == "2192-002" & date == "2024-08-30") |
    (secret_user_id == "2200-001" & between(date, as.Date("2024-09-21"),as.Date("2024-10-03"))) |
    (secret_user_id == "2205-001" & date == "2025-01-18") |
    (secret_user_id == "2208-002" & date == "2024-08-28") |
    (secret_user_id == "2212-001" & between(date, as.Date("2024-09-04"),as.Date("2024-09-09"))) |
    (secret_user_id == "2217-001" & between(date, as.Date("2024-11-06"),as.Date("2024-11-20"))) |
    (secret_user_id == "2217-001" & between(date, as.Date("2024-12-28"),as.Date("2024-12-31"))) |
    (secret_user_id == "2248-001" & date == "2025-02-06")
)
View(incorrect_dates)

length(unique(incorrect_dates$secret_user_id)) # num of participants w incorrect dates

# removing those from the cleaned dataset
dat_clean <- dat_flow %>% anti_join(incorrect_dates)

# 2. Finding duplicate assessments: checks that no assessment appears more than once per day

duplicates <- dat_flow %>% 
  group_by(secret_user_id, date, activity_flow) %>%
  mutate(repeats = n()) %>%
  filter(repeats != 1) %>%
  mutate(duplicate_assessments = "yes")
View(duplicates)

# manual review of assessments that must be deleted
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
    (secret_user_id == "2215-001" & schedule_Time == as.POSIXct("2024-11-10 23:00:00", tz = "UTC") & event_id == "82c22b15-a3f4-41e7-ae6b-7ebe12fa720c") |
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
    (secret_user_id == "2248-001" & schedule_Time == as.POSIXct("2024-11-28 17:30:00", tz = "UTC"))
  )
View(duplicates_to_remove)

length(unique(duplicates_to_remove$secret_user_id)) # num of participants w duplicates

# removing those from the cleaned datset
dat_clean <- dat_clean %>% anti_join(duplicates_to_remove)

# 3. Finding unscheduled assessments: checks that all assessments have scheduled dates

unscheduled <- dat_flow %>%
  group_by(secret_user_id) %>%
  filter(is.na(date)) %>%
  mutate(unscheduled_assessments = "yes")
View(unscheduled)

length(unique(unscheduled$secret_user_id)) # num of participants w unscheduled

# removing those from the cleaned dataset
dat_clean <- dat_clean %>% anti_join(unscheduled)

# 4. and 5. Finding random and missing assessments: checks that on each day all assessments appear once

EMA_mandatory_assessments <- c("Morning Assessment", "Mid-day Assessment", "Afternoon Assessment")
EMA_evening_options <- c("Evening Assessment", "Evening Assessment (Female)")
saliva_mandatory_assessments <- c("Morning Assessment (with saliva)",
                                  "Mid-day Assessment (with saliva)",
                                  "Afternoon Assessment (with saliva)")
saliva_evening_options <- c("Evening Assessment (with saliva)",
                            "Evening Assessment (Female; with saliva)")

find_missing_random <- function(data, saliva_value, mandatory_assessments, evening_options){
  data %>%
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
    mutate(missing_random = "yes")
}

EMA_missing_random <- find_missing_random(dat_flow, 0, EMA_mandatory_assessments, EMA_evening_options)
saliva_missing_random <- find_missing_random(dat_flow, 1, saliva_mandatory_assessments, saliva_evening_options)
missing_random <- bind_rows(EMA_missing_random, saliva_missing_random) %>%
  arrange(secret_user_id, date) %>%
  select(-c("has_evening","mult_evening"))
View(missing_random)

length(unique(missing_random$secret_user_id)) # num of participants w missing and random

# manual review of random assessments to be removed
random_to_remove <- missing_random %>% filter(
  (secret_user_id == "2216-001" & date == "2025-01-04") |
    (secret_user_id == "2216-001" & date == "2025-01-11") |
    (secret_user_id == "2217-001" & date == "2024-12-12")
)

# removing those random assessments from the dataset
dat_clean <- dat_clean %>% anti_join(random_to_remove)

# manually flagging the missing assessments in the dataset
dat_clean <- dat_clean %>%
  mutate(flag_missing_assessments = ifelse(
    (secret_user_id == "2216-001" & date == "2024-12-21") |
          (secret_user_id == "2216-001" & date == "2024-12-22") |
          (secret_user_id == "2216-001" & date == "2024-12-29"), 1, 0))

# 6. Finding long assessments: checks that all assessments were finished in <= 1hour (after other cleaning is done)

long <- dat_clean %>% filter(duration >3600)
View(long)

# 7. Flagging participants who were affected by technical issues, bugs, etc.

dat_clean <- dat_clean %>%
  mutate(flag_tech = ifelse((secret_user_id == "2211-001") |
                         (secret_user_id == "2215-001") |
                         (secret_user_id == "2216-001") |
                         (secret_user_id == "2217-001"), 1, 0))
         # exclude_compl = ifelse((secret_user_id == "2216-001") |
         #                        (secret_user_id == "2217-001"), 1, 0))

# 8. Flagging drop-outs

dat_clean <- dat_clean %>%
  mutate(flag_dropout = ifelse((secret_user_id == "2161-001") |
                                  (secret_user_id == "2163-001") |
                                  (secret_user_id == "2207-001"), 1, 0))

# ---> Clean data sets
View(dat_clean)
glimpse(dat_clean)
# write_csv(dat_clean, "dat_clean.csv")

ema_clean <- dat_clean %>% filter(saliva == 0) # EMA only
saliva_clean <- dat_clean %>% filter(saliva == 1) # saliva only

# All data to review
to_review <- full_join(duplicates, unscheduled) %>% full_join(missing_random) %>%
  full_join(empty_dates) %>%
  arrange(secret_user_id, date) %>%
  select(c("secret_user_id", "date", "activity_flow", "duplicate_assessments", "repeats", "unscheduled_assessments", "start_Time", "end_Time", "duration", "missing_random", "present", "absent", "empty_day"))
View(to_review)

# Exporting csvs of full data file and the rows that must be reviewed
# write_csv(dat_ema_flow, "dat_ema_flow.csv")
# write_csv(to_review, "to_review.csv")

-------

# SAMPLE-WIDE EMA SUMMARY

ema_clean_forcomp <- ema_clean %>% filter(flag_tech == 0 & flag_dropout == 0) # excluding tech issues and drop-outs
ema_complete_forcomp <- ema_clean_forcomp %>% filter(completed == 1) # excluding incomplete EMA responses

## How many participants have completed the EMA to date?
number_completed = length(unique(ema_clean$secret_user_id))

## How many participants are considered in the compliance calculations? (excl tech issues and drop-outs)
number_forcomp = length(unique(ema_clean_forcomp$secret_user_id))

## What is the total compliance of the sample?
total_compliance = 1 - sum(ema_clean_forcomp$missed)/nrow(ema_clean_forcomp)

## What is the total compliance for completed assessments?
complete_compliance = nrow(ema_complete_forcomp)/nrow(ema_clean_forcomp)

## What percent of assessments are fully completed?
prop_complete = nrow(ema_complete_forcomp)/nrow(ema_clean_forcomp) 

## On average, how long does it take participants to complete the EMA, in minutes?
summary(ema_clean_forcomp$duration/60) # all surveys
sd(ema_clean_forcomp$duration, na.rm = TRUE)/60
summary(ema_complete_forcomp$duration/60) # completed surveys
sd(ema_complete_forcomp$duration, na.rm = TRUE)/60

-------
  
# SUBJECT-LEVEL COMPLIANCE

compliance_subj = ema_clean_forcomp %>% ## compliance by subject
  group_by(secret_user_id) %>% 
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  arrange(pct_compliance) %>%
  ungroup()
View(compliance_subj)

## What is the average compliance for a subject?
avg_compliance = mean(compliance_subj$pct_compliance)

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

-----
  
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
  