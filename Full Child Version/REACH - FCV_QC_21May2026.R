rm(list=ls())
library(tidyverse)
library(dplyr)
library(stringr)

#=========================================================
# Importing Curious Data
#=========================================================

# UPDATE FILENAME EACH TIME!
# Read in all uncleaned data to date: current export's "flow_final.csv" file from Python script output
dat <- read_csv("~/.../Output Files/flow_final.csv")

# ==== OPTION A: For the first time only =================
current_export = dat

# ==== OPTION B: Every subsequent time ===================

# UPDATE FILENAME EACH TIME!
# Read in uncleaned data from previous export: previous export's "flow_final.csv" file
prev_export <- read_csv("~/.../flow_final.csv")

# Remove entries that were in previous export to isolate new data
current_export = anti_join(dat, prev_export, by = c("userId", "activity_submission_id", "secret_user_id", "schedule_Time"))

# ==== Filtering Data ====================================

# Add code here to remove admin or test entries, etc.

#=========================================================
# Creating Metadata
#=========================================================

# Extract date, day of week, and survey duration (secs)
current_export$date = as.Date(current_export$schedule_Time)
current_export$dow = weekdays(as.Date(current_export$schedule_Time))
current_export$duration = as.numeric(current_export$end_Time - current_export$start_Time)

#=========================================================
# Setting up Cleaning Functions
#=========================================================

# Unscheduled Assessments
# Check that all assessments have scheduled dates
get_unscheduled <- function(dat, user, date) {
  unscheduled <- dat %>%
    group_by({{ user }}) %>%
    filter(is.na({{ date }})) %>%
    mutate(unscheduled_assessments = "yes") %>%
    ungroup()
  
  View(unscheduled)
  return(unscheduled)
}

# Long assessments
# Check that all assessments were finished in <= 1hour
get_long <- function(dat, duration){
  long <- dat %>% filter(duration >3600)
  View(long)
  return(long)
}

# Random assessments
# Check that there are no standalone (only 1 per day) or extra (>4 per day) assessments
get_random <- function(dat, user, date, assessment){
    random <- dat %>%
      group_by({{ user }}, {{ date }}) %>% 
      mutate(number = length(unique({{ assessment }}))) %>%
      filter(number == 1 | number >4) %>%
      ungroup()
    
    View(random)
    return(random)
}

#=========================================================
# Cleaning: Unscheduled, Random, Long Assessments
#=========================================================

# ==== Unscheduled Assessments ===========================

# 1. Find unscheduled assessments and manually review -- should have nrow=0
unscheduled <- get_unscheduled(current_export, secret_user_id, date)

# 1b. Number of participants with unscheduled assessments -- should be 0
length(unique(unscheduled$secret_user_id))

# 2. Remove unscheduled assessments from original dataset (current_export) to create new clean dataset (current_export_clean)
current_export_clean <- current_export %>% anti_join(unscheduled)

# ==== Random Assessments ================================

# 1. Find random assessments and manually review (only 1/day or >4/day)
random <- get_random(current_export_clean, secret_user_id, date, activity_flow_name)

# 1b. Number of participants with random assessments
length(unique(random$secret_user_id))

# 2. Identify which random assessments should be removed
random_to_remove <- random %>% filter(
  (secret_user_id == "XXXX-XXX" & date == "YYYY-MM-DD") |
    (secret_user_id == "XXXX-XXX" & date == "YYYY-MM-DD")
)

# 3. Remove identified random assessments from clean dataset (current_export_clean)
current_export_clean <- current_export_clean %>% anti_join(random_to_remove)

# ==== Long Assessments (after other cleaning) ===========

# 1. Find long assessments (duration >1hr) -- should have nrow=0
long <- get_long(current_export_clean, duration)

# 1b. Number of participants with long assessments -- should be 0
length(unique(long$secret_user_id))

# 2. Remove long assessments from clean dataset (current_export_clean)
current_export_clean <- current_export %>% anti_join(long)

#=========================================================
# Flags: Technical Issues, Drop-Outs, Other
#=========================================================

# ==== Technical Issues (excluded from compliance) =======

# 1. Flag participants who were affected by technical issues, bugs, etc.

current_export_clean <- current_export_clean %>%
  mutate(flag_tech = ifelse((secret_user_id == "XXXX-XXX") |
                              (secret_user_id == "XXXX-XXX"), 1, 0))

# ==== Drop-outs (excluded from compliance) ==============

# 1. Flag participants who officially dropped out

current_export_clean <- current_export_clean %>%
  mutate(flag_dropout = ifelse((secret_user_id == "XXXX-XXX") |
                              (secret_user_id == "XXXX-XXX"), 1, 0))

# ==== Other (included in compliance) ====================

# 1. Flag participants for any other reason (eg.- holiday periods)

current_export_clean <- current_export_clean %>%
  mutate(flag_other = ifelse((secret_user_id == "XXXX-XXX") |
                                 (secret_user_id == "XXXX-XXX"), 1, 0))

#=========================================================
# Single Participant Review
#=========================================================

# To reference a single participant's full dataset: 
this_id <- master %>% filter(secret_user_id == "XXXX-XXX") # focusing on one subject
View(this_id)

#=========================================================
# Exporting Cleaned Data
#=========================================================

# ==== OPTION A: For the first time only =================
dat_clean = current_export_clean %>% select(-date)
View(dat_clean)

# UPDATE FILENAME EACH TIME!
# Export csv of cleaned data 
write_csv(dat_clean, "dat_clean_MMDDYY.csv")

# ==== OPTION B: Every subsequent time ===================

# Read in previously cleaned dataset: previous export's cleaned file
prev_export_clean <- read_csv("")

# Align previous export's variable types to conform to current export if needed
## Example: prev_export_clean$since_food1 <- as.character(prev_export_clean$since_food1)

# Check for observations that straddle exports (present in both prev_export_clean and current_export_clean)
key_cols <- c("secret_user_id", "date", "activity_flow")
in_both <- prev_export_clean %>% semi_join(current_export_clean, by = key_cols)
# And remove these obs from prev_cleaned 
prev_export_clean <- prev_export_clean %>% anti_join(in_both)

# Merge current export's cleaned data with previous export's cleaned data
dat_clean <- bind_rows(current_export_clean, prev_export_clean) %>% arrange(secret_user_id, schedule_Time) %>% select(-date)
View(dat_clean)

# UPDATE FILENAME EACH TIME!
# Export csv of all cleaned data to date
write_csv(dat_clean, "dat_clean_MMDDYY.csv")

#=========================================================
# Compliance Calculations
#=========================================================

# ==== Preparing Sample for Compliance Calculations ======

# 1. Create sample for compliance calculations: excludes those with tech issues and drop-outs
for_comp <- current_export_clean %>% filter(flag_tech == 0 & flag_dropout == 0) 

# 2. Standardize assessment names if needed (if different across participants)
for_comp <- for_comp %>%
  mutate(activity_flow_name = recode(activity_flow_name,
                                     "A" = "B"))

# ==== Integrating Assessment Schedule History ===========

# 1. Import schedule history file (per template provided)
schedule_history <- read_csv("~/.../Input Files/schedule-history.csv")

# 2. Assessment names (for merging) -- update as needed
assessments <- c("Morning Assessment", "Mid-day Assessment", "Afternoon Assessment", "Evening Assessment")

# Create expanded history of each participant's scheduled EMA prompts
schedule_history_long <- schedule_history %>%
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date)
  ) %>%
  rowwise() %>%
  mutate(date = list(seq(start_date, end_date, by = "day"))) %>%
  unnest(date) %>%
  crossing(activity_flow_name = assessments) %>%
  ungroup() %>%
  select(c(secret_user_id, date, activity_flow_name))

# Identify which rows from schedule_history_long are not present in for_comp and add them
# (for_comp should now include empty rows for missing assessments)
rows_to_add <- schedule_history_long %>%
  anti_join(for_comp, by = c("secret_user_id", "activity_flow_name", "date")) %>%
  mutate(schedule_Time = as.POSIXct(date, tz = 'UTC'))

for_comp <- bind_rows(for_comp, rows_to_add) %>% arrange(secret_user_id, schedule_Time)

# Check for any merge issues (that each date has 4 assessments) -- should have nrow=0
merge_issues <- for_comp %>%
  group_by(secret_user_id, date) %>%
  summarise(
    n_assessments = n(),
    .groups = "drop"
  ) %>%
  filter(n_assessments != 4) # update # of assessments if needed
View(merge_issues)

# ==== Creating Metadata for Compliance ==================

# 1. Identify completed entries (response to now_pain, which is last question)
for_comp$completed = ifelse(is.na(for_comp$now_pain),0,1)

# 2. Identify missed entries (no start time)
for_comp$missed = ifelse(is.na(for_comp$start_Time),1,0)

# 3. Create sample of fully complete EMA responses
complete_for_comp <- for_comp %>% filter(completed == 1)

# ==== Sample-wide Compliance Summary ====================

# How many participants have completed the EMA to date?
number_completed = length(unique(dat_clean$secret_user_id))
print(number_completed)

# How many participants are considered in the compliance calculations? (excl tech issues and drop-outs)
number_for_comp = length(unique(for_comp$secret_user_id))
print(number_for_comp)

# How many participants have fully complete EMA responses? (excl incomplete responses)
number_complete = length(unique(complete_for_comp$secret_user_id))
print(number_complete)

# What is the total compliance of the sample?
total_compliance = 1 - sum(for_comp$missed)/nrow(for_comp)
print(total_compliance)

# What is the total compliance for completed assessments?/What percent of assessments are fully completed?
complete_compliance = nrow(complete_for_comp)/nrow(for_comp)
print(complete_compliance)

# On average, how long does it take participants to complete the EMA, in minutes?
summary(for_comp$duration/60) # all surveys
sd(for_comp$duration, na.rm = TRUE)/60
summary(complete_for_comp$duration/60) # completed surveys
sd(complete_for_comp$duration, na.rm = TRUE)/60

# ==== Subject-level Compliance Summary ==================

# Obtain individual compliance statistics for each participant
compliance_subj = for_comp %>%
  group_by(secret_user_id) %>% 
  
  # identify and separate different epochs (if >30 days between them)
  mutate(
    time_since = as.numeric(date - lag(date)),
    new_epoch = ifelse(is.na(time_since) | time_since > 30, 1, 0),
    epoch = cumsum(new_epoch)
  ) %>%
  
  group_by(secret_user_id, epoch) %>%
  summarise(pct_comp_overall = 100-100*mean(missed), # overall compliance %
            pct_comp_complete = 100*mean(completed), # % compliance for fully complete assessments
            n_scheduled=n(), # n of assessments scheduled
            n_answered=n()-sum(missed), # n of assessments answered
            n_days = n_distinct(date), # n of days on which they answered assessments
            first_date = min(date), # first date of answered assessments
            last_date = max(date)) %>% # last date of answered assessments
  arrange(secret_user_id, epoch) %>%
  ungroup()
View(compliance_subj)

# UPDATE FILENAME EACH TIME!
# Export table of compliance for each participant
write_csv(compliance_subj, "px_compliance_MMDDYY.csv")

# ==== Distribution and Outliers =========================

# How many participants have 75% compliance or above?
compliance_subj %>% filter(pct_comp_overall >= 75) %>% nrow()

# How many participants have below 25% compliance?
compliance_subj %>% filter(pct_comp_overall < 25) %>% nrow()

# What does the compliance distribution look like?
hist(compliance_subj$pct_comp_overall)

# Which participants have low compliance (<25%)?
low_compliance <- compliance_subj %>% filter(pct_comp_overall < 25) %>% arrange(secret_user_id)
View(low_compliance)

# ==== Assessment Type and Day of Week Effects ===========

# 1. Compliance by subject, by assessment type
subj_x_type = for_comp %>%
  group_by(secret_user_id, activity_flow_name) %>% 
  summarise(pct_comp_overall=100-100*mean(missed)) %>%
  # filter(secret_user_id == "xxxx-xxx") %>% ## for a specific subject
  ungroup()
View(subj_x_type)

# 2. Overall compliance by assessment type
assessment_type = for_comp %>%
  group_by(activity_flow_name) %>%
  summarise(pct_comp_overall=100-100*mean(missed), n=n()) %>%
  ungroup()
View(assessment_type)

# 3. Overall compliance by day of week
for_comp$dow = weekdays(as.Date(for_comp$schedule_Time)) # extract dow for newly merged rows

dow = for_comp %>%
  group_by(dow) %>%
  summarise(pct_comp_overall=100-100*mean(missed), n=n()) %>%
  ungroup()
View(dow)
