
  library(readxl)
  library(tidyverse)
  library(viridis)
  library(lubridate)
  library(janitor)
  library(summarytools)
  library(testthat)


# 1.0 Data ---------------------------------------------------------------------

  data <- read_excel("data/16_12_19_ed_1_data.xlsx")

  labels <- names(data)

  data <- clean_names(data)

# Remaining missing values -----------------------------------------------------

  data[data == 999] <- NA

  data[data == 888] <- NA # Technically these are "erroneous"

# map(data, class)
# view(dfSummary(data))


# 2.0 Clean variables ----------------------------------------------------------


# Dates ----------------------------------------------------------------------

  excel_time <- function(x){
    as.POSIXct(
      as.numeric(x) * 60 * 60 * 24,
      origin = "1899-12-30"
    )
  }

  # Fix excel times ----

  tar <- names(select(data, contains("date_time")))
  data[tar] <- map_df(data[tar], excel_time)

# Visits ---------------------------------------------------------------------

  # Order patients by first ed visit

  data <- group_by(data, patient_code) %>%
    mutate(first_visit = min(ed_arrive_date_time)) %>%
    ungroup() %>%
    mutate(patient_code = reorder(factor(patient_code), first_visit))

  # Add count of repeat visits

  data <- arrange(data, patient_code, ed_arrive_date_time) %>%
    group_by(patient_code) %>%
    mutate(visit = row_number()) %>%
    ungroup() %>%
    select(
      patient_code, visit, -first_visit,
      ed_arrive_date_time, time_to_triage_hrs,
      triage_start_date_time, triage_to_seen_time_hrs,
      seen_date_time, ed_arrival_to_seen_time_hrs, ed_depart_date_time,
      everything()
      )

# Timespan variables -----------------------------------------------------------

  # One error in ed admit date, see View(filter(data, time_to_triage_hrs > 500))
  # patient 463839. Set values to missing for now.
  data$ed_arrive_date_time[data$patient_code == "463839"] <- NA
  data$time_to_triage_hrs[data$patient_code == "463839"] <- NA
  data$ed_arrival_to_seen_time_hrs[data$patient_code == "463839"] <- NA

  # They aren't missing the relevant dates so set to missing and see if
  # there is an explanation.                                               QUERY

  # Seen and depart times appear to be off for patient 1506430 (10 days after)
  # View(filter(data, triage_to_seen_time_hrs > 200))
  # View(filter(data, ed_arrival_to_seen_time_hrs > 200))                  QUERY
  # Patient 1506430 visit 1 has seen and depart dates 10 days later. Will assume
  # type for now.
  tar <- data$patient_code == "1506430" & data$visit == 1
  data$seen_date_time[tar] <- "2018-06-19 14:28:00 BST"
  data$ed_depart_date_time[tar] <- "2018-06-19 14:44:00 BST"

  # Patient with 800+ hour seen to discharge. Assume incorrect and set discharge
  # to missing for now.
  tar <- data$patient_code == "1075173" & data$visit == 1
  data$ed_depart_date_time[tar] <- NA


  # I check the math on all of these "time to" and they are correct and don't
  # need to be recalculated.

  # Make sure everyone missing a "time to" value doesn't have both relevant
  # times, i.e. make sure the value isn't needlessly missing

  data$triage_to_seen_time_hrs[
    is.na(data$triage_to_seen_time_hrs) &
    !is.na(data$triage_start_date_time) &
    !is.na(data$seen_date_time)] # 0

  data$ed_arrival_to_seen_time_hrs[
    is.na(data$ed_arrival_to_seen_time_hrs) &
      !is.na(data$ed_arrive_date_time) &
      !is.na(data$seen_date_time)] # 0

  data$time_to_triage_hrs[
    is.na(data$time_to_triage_hrs) &
      !is.na(data$ed_arrive_date_time) &
      !is.na(data$triage_start_date_time)] # 0

# Ordering of date-times -------------------------------------------------------

  # Make sure date/times are in the correct order

  tar <- (data$ed_arrive_date_time < data$triage_start_date_time) &
         (data$triage_start_date_time < data$seen_date_time) &
         (data$seen_date_time < data$ed_depart_date_time)

  # Flag patients for whom all the dates are in order

  data <- data %>%
    mutate(
      correct_order =
        ed_arrive_date_time < triage_start_date_time &
        triage_start_date_time < seen_date_time &
        seen_date_time < ed_depart_date_time
    )
  # Note: this variable is missing for anyone without all 4 times, so let's
  # expand to other groups with 3 of 4 times, but still in the right order.

  data$correct_order <- ifelse(
    (data$ed_arrive_date_time < data$seen_date_time &
       data$seen_date_time == data$triage_start_date_time &
       data$triage_start_date_time < data$ed_depart_date_time),
    "A/S-T/D Correct",
    data$correct_order
  )

  data$correct_order <- ifelse(
    is.na(data$correct_order) &
      (data$ed_arrive_date_time < data$triage_start_date_time &
         data$triage_start_date_time < data$ed_depart_date_time),
    "A/T/D Correct",
    data$correct_order
  )

  data$correct_order <- ifelse(
    is.na(data$correct_order) &
      (data$ed_arrive_date_time < data$seen_date_time &
         data$seen_date_time < data$ed_depart_date_time),
    "A/S/D Correct",
    data$correct_order
  )

  data$correct_order <- ifelse(
    is.na(data$correct_order) &
      (data$ed_arrive_date_time < data$ed_depart_date_time),
    "A/D Correct",
    data$correct_order
  )

  data$correct_order <- factor(
    data$correct_order,
    labels = c("A/D Correct", "A/S-T/D Correct", "A/S/D Correct",
               "A/T/D Correct", "Incorrect", "A/T/S/D Correct")
  )

  data <- full_join(
    data,
    group_by(data, correct_order) %>% summarise(n = n()),
    by = "correct_order"
  ) %>%
    mutate(correct_order = reorder(correct_order, n)) %>%
    select(-n)

  data <- data %>%
    mutate(
      arrive_to_triage_hours =
        as.numeric(
          difftime(triage_start_date_time, ed_arrive_date_time, units = "hours")
        ),
      triage_to_seen_hours =
        as.numeric(
          difftime(seen_date_time, triage_start_date_time, units = "hours")
        ),
      seen_to_discharge_hours = as.numeric(
        difftime(ed_depart_date_time, seen_date_time, units = "hours")
      ),
      arrive_to_seen_hours = as.numeric(
        difftime(seen_date_time, ed_arrive_date_time, units = "hours")
      ),
      arrive_to_discharge_hours = as.numeric(
        difftime(ed_depart_date_time, ed_arrive_date_time, units = "hours")
      ),
      a_t_comp = arrive_to_triage_hours  / arrive_to_discharge_hours,
      t_s_comp = triage_to_seen_hours    / arrive_to_discharge_hours,
      s_d_comp = seen_to_discharge_hours / arrive_to_discharge_hours,
      total_comp = a_t_comp + t_s_comp + s_d_comp
    )

  # Data check:
  # All components of total time should sum to 1
  expect_equal(unique(data$total_comp[!is.na(data$total_comp)])[1], 1)

  # ggplot(data, aes(x = arrive_to_triage_hours, y = time_to_triage_hrs)) +
  #   geom_point() + coord_fixed()
  # ggplot(data, aes(x = triage_to_seen_hours, y = triage_to_seen_time_hrs)) +
  #   geom_point() + coord_fixed()
  # ggplot(data, aes(x = arrive_to_seen_hours, y = ed_arrival_to_seen_time_hrs)) +
  #     geom_point() + coord_fixed()
  # There was one person with an arrival date corrected in data_02.R. Replace
  # the relevant timespan variables with the new calcs based on corrected date
  tar <- data$patient_code == "1506430" & data$visit == 1
  data$triage_to_seen_time_hrs[tar] <- data$triage_to_seen_hours[tar]
  data$ed_arrival_to_seen_time_hrs[tar] <- data$seen_to_discharge_hours[tar]
  data$ed_arrival_to_seen_time_hrs[tar] <- data$arrive_to_seen_hours[tar]

  # table(tar) A handful of FALSES, meaning dates are out of order
  # table(is.na(tar)) Plenty of missings in at least one time

  # map(
  #   select(data, contains("date_time")),
  #   function(x) table(is.na(x))
  # )


# Shift indicators -------------------------------------------------------------

  # The dataset includes an indicator for the shift (`shift_code`) which can be
  # divided into day and night. The codes themselves are a little messy though

  # Split the shift into 2 columns, one for night/day and one for the number.

  data$shift_dn <- ifelse(
    grepl("D", data$shift_code), "day", "night"
  )

  data$shift_number <- gsub("[[:alpha:]]", "", data$shift_code)

  # Are these shift numbers in order with ed_arrive_date_time?

  data$shift_number <- reorder(factor(data$shift_number), data$ed_arrive_date_time)
  # First, these can be converted to numbers and retain their current ordering.
  # Next, they do appear to be in order, expect at the end.
  order(data$shift_number) == order(as.numeric(data$shift_number))

  x <- as.numeric(as.character(levels(data$shift_number)))
  expect_equal(max(x), length(x) - 1)
  expect_equal(min(x), 0)
  expect_equal(unique(x), x)

  out_of_order <- x[!(x == 0:max(x))]

  data$shift_in_order <- ifelse(
    as.numeric(data$shift_number) %in% out_of_order,
    "Out of order",
    "In order"
  )


# 24 hours periods -------------------------------------------------------------

  data$p24_number <- gsub("[[:alpha:]]", "", data$x24hr_period_code)
  data$p24_number <- reorder(factor(data$p24_number), data$ed_arrive_date_time)
  # order(data$p24_number) == order(as.numeric(data$p24_number)) # All in order

  x <- as.numeric(as.character(levels(data$p24_number)))
  expect_equal(max(x), length(x))
  expect_equal(min(x), 1)
  expect_equal(unique(x), x)

  data$p24_number <- as.numeric(data$p24_number)

# Trolley dates/times ----------------------------------------------------------

  tar <- names(select(data, "trolley_start", "trolley_end"))
  data[tar] <- map_df(data[tar], excel_time)


# Other ------------------------------------------------------------------------

  data$age_at_admission <- as.numeric(data$age_at_admission)

  data$gender <- factor(
    data$gender, labels = c("Male", "Female", "Not Reported")
    )

  data$referral_description <- factor(data$referral_description)
  data$mode_of_arrival <- factor(data$mode_of_arrival)

  data$triage_category <- factor(
    data$triage_category,
    labels = c("Not Triaged", "Immediate", "Very Urgent", "Urgent",	"Standard",
               "Non Urgent")
    )

  data$discharge_destination <- factor(data$discharge_destination)

# X.0 Save data ----------------------------------------------------------------

  save(data, labels, file = "data/er_data.RData")











