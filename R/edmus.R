library(dplyr)
library(lubridate)
library(magrittr)
library(purrr)
library(readr)
library(tidyr)

edmus_personal_data_path <- "data/edmus-personal-220813_133309-DEN.txt"
edmus_diagnosis_data_path <- "data/edmus-diagnosis-220811_121631-DEP.txt"
edmus_assessments_data_path <- "data/edmus-clinical-220811_122451-DEP.txt"
edmus_episodes_data_path <- "data/edmus-episodes-220811_121723-DEP.txt"

col_edmus_date <- function(...) {
  col_date(format = "%d/%m/%Y", ...)
}

edmus_load_file <- function(path, ...) {
  read_tsv(path, locale = locale(encoding = "UTF-16"), ...)
}

assessments_in_edss_range <- function(data, min, max) {
  data %>%
    filter(between(IEDSS, min, max)) %>%
    group_by(`Patient ID`) %>%
    arrange(Date, .by_group = TRUE)
}

edmus_personal <- edmus_load_file(
  edmus_personal_data_path,
  col_types = cols_only(
    `Patient ID` = col_character(),
    `Local identifier` = col_character(),
    `Date of birth` = col_edmus_date(),
    `Gender` = col_factor(levels = c("M", "F")),
    `First exam` = col_edmus_date(),
    `Last clinical assessment` = col_edmus_date(),
    `Last info` = col_edmus_date(),
    `Wait and see` = col_logical(),
    `Vital status date` = col_edmus_date(),
  )
) %>%
  drop_na(`Patient ID`) %>%
  mutate(
    Sex = recode(Gender, M = "Male", F = "Female"),
    .keep = "unused"
  ) %>%
  structure(class = c("edmus", class(.)))

edmus_diagnosis <- edmus_load_file(
  edmus_diagnosis_data_path,
  col_types = cols_only(
    `Patient ID` = col_character(),
    `Disease Course` = col_factor(),
    `MS Onset` = col_edmus_date(),
    `Progression Onset` = col_edmus_date(),
  )
) %>%
  rename(
    `Disease course` = `Disease Course`,
    `MS onset` = `MS Onset`,
    `Progression onset` = `Progression Onset`
  ) %>%
  mutate(
    `Disease course` = recode(
      `Disease course`,
      `0` = NULL,
      `1` = "RR",
      `2` = "RR",
      `3` = "SP-NR",
      `4` = "SP-R",
      `5` = "PP-NR",
      `6` = "PP-R",
      `7` = "PP-R",
    )
  )

edmus_patients <- edmus_personal %>%
  left_join(edmus_diagnosis, by = "Patient ID") %>%
  mutate(
    `Age at onset` = (`Date of birth` %--% `MS onset`) %/% dyears(1),
    `Disease duration` = as_date(`Vital status date`) - `MS onset`,
    `Duration of RR phase` = case_when(
      !is.na(`Progression onset`) ~ `Progression onset` - `MS onset`,
      `Disease course` == "RR" ~ `Disease duration`,
      `Disease course` == "PP" ~ 0,
    )
  ) %>%
  structure(class = c("edmus", class(.)))

edmus_episodes <- edmus_load_file(
  edmus_episodes_data_path,
  col_types = cols_only(
    `Patient ID` = col_character(),
    `Episode ID` = col_character(),
    `Date` = col_edmus_date(),
  )
) %>%
  arrange("Patient ID", "Date")

edmus_assessments <- edmus_load_file(
  edmus_assessments_data_path,
  col_types = cols_only(
    `Patient ID` = col_character(),
    `Date` = col_edmus_date(),
    `Assessment ID` = col_character(),
    `Concurrent relapse` = col_factor(levels = c("Yes", "No")),
    `EDSS (entered)` = col_number(),
  )
) %>%
  rename(EDSS = `EDSS (entered)`) %>%
  mutate(`Concurrent relapse` = case_when(
    `Concurrent relapse` == "Yes" ~ TRUE,
    `Concurrent relapse` == "No" ~ FALSE,
  )) %>%
  arrange("Patient ID", "Date")

patient_assessments <- edmus_assessments %>%
  left_join(edmus_patients, by = "Patient ID")

assessments_iedss <- edmus_assessments %>%
  group_by(`Patient ID`) %>%
  arrange(desc(Date), .by_group = TRUE) %>%
  mutate(
    IEDSS = accumulate(EDSS, function(x, y) {
      if (all(!is.finite(c(x, y)))) {
        return(NA)
      }
      min(x, y, na.rm = TRUE)
    })
  ) %>%
  ungroup() %>%
  select(`Patient ID`, Date, IEDSS)

edmus_assessments %<>%
  mutate(
    Age = (Date - patient_assessments$`Date of birth`) %/% dyears(1),
    Duration = Date - patient_assessments$`MS onset`,
    PI = if_else(Duration >= dyears(1),
      EDSS / (Duration %/% dyears(1)), 3,
      NA_real_
    )
  ) %>%
  left_join(assessments_iedss, by = c("Patient ID", "Date")) %>%
  relocate(Age:Duration, .after = Date) %>%
  relocate(IEDSS, .after = EDSS)

edmus_patients %<>%
  left_join(
    edmus_assessments %>%
      group_by(`Patient ID`) %>%
      summarize(
        `Time of follow-up start` = first(Date),
        `Age at follow-up start` = first(Age),
      ),
    by = "Patient ID"
  ) %>%
  mutate(
    `Consultation delay` =
      as_date(`Time of follow-up start`) - `MS onset`,
    `Duration of follow-up` =
      as_date(`Last clinical assessment`) - `Time of follow-up start`,
  )

edmus_export <- function(path, anonimize_data = TRUE) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  files <- list(
    "patients" = edmus_patients,
    "episodes" = edmus_episodes,
    "assessments" = edmus_assessments
  )

  for (key in names(files)) {
    data <- files[[key]]
    if (anonimize_data) {
      data <- anonimize(data)
    }
    write_csv(data, file.path(path, paste("edmus-", key, ".csv", sep = "")))
  }
}
