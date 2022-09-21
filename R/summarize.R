library(dplyr)
library(magrittr)
library(stringr)

summarize_relapses <- function(data, period, from, until) {
  episodes <- edmus_episodes %>%
    left_join(edmus_patients, by = "Patient ID") %>%
    filter(Date >= {{ from }}) %>%
    filter(Date < {{ until }}) %>%
    select(`Patient ID`, `Date`)

  data %<>%
    left_join(
      episodes %>%
        group_by(`Patient ID`) %>%
        summarize(
          "Episode count ({period})" := n(),
        ),
      by = "Patient ID"
    ) %>%
    mutate(
      "Episode count ({period})" :=
        replace_na(.[[str_glue("Episode count ({period})")]], 0),
      "ARR ({period})" := case_when(
        ({{ until }} - {{ from }}) >= dyears(1) ~
          .[[str_glue("Episode count ({period})")]] /
            (({{ until }} - {{ from }}) %/% dyears(1))
      ),
    )
}

summarize_severity <- function(data, period, from, until) {
  assessments <- edmus_assessments %>%
    left_join(edmus_patients, by = "Patient ID") %>%
    filter(Date >= {{ from }}) %>%
    filter(Date < {{ until }}) %>%
    filter(`Concurrent relapse` != TRUE) %>%
    select(`Patient ID`, PI, MSSS, ARMSS)

  data %>%
    left_join(
      assessments %>%
        group_by(`Patient ID`) %>%
        summarize(
          "PI ({period})" := last(PI),
          "MSSS ({period})" := last(MSSS),
          "ARMSS ({period})" := last(ARMSS),
          "PI SD ({period})" := sd(PI),
          "MSSS SD ({period})" := sd(MSSS),
          "ARMSS SD ({period})" := sd(ARMSS),
        ),
      by = "Patient ID"
    )
}

summarize_period <- function(data, period, from, until) {
  data %>%
    summarize_relapses(period, {{ from }}, {{ until }}) %>%
    summarize_severity(period, {{ from }}, {{ until }})
}

summarize_edss <- function(data, value) {
  assessments <- edmus_assessments %>%
    filter(
      IEDSS >= value, IEDSS < (value + 1),
      `Concurrent relapse` != TRUE,
    )

  data %>% left_join(
    assessments %>%
      group_by(`Patient ID`) %>%
      summarize(
        "Time of EDSS {value}" := first(Date),
        "Duration of EDSS {value}" := last(Date) - first(Date),
      ),
    by = "Patient ID"
  )
}

edmus_patients <-
  reduce(0:10, summarize_edss, .init = edmus_patients)

edmus_patients %<>%
  summarize_period(
    "global",
    from = `MS onset`,
    until = `Last clinical assessment`
  ) %>%
  summarize_period(
    "RR phase",
    from = `MS onset`,
    until = case_when(
      `Disease course` == "RR" ~ `Last clinical assessment`,
      `Disease course` %in% c("SP-R", "SP-NR") ~ `Progression onset`,
      `Disease course` == "PP" ~ `MS onset`,
    )
  ) %>%
  summarize_period("EDSS 0-3",
    from = `MS onset`,
    until = coalesce(
      `Time of EDSS 3`,
      `Last clinical assessment`
    )
  ) %>%
  summarize_period("EDSS 3-6",
    from = `Time of EDSS 3`,
    until = coalesce(
      `Time of EDSS 6`,
      `Last clinical assessment`
    )
  )
