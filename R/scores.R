library(ms.sev)

ms_data <- edmus_assessments %>%
  transmute(pid = `Patient ID`,
            edss = EDSS,
            dd = Duration %/% dyears(1),
            ageatedss = Age)

suppressWarnings(
  edmus_assessments %<>% mutate(
    MSSS = ms_sev(ms_data, type = "global_msss", omsss = TRUE)$data$oGMSSS,
    ARMSS = ms_sev(ms_data, type = "global_armss")$data$gARMSS
  )
)
