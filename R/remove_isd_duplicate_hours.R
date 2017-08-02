# Todo: Documentation

clean_isd <- function(x) {
  met_dataclean <- x %>%
    filter(
      (aa1_1 == 1 | is.na(aa1_1)) & # Remove records 24 hour precip
        report_type %in% c("FM-15", "FM-16")
    )
  # c             Multiple records for the same observation hour. Decide
  # c              whether to overwrite data from previous records for this
  # c              hour.  We want to keep the last non-special observation
  # c              up to (and including) the hour (min=00).  We will only
  # c              use special observations if there are no "regular"
  # c              observations available for that hour.
  
  met_dataclean$duplicate <- duplicated(
    met_dataclean[c("year", "month", "day", "hour", "usaf", "wban")]) |
    duplicated(
      met_dataclean[c("year", "month", "day", "hour", "usaf", "wban")],
      fromLast = TRUE)
  
  met_data_nospec <- subset(met_dataclean, !duplicate | report_type == "FM-15")
  
  last_hour <- met_data_nospec %>%
    group_by(year, month, day, hour, usaf, wban) %>%
    summarize(minute = max(minute)) %>%
    mutate(is_last = TRUE)
  
  met_data_last <- left_join(met_data_nospec, last_hour)
  
  met_data_final <- met_data_last %>%
    filter(is_last) %>%
    select(-duplicate, -is_last)
  
  met_data_final
}
