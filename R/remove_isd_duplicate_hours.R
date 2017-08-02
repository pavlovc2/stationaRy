# Todo: Documentation
# Todo: Handle more report types

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
  
  met_data_nospec <- met_dataclean %>%
    subset(!duplicate | report_type == "FM-15")
  
  met_data_nospec$duplicate <- duplicated(
    met_data_nospec[c("year", "month", "day", "hour", "usaf", "wban")]) |
    duplicated(
      met_data_nospec[c("year", "month", "day", "hour", "usaf", "wban")],
      fromLast = TRUE)
  
 
  met_data_dup <- met_data_nospec[met_data_nospec$duplicate,]
  met_data_nodup <- met_data_nospec[!met_data_nospec$duplicate,]
  
  out_data <- NULL
  unique_vals <- unique(met_data_dup[met_data_dup$duplicate, c("year", "month", 
                                        "day", "hour", "usaf", "wban")]) 
  for (i in 1:nrow(unique_vals)) {
    trow <- unique_vals[i,]
    tdata <- inner_join(met_data_dup, trow)
    tdata <- tdata[order(tdata$year, tdata$month,
                         tdata$day, tdata$hour, 
                         tdata$minute),]
    # Find the last non-na value for each column
    final_row <- tdata[nrow(tdata),]

    # Check NAs
    for (n in c("wd", "ws", "ceil_hgt", "temp", "dew_point",
                "atmos_pres", "rh", "aa1_2")) {
      if (is.na(final_row[n])) {
        wds <- tdata[!is.na(tdata[n]), n]
        final_row[n] <-  wds[length(wds)]
      }
    }
    out_data <- rbind(out_data, final_row)
  }

  met_data_final <- out_data %>%
    rbind(met_data_nodup) %>%
    select(-duplicate)

  met_data_final
}

