########################################################
# CALCULATE THE 7D DIFFERENCE
########################################################
META_7d_diff <- META %>%
  mutate(close_diff_7_days_META = round((dplyr::lag(META.Close, 7) - META.Close) / dplyr::lag(META.Close, 7) * 100, 2)) %>%
  slice(8) %>%
  select(close_diff_7_days_META)


AAPL_7d_diff <- AAPL %>%
  mutate(close_diff_7_days_AAPL = round((dplyr::lag(AAPL.Close, 7) - AAPL.Close) / dplyr::lag(AAPL.Close, 7) * 100, 2)) %>%
    slice(8) %>%
  select(close_diff_7_days_AAPL)


ABNB_7d_diff <- ABNB %>%
  mutate(close_diff_7_days_ABNB = round((dplyr::lag(ABNB.Close, 7) - ABNB.Close) / dplyr::lag(ABNB.Close, 7) * 100, 2)) %>%
    slice(8) %>%
  select(close_diff_7_days_ABNB)


AMZN_7d_diff <- AMZN %>%
  mutate(close_diff_7_days_AMZN = round((dplyr::lag(AMZN.Close, 7) - AMZN.Close) / dplyr::lag(AMZN.Close, 7) * 100, 2)) %>%
  slice(8) %>%
  select(close_diff_7_days_AMZN)


GOOGL_7d_diff <- GOOGL %>%
  mutate(close_diff_7_days_GOOGL = round((dplyr::lag(GOOGL.Close, 7) - GOOGL.Close) / dplyr::lag(GOOGL.Close, 7) * 100, 2)) %>%
  slice(8) %>%
  select(close_diff_7_days_GOOGL)


SPOT_7d_diff <- SPOT %>%
  mutate(close_diff_7_days_SPOT = round((dplyr::lag(SPOT.Close, 7) - SPOT.Close) / dplyr::lag(SPOT.Close, 7) * 100, 2)) %>%
  slice(8) %>%
  select(close_diff_7_days_SPOT)


diff_7d <- cbind(META_7d_diff, AAPL_7d_diff, ABNB_7d_diff, AMZN_7d_diff, GOOGL_7d_diff, SPOT_7d_diff)

########################################################
# IFELSE FOR THE ARROS IN THE VALUEBOXES
########################################################
# Check if the value of "closing" is positive
if (diff_7d$close_diff_7_days_SPOT > 0) {
  arrow_html_SPOT <- HTML("&uarr;")
} else {
  arrow_html_SPOT <- HTML("&darr;")
}

if (diff_7d$close_diff_7_days_META > 0) {
  arrow_html_META <- HTML("&uarr;")
} else {
  arrow_html_META <- HTML("&darr;")
}

if (diff_7d$close_diff_7_days_AAPL > 0) {
  arrow_html_AAPL <- HTML("&uarr;")
} else {
  arrow_html_AAPL <- HTML("&darr;")
}

if (diff_7d$close_diff_7_days_ABNB > 0) {
  arrow_html_ABNB <- HTML("&uarr;")
} else {
  arrow_html_ABNB <- HTML("&darr;")
}

if (diff_7d$close_diff_7_days_AMZN > 0) {
  arrow_html_AMZN <- HTML("&uarr;")
} else {
  arrow_html_AMZN <- HTML("&darr;")
}

if (diff_7d$close_diff_7_days_GOOGL > 0) {
  arrow_html_GOOGL <- HTML("&uarr;")
} else {
  arrow_html_GOOGL <- HTML("&darr;")
}


########################################################
# CALCULATE PORTFOLIO SETUP
########################################################

