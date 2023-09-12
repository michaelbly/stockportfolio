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

if (portfolio_diff_7d$diff_absolut_7d > 0) {
  arrow_html_portfolio_7d <- HTML("&uarr;")
} else {
  arrow_html_portfolio_7d <- HTML("&darr;")
}

if (portfolio_diff_30d$diff_absolut_30d > 0) {
  arrow_html_portfolio_30d <- HTML("&uarr;")
} else {
  arrow_html_portfolio_30d <- HTML("&darr;")
}

if (portfolio_diff_ytd$diff_absolut_ytd > 0) {
  arrow_html_portfolio_ytd <- HTML("&uarr;")
} else {
  arrow_html_portfolio_ytd <- HTML("&darr;")
}



########################################################
# CALCULATE WEEKLY BEST PERFORMERS
########################################################
weekly_performer_absolute <- stock_value_date %>%
          select("Date", "stock_name_full", "value_stock_date") %>%
          pivot_wider(names_from = stock_name_full, values_from = value_stock_date) %>%
          arrange(Date) %>%
          tail(7) %>%
          slice(c(7, 1)) %>%
          summarise(across(where(is.numeric), ~ round(first(.) - last(.))))

weekly_performer_perc <- stock_value_date %>%
  select("Date", "stock_name_full", "value_stock_date") %>%
  pivot_wider(names_from = stock_name_full, values_from = value_stock_date) %>%
  arrange(Date) %>%
  tail(7) %>%
  slice(c(7, 1)) %>%
  summarise(across(where(is.numeric), ~ round(((first(.) - last(.)) / last(.)) * 100, 1)))

weekly_performer <- bind_rows(weekly_performer_perc, weekly_performer_absolute)

weekly_performer <- weekly_performer %>% 
                    t() %>%
                    as.data.frame() %>%
                    rownames_to_column(var = "stock") %>%
                    rename(
                          "absolute_delta_7d" = V2,
                          "perc_delta_7d" = V1) %>%
                    arrange(desc(perc_delta_7d)) %>%
                    mutate(perc_delta_7d = ifelse(perc_delta_7d >= 0, paste("+ ", perc_delta_7d, "%", sep = ""), paste(perc_delta_7d, "%", sep = ""))) %>%
                    mutate(absolute_delta_7d = ifelse(absolute_delta_7d >= 0, paste("+ ", absolute_delta_7d, "USD", sep = ""), paste(absolute_delta_7d, "USD", sep = ""))) %>%
                    rename(
                          "USD change" = absolute_delta_7d,
                          "% change" = perc_delta_7d,
                          "Stock" = stock)


weekly_performer_table <- weekly_performer %>%
  kbl(escape = F, format.args = list(big.mark = ","), align = "lrrr") %>%
  kable_styling(bootstrap_options = c("striped"), fixed_thead = T, full_width = T) %>%
  row_spec(0, background = "darkgrey")


weekly_performer_table

