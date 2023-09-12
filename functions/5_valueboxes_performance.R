#########################################################
# PORTFOLIO PERFORMANCE
########################################################
portfolio_value_date_7d <- portfolio_value_date %>%
  tail(7)
portfolio_value_date_30d <- portfolio_value_date %>%
  tail(30)
portfolio_value_date_ytd <- portfolio_value_date %>%
  filter(Date > "2023-01-01")


# 7d portfolio performance
hc_portfolio_value_7d <- hchart(portfolio_value_date_7d, "line", hcaes(Date, value_portfolio_date), name = "portfolio value")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())

vb_portfolio_value_7d <- valueBoxSpark(
  value = paste(format(portfolio_diff_7d$diff_absolut_7d, big.mark = ","), " USD", sep = ""),
  title = toupper("WEEKLY Portfolio Value"),
  sparkobj = hc_portfolio_value_7d,
  subtitle = tagList(arrow_html_portfolio_7d, paste(portfolio_diff_7d$diff_perc_7d, "%", sep = ""), " in the last 7 days"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("w", lib = "font-awesome"),
  width = 7,
  color = "teal",
  href = NULL
)


# 30d portfolio performance
hc_portfolio_value_30d <- hchart(portfolio_value_date_30d, "line", hcaes(Date, value_portfolio_date), name = "portfolio value")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())

vb_portfolio_value_30d <- valueBoxSpark(
  value = paste(format(portfolio_diff_30d$diff_absolut_30d, big.mark = ","), " USD", sep = ""),
  title = toupper("MONTHLY Portfolio Value"),
  sparkobj = hc_portfolio_value_30d,
  subtitle = tagList(arrow_html_portfolio_30d, paste(portfolio_diff_30d$diff_perc_30d, "%", sep = ""), " in the last 30 days"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("m", lib = "font-awesome"),
  width = 2,
  color = "green",
  href = NULL
)



# YTD portfolio performance
hc_portfolio_value_ytd <- hchart(portfolio_value_date_ytd, "line", hcaes(Date, value_portfolio_date), name = "portfolio value")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())

vb_portfolio_value_ytd <- valueBoxSpark(
  value = paste(format(portfolio_diff_ytd$diff_absolut_ytd, big.mark = ","), " USD", sep = ""),
  title = toupper("YTD Portfolio Value"),
  sparkobj = hc_portfolio_value_ytd,
  subtitle = tagList(arrow_html_portfolio_ytd, paste(portfolio_diff_ytd$diff_perc_ytd, "%", sep = ""), " since the beginning of the year"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("y", lib = "font-awesome"),
  width = 2,
  color = "olive",
  href = NULL
)

