library(quantmod)

################################################
# ROUNDING FUNCTION THAT ALSO CONVERTS XTS OBJECT INTO DATAFRAME AND MAKES ROWNAMES FIRST COLUMN
################################################
round_df <- function(data, digits = 2) {
  # Convert xts to dataframe
  data_df <- as.data.frame(data)
  
  # Add "date" column with rownames as dates in YYYY-MM-DD format
  data_df <- tibble(date = as.Date(rownames(data_df), format = "%Y-%m-%d"), data_df)
  
  # Round columns
  for (col in colnames(data_df)[-1]) {
    data_df[, col] <- round(data_df[, col], digits)
  }
  
  # Order the dataframe by date in descending order
  data_df <- data_df %>% arrange(desc(date))
  
  return(data_df)
}


################################################
# IMPORT ALL STOCK PRICES AND STORE THEM IN A JOINT DATAFRAME
################################################
stock_symbols <- c("AAPL", "GOOGL", "ABNB", "AMZN", "META", "SPOT")

# Download stock data for each symbol and store them in a list
stock_data_list <- lapply(stock_symbols, function(symbol) {
  getSymbols(symbol, auto.assign = FALSE)
})
stock_data_full <- as.data.frame(do.call(merge, stock_data_list))
numeric_cols <- sapply(stock_data_full, is.numeric)

# Round numeric columns to the second decimal place
stock_data_full[numeric_cols] <- lapply(stock_data_full[numeric_cols], function(x) round(x, 2))
stock_data_today <- tail(stock_data_full, 1)
stock_data_today$Index.Close <- c(1)

stock_data_today <- stock_data_today %>%
                    select(ends_with("Close") | ends_with("Index")) %>%
                    rename_with(~substr(., 1, nchar(.) - 6), everything()) %>%
                    pivot_longer(cols = -Index, names_to = "stock", values_to = "close_price_today") %>%
                    select(-Index)




################################################
# IMPORT STOCK PRICES
################################################
#GOOGLE
GOOGL <- getSymbols("GOOGL", auto.assign = FALSE)
GOOGL <- round_df(GOOGL)
GOOGL$stock_name <- "Google"

#AMAZON
AMZN <- getSymbols("AMZN", auto.assign = FALSE)
AMZN <- round_df(AMZN)
AMZN$stock_name <- "Amazon"

#APPLE
AAPL <- getSymbols("AAPL", auto.assign = FALSE)
AAPL <- round_df(AAPL)
AAPL$stock_name <- "Apple"

#META
META <- getSymbols("META", auto.assign = FALSE)
META <- round_df(META)
META$stock_name <- "Meta"

#AIRBNB
ABNB <- getSymbols("ABNB", auto.assign = FALSE)
ABNB <- round_df(ABNB)
ABNB$stock_name <- "Airbnb"

#SPOTIFY
SPOT <- getSymbols("SPOT", auto.assign = FALSE)
SPOT <- round_df(SPOT)
SPOT$stock_name <- "Spotify"



################################################
# IMPORT INFLATION RATES CH AND USA
################################################
#USA INFLATION RATE
getSymbols("CPIAUCSL", src = "FRED")
CPIAUCSL <- round_df(CPIAUCSL)
CPIAUCSL <- CPIAUCSL %>%
  mutate(inflation_rate_cpiaucsl = as.numeric((CPIAUCSL / dplyr::lead(CPIAUCSL, 12) - 1) * 100)) %>%
  filter(date >= "2007-01-01") %>%
  select("date", "inflation_rate_cpiaucsl")



################################################
# IMPORT STOCK PORTFOLIO
################################################
portfolio <- read.csv("input/stockportfolio.csv", sep = ";"
                     , comment.char = "", strip.white = TRUE,
                     stringsAsFactors = TRUE, encoding="UTF-8-BOM")

portfolio_filter <- read.csv("input/stockportfolio_filter.csv", sep = ";"
                           , comment.char = "", strip.white = TRUE,
                           stringsAsFactors = TRUE, encoding="UTF-8-BOM")



###############################################
# CALCULATE TOTAL VALUE OF PORTFOLIO 
##############################################
portfolio <- portfolio %>% 
         left_join(stock_data_today, by = "stock") %>%
         mutate("value_stock" = round(volume * close_price_today,0))

portfolio_wide <- portfolio %>%
              select("stock", "value_stock") %>%
              spread(key = stock, value = value_stock)


###############################################
# CALCULATE VALUE BY DAY OF PORTFOLIO
##############################################
stock_value_date <- stock_data_full %>%
      rownames_to_column(var = "Date.Close") %>%
      select(ends_with("Close")) %>%
      rename_with(~substr(., 1, nchar(.) - 6), everything()) %>%
      pivot_longer(cols = -Date, names_to = "stock", values_to = "close_price_date") %>%
      left_join(portfolio, by = "stock") %>%
      select("Date", "stock", "close_price_date", "stock_name_full", "volume") %>%
      mutate("value_stock_date" = round(close_price_date * volume,0))


portfolio_value_date <- stock_value_date %>%
                        group_by(Date) %>%
                        summarise("value_portfolio_date" = round(sum(value_stock_date, na.rm = TRUE)),0)



###############################################
# CALCULATE 7d, 30d and YTD value FOR ENTIRE PORTFOLIO
##############################################
portfolio_diff_7d <- portfolio_value_date %>%
  tail(7) %>%
  arrange(desc(Date)) %>%
  slice(c(7, 1)) %>%
  summarize(diff_absolut_7d = diff(value_portfolio_date),
            diff_perc_7d = round((last(value_portfolio_date) - first(value_portfolio_date)) / first(value_portfolio_date) * 100,1))


portfolio_diff_30d <- portfolio_value_date %>%
  tail(30) %>%
  arrange(desc(Date)) %>%
  slice(c(30, 1)) %>%
  summarize(diff_absolut_30d = round(last(value_portfolio_date) - first(value_portfolio_date),0),
            diff_perc_30d = round((last(value_portfolio_date) - first(value_portfolio_date)) / first(value_portfolio_date) * 100,1))


portfolio_diff_ytd <- portfolio_value_date %>%
  filter(Date >= "2023-01-01") %>%
  arrange(desc(Date)) %>%
  slice(n(), 1) %>%
  summarize(diff_absolut_ytd = diff(value_portfolio_date),
            diff_perc_ytd = round((last(value_portfolio_date) - first(value_portfolio_date)) / first(value_portfolio_date) * 100,1)
  )
                    


