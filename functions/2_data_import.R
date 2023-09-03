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
# IMPORT STOCK PRICES
################################################
#GOOGLE
GOOGL <- getSymbols("GOOGL", auto.assign = FALSE)
GOOGL <- round_df(GOOGL)


#AMAZON
AMZN <- getSymbols("AMZN", auto.assign = FALSE)
AMZN <- round_df(AMZN)

#NOVARTIS
NVS <- getSymbols("NVS", auto.assign = FALSE)
NVS <- round_df(NVS)

#NESTLE
NSRGY <- getSymbols("NSRGY", auto.assign = FALSE)
NSRGY <- round_df(NSRGY)

#APPLE
AAPL <- getSymbols("AAPL", auto.assign = FALSE)
AAPL <- round_df(AAPL)

#META
META <- getSymbols("META", auto.assign = FALSE)
META <- round_df(META)

#META
WMT <- getSymbols("WMT", auto.assign = FALSE)
WMT <- round_df(WMT)

#AIRBNB
ABNB <- getSymbols("ABNB", auto.assign = FALSE)
ABNB <- round_df(ABNB)


#SPOTIFY
SPOT <- getSymbols("SPOT", auto.assign = FALSE)
SPOT <- round_df(SPOT)



################################################
# IMPORT STOCK PORTFOLIO
################################################
portfolio <- read.csv("input/stockportfolio.csv", sep = ";"
                     , comment.char = "", strip.white = TRUE,
                     stringsAsFactors = TRUE, encoding="UTF-8-BOM")

portfolio_wide <- read.csv("input/stockportfolio_wide.csv", sep = ";"
                      , comment.char = "", strip.white = TRUE,
                      stringsAsFactors = TRUE, encoding="UTF-8-BOM")

portfolio_filter <- read.csv("input/stockportfolio_filter.csv", sep = ";"
                           , comment.char = "", strip.white = TRUE,
                           stringsAsFactors = TRUE, encoding="UTF-8-BOM")


