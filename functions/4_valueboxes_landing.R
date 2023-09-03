#########################################################
# VALUEBOX FUNCTION
########################################################
valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 2, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}



#########################################################
# HIGHCHARTER LINE CHARTS
########################################################
#AMAZON
hc_amazon <- hchart(AMZN, "area", hcaes(date, AMZN.Close), name = "stock price")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())


vb_amazon <- valueBoxSpark(
  value = paste(format(portfolio_wide$AMZN, big.mark = ","), " USD", sep = ""),
  title = toupper("AMAZON (AMZN)"),
  sparkobj = hc_amazon,
  subtitle = tagList(arrow_html_AMZN, paste(diff_7d$close_diff_7_days_AMZN, "%", sep = ""), " since last week"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("amazon", lib = "font-awesome"),
  width = 2,
  color = "yellow",
  href = NULL
)

#APPLE
hc_apple <- hchart(AAPL, "area", hcaes(date, AAPL.Close), name = "stock price")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())


vb_apple <- valueBoxSpark(
  value = paste(format(portfolio_wide$AAPL, big.mark = ","), " USD", sep = ""),
  title = toupper("APPLE (AAPL)"),
  sparkobj = hc_apple,
  subtitle = tagList(arrow_html_AAPL, paste(diff_7d$close_diff_7_days_AAPL, "%", sep = ""), " since last week"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("apple", lib = "font-awesome"),
  width = 2,
  color = "olive",
  href = NULL
)


#META
hc_meta <- hchart(META, "area", hcaes(date, META.Close), name = "stock price")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())


vb_meta <- valueBoxSpark(
  value = paste(format(portfolio_wide$META, big.mark = ","), " USD", sep = ""),
  title = toupper("META (META)"),
  sparkobj = hc_meta,
  subtitle = tagList(arrow_html_META, paste(diff_7d$close_diff_7_days_META, "%", sep = ""), " since last week"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("meta", lib = "font-awesome"),
  width = 2,
  color = "blue",
  href = NULL
)


#GOOGLE
hc_google <- hchart(GOOGL, "area", hcaes(date, GOOGL.Close), name = "stock price")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())


vb_google <- valueBoxSpark(
  value = paste(format(portfolio_wide$GOOGL, big.mark = ","), " USD", sep = ""),
  title = toupper("Google (GOOGL)"),
  sparkobj = hc_google,
  subtitle = tagList(arrow_html_GOOGL, paste(diff_7d$close_diff_7_days_GOOGL, "%", sep = ""), " since last week"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("google", lib = "font-awesome"),
  width = 2,
  color = "light-blue",
  href = NULL
)


#AIRBNB
hc_airbnb <- hchart(ABNB, "area", hcaes(date, ABNB.Close), name = "stock price")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())


vb_airbnb <- valueBoxSpark(
  value = paste(format(portfolio_wide$ABNB, big.mark = ","), " USD", sep = ""),
  title = toupper("Airbnb (ABNB)"),
  sparkobj = hc_airbnb,
  subtitle = tagList(arrow_html_ABNB, paste(diff_7d$close_diff_7_days_ABNB, "%", sep = ""), " since last week"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("airbnb", lib = "font-awesome"),
  width = 2,
  color = "red",
  href = NULL
)



#AIRBNB
hc_spotify <- hchart(SPOT, "area", hcaes(date, SPOT.Close), name = "stock price")  %>% 
  hc_size(height = 100) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_sparkline_vb())



vb_spotify <- valueBoxSpark(
  value = paste(format(portfolio_wide$SPOT, big.mark = ","), " USD", sep = ""),
  title = toupper("Spotify (SPOT)"),
  sparkobj = hc_spotify,
  subtitle = tagList(arrow_html_SPOT, paste(diff_7d$close_diff_7_days_SPOT, "%", sep = ""), " since last week"),
  info = "This is the stock price I've written in the past 20 days! That's a lot, right?",
  icon = shiny::icon("spotify", lib = "font-awesome"),
  width = 2,
  color = "green",
  href = NULL
)
