source("functions/1_setup.R")

library(shinyWidgets)                                                             # additional UI options for shiny
library(shinythemes)    
library(shinydashboard)
library(shinyauthr)
library(shinymanager)
library(shiny)
library(bs4Dash)
conflicts_prefer(bs4Dash::dashboardPage)
conflicts_prefer(bs4Dash::dashboardHeader)
conflicts_prefer(bs4Dash::dashboardSidebar)
conflicts_prefer(bs4Dash::dashboardBody)
conflicts_prefer(bs4Dash::sidebarMenu)
conflicts_prefer(bs4Dash::menuItem)
conflicts_prefer(bs4Dash::tabItems)
conflicts_prefer(bs4Dash::tabItem)
conflicts_prefer(bs4Dash::menuSubItem)
conflicts_prefer(bs4Dash::actionButton)

grey_palette <- c(
  "#000000", "#222222", "#444444", "#555555", "#666666",
  "#777777", "#888888", "#999999", "#aaaaaa")
grey_palette_rev <- rev(grey_palette)


source("functions/2_data_import.R")
source("functions/3_dataprocessing.R")
source("functions/4_valueboxes_landing.R")
source("functions/5_valueboxes_performance.R")


##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
# UI   UI   UI    UI   UI    UI    UI    UI    UI    UI    UI    UI
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
########################################################################################################
ui <- dashboardPage(
  dark = TRUE,
  help = NULL,
  title = "Portfolio Tracker",
  fullscreen = TRUE,
  
  
  #######################################################################################################
  #HEADER
  header = dashboardHeader(
    title = dashboardBrand(
      title = "MyPortfolio",
      color = "secondary",
      href = "https://finance.yahoo.com/",
      image = "https://cdn-icons-png.flaticon.com/512/3081/3081162.png",
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("sliders"),
    fixed = FALSE),
  
  
  #######################################################################################################
  #SIDEBAR
  bs4Dash::dashboardSidebar(
    skin = "light",
    status = "secondary",
    elevation = 9,
    collapsed = T,
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      
      menuItem("At a Glance", tabName = "glance", icon = icon("house")),
      
      menuItem("Performance", tabName = "performance", icon = icon("money-bill-trend-up"), startExpanded = F,
               menuSubItem("Value Performance", tabName = "subtab_portfolio_performance", icon = icon("gauge")),
               menuSubItem("Price Performance", tabName = "subtab_stock_performance", icon = icon("chart-line"))
      ),
      menuItem("Portfolio Diversification", tabName = "portfolio", icon = icon("layer-group"))
      
    )
  ),
  
  
  controlbar = dashboardControlbar(
    id= "controlbar",
    collapsed = TRUE,
    width = "330px",
    conditionalPanel(
      condition = "input.sidebarMenu=='portfolio'",
      controlbarMenu(
        controlbarItem(pickerInput("portfolio_diversification",
                             label = "Portfolio Diversification:",   
                              options = list(title = "Select"),
                              choices = c("By Stock", "By Industry", "By Country"),
                              multiple = FALSE, 
                              selected = "By Stock"),
        br(),
        
        radioGroupButtons(
          inputId = "portfolio_charttype",
          label = "Chart Type:", 
          choices = c(`<i class='fa fa-pie-chart'></i>` = "pie", `<i class='fa fa-trello'></i>` = "chart-tree-map"),
          justified = TRUE,
          selected = "pie"))
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebarMenu=='subtab_stock_performance'",
      controlbarMenu(
        controlbarItem(pickerInput("stock_performance",
                                   label = "Select a stock:",   
                                   options = list(title = "Select"),
                                   choices = c("AAPL", "ABNB", "AMZN", "GOOGL", "META", "SPOT"),
                                   multiple = FALSE, 
                                   selected = "AAPL")))
    )),
  
  #######################################################################################################
  #BODY
  
  dashboardBody(
    tabItems(
      #######################################################################################################
      #FIRST PAGE - AT A GLANCE
      #######################################################################################################
      bs4TabItem(tabName = "glance",
                 
                 fluidRow(
                   valueBoxOutput("vbox_amazon"),
                   valueBoxOutput("vbox_apple"),
                   valueBoxOutput("vbox_meta")
                 ),
                 
                 fluidRow(
                   valueBoxOutput("vbox_google"),
                   valueBoxOutput("vbox_airbnb"),
                   valueBoxOutput("vbox_spotify")
                 )
      ), #CLOSE TABITEM GLANCE
      
      
      #######################################################################################################
      #SECOND PAGE - YOUR PORTFOLIO
      #######################################################################################################
      bs4TabItem(tabName = "portfolio",
                 fluidRow(
                     highchartOutput("chart_diversification", height = "150%", width = "150%"),
                   )
                 
      ), #CLOSE TABITEM PORTFOLIO
    
    
    
    #######################################################################################################
    #THIRD PAGE - STOCK PERFORMANCE
    #######################################################################################################
    bs4TabItem(tabName = "subtab_stock_performance",
               fluidPage(
                 highchartOutput("stock_performance", height = "200%")
               )
    ),
    
    
    #######################################################################################################
    #FOURTH PAGE - PORTFOLIO PERFORMANCE
    #######################################################################################################
    bs4TabItem(tabName = "subtab_portfolio_performance",
               fluidRow(
                        valueBoxOutput("vbox_portfolio_7d"),
                        valueBoxOutput("vbox_portfolio_30d"),
                        valueBoxOutput("vbox_portfolio_ytd")
               ),
               fluidRow(
                 column(width = 4,
                        HTML('<h5 style="color: darkgrey; text-align: center;">7 day performance</h5>'),  # Center and color the text
                        
                        HTML(weekly_performer_table) 
                 ),
                 
                 column(width = 8,
                        highchartOutput("performance_chart", height = "100%", width = "100%")  # Change this ID
                 )
                 
               )
    )
    
    
    ) #CLOSE TABITEMS
  ) #CLOSE DASHBOARD BODY
) #CLOSE UI






##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
# SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
server <- function(input, output) {
  ################################################################
  # PAGE 1 - AT A GLANCE
  ################################################################
  output$vbox_amazon <- bs4Dash::renderValueBox(vb_amazon)
  output$vbox_apple <- bs4Dash::renderValueBox(vb_apple)
  output$vbox_meta <- bs4Dash::renderValueBox(vb_meta)
  output$vbox_google <- bs4Dash::renderValueBox(vb_google)
  output$vbox_airbnb <- bs4Dash::renderValueBox(vb_airbnb)
  output$vbox_spotify <- bs4Dash::renderValueBox(vb_spotify)
  
  
  ################################################################
  # PAGE 2 - PORTFOLIO DIVERSIFICATION
  ################################################################
  #################################################################
  #PIE CHART FOR DIVERSIFICATION
  function_pie_diversification <- function(){
    pie_chart <- portfolio_filter %>% 
      #filter(filter_portfolio == "By Stock") %>% 
      filter(filter_portfolio == input$portfolio_diversification) %>% 
      mutate(y = round((volume / sum(volume)) * 100, 1)) %>%
      select(variable, y) %>%
      group_by(variable) %>%
      summarise(
        y = sum(y))  %>%
      arrange(desc(y))
    
    
    highchart(height = 500) %>%
      hc_chart(type = "pie") %>%
      hc_tooltip(
        valueSuffix = "% of total volume",
        pointFormat = "{point.variable}: <b>{point.y}</b>"
      ) %>%
      hc_plotOptions(
        pie = list(
          colors = grey_palette,  # Set the custom grey color palette
          borderWidth = 0,  # Set the border width to 0
          dataLabels = list(
            enabled = TRUE,
            distance = -30,  # Adjust the distance to move labels inside the pie
            format = "<b>{point.variable}</b>: {point.percentage:.1f}%"))) %>%
      hc_add_series(data = pie_chart, marginBottom = -50)
  }
  
  #################################################################
  #TREEMAP FOR DIVERSIFICATION
  function_treemap_diversification <- function(){
    
    tree_chart <- portfolio_filter %>% 
      #filter(filter_portfolio == "By Stock") %>% 
      filter(filter_portfolio == input$portfolio_diversification) %>% 
      mutate(y = round((volume / sum(volume)) * 100, 1)) %>%
      select(variable, y) %>%
      group_by(variable) %>%
      summarise(
        y = sum(y)) %>%
      arrange(y)
    
    
    tree_chart %>%
      hchart(
        "treemap", height = 900,
        hcaes(x = tree_chart, value = y, color = y)
      ) %>%
      hc_colorAxis(stops = color_stops(colors = grey_palette_rev)) %>%
      hc_tooltip(
        valueSuffix = "% of total volume",
        pointFormat = "{point.variable}: {point.y:.1f}%"
      ) %>%
      hc_plotOptions(
        treemap = list(
          borderWidth = 0,  # Set the border width to 0
          dataLabels = list(
            enabled = TRUE,
            format = "<b>{point.variable}</b>: {point.y:.1f}%"))) %>%
      hc_legend(element_blank)
  }
  
  
  output$chart_diversification <- renderHighchart({
    if(input$portfolio_charttype == "pie"){
      pie_diversification <- function_pie_diversification()
    }
    else if (input$portfolio_charttype == "chart-tree-map") 
    {treemap_diversification <- function_treemap_diversification()}
    
  })
  
  
  ################################################################
  # PAGE 3 - STOCK PERFORMANCE
  ################################################################
  data_flags <- tibble(
    date = as.Date("2017-10-28"),
    title = "Buy",
    text = "Bought: 4,000"
  )
  

  function_stock_performance <- function(){
    stock_performance <- stock_data_full %>%
                         #select(starts_with(input$'stock_performance')) %>%
                         select(starts_with("AAPL")) %>%
                         na.omit() %>%
                         as.xts()
    
    SMA_5 <- round(SMA(Cl(stock_performance), n = 5),1)
    
    
    highchart(type = "stock", height = 500) %>%
      hc_title(text = paste("Stock Price of ", as.factor(input$'stock_performance')),
               style = list(color = "lightgrey")) %>%  # Add a title to the chart
      hc_add_series(stock_performance, id = 2, type = "ohlc", upColor = "green", color = "red") %>%
      hc_add_series(
        data_flags, 
        hcaes(x = date),
        type = "flags", 
        onSeries = 2, 
        color = "darkolivegreen",
        style = list(
          backgroundColor = "gray",
          color = "darkkhaki")) %>%
      hc_add_series(SMA_5, yAxis = 0, name = "5d mean", color = '#CBD5C0')
      
      
    
    
  }
  
  output$stock_performance <- renderHighchart({
    function_stock_performance()
  })
  
  
  ################################################################
  # PAGE 4 - PORTFOLIO PERFORMANCE
  ################################################################
  output$vbox_portfolio_7d <-  bs4Dash::renderValueBox(vb_portfolio_value_7d)
  output$vbox_portfolio_30d <- bs4Dash::renderValueBox(vb_portfolio_value_30d)
  output$vbox_portfolio_ytd <- bs4Dash::renderValueBox(vb_portfolio_value_ytd)
  
  
  #PORFOLIO VALUE PERFORMANCE
  portfolio_value_function <- function(){
    portfolio_value_date %>% 
      hchart(type = "area", hcaes(x=Date, y = value_portfolio_date), marginTop = 20) %>%
      hc_colors('#CBD5C0') %>%
      hc_plotOptions(
        series = list(showInLegend = F)
      ) %>%
      hc_yAxis(
        title = list(text = "Value"))%>%
      hc_xAxis(
        title = list(text = "Date"),
        labels = list(enabled = FALSE)) %>%
      hc_tooltip(
        valueSuffix = " USD",
        pointFormat = "{point.name}: <b>{point.y}</b>") %>%
      hc_title(
        text = "Portfolio Value",
        style = list(color = "darkgrey"))
  }
  
  output$performance_chart <- renderHighchart({
    portfolio_value_function()
  })

  
}

shinyApp(ui, server)


# shiny::shinyApp(ui, server, options = list(launch.browser = .rs.invokeShinyPaneViewer))
shiny::shinyApp(ui, server, options = list(launch.browser = .rs.invokeShinyPaneViewer))


