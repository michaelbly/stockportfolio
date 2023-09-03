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

grey_palette <- c(
  "#000000", "#222222", "#444444", "#555555", "#666666",
  "#777777", "#888888", "#999999", "#aaaaaa")
grey_palette_rev <- rev(grey_palette)


source("functions/1_setup.R")
source("functions/2_data_import.R")
source("functions/3_dataprocessing.R")
source("functions/4_valueboxes_landing.R")


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
  title = "Basic Dashboard",
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
    controlbarIcon = icon("th"),
    fixed = FALSE),
  
  #######################################################################################################
  #SIDEBAR
  bs4Dash::dashboardSidebar(
    skin = "light",
    status = "secondary",
    elevation = 9,
    bs4Dash::sidebarMenu(
      menuItem("At a Glance", tabName = "glance", icon = icon("house")),
      
      menuItem("Your Portfolio", tabName = "portfolio", icon = icon("layer-group"))
    )
  ),
  
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
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     pickerInput("portfolio_diversification",
                                 label = "Portfolio Diversification:",   
                                 options = list(title = "Select"),
                                 choices = c("By Stock", "By Industry", "By Country"),
                                 multiple = FALSE, 
                                 selected = "By Stock"
                     ),
                     
                     br(),
                     br(),
                     
                     radioGroupButtons(
                       inputId = "portfolio_charttype",
                       label = "Chart Type:", 
                       choices = c(`<i class='fa fa-pie-chart'></i>` = "pie", `<i class='fa fa-trello'></i>` = "chart-tree-map"),
                       justified = TRUE,
                       selected = "pie"
                     ),
                     
                     
                   ), 
                   
                   mainPanel(
                     width = 9,
                     highchartOutput("chart_diversification")
                   )
                 )
                 
      ) #CLOSE TABITEM PORTFOLIO
    
    #######################################################################################################
    #THIRD PAGE - STOCK PERFORMANCE
    #######################################################################################################
    
    
    
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
    
    
    highchart() %>%
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
            format = "<b>{point.variable}</b>: {point.percentage:.1f}%"
          )
        )
      ) %>%
      hc_add_series(data = pie_chart)
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
        "treemap", 
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
            format = "<b>{point.variable}</b>: {point.y:.1f}%"
          )
        )
      ) %>%
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
  
  
  
  
}

shinyApp(ui, server)


# shiny::shinyApp(ui, server, options = list(launch.browser = .rs.invokeShinyPaneViewer))
shiny::shinyApp(ui, server, options = list(launch.browser = .rs.invokeShinyPaneViewer))


