#SETUP
##############
##############
rm(list=ls(all=T))

#### 1 LOAD PACKAGES ###########################################################

library(dplyr)                                                                    # data wrangling work horse
library(tidyr)                                                                    # additional data wrangling
library(tidytidbits)                                                              # for conditional piping
library(stringr)                                                                  # to do some operations with strings
#library(shiny)                                                                    # for shiny app functions                                                          # to apply a theme to the shiny app
library(sf)                                                                       # to read/manipulate shapefiles
library(leaflet)                                                                  # to display maps
library(leaflet.extras)                                                           # additional options for leaflet
library(highcharter)                                                              # to build plots
library(DT)                                                                       # for datatable in data explorer
library(kableExtra)                                                               # to make tables
library(scales)                                                                   # to define percentages
library(leaflet.extras)
library(stringi)
library(ggplot2)
library(forcats)
library(plotly)
library(xlsx)
library(rJava)
library("pryr")
library("rgdal")
library("rmapshaper")
library(raster)
library(conflicted)
#library(hypegrammaR) # simple stats 4 complex samples
#library(surveyweights) # calculate weights from samplingframes
library(htmltools)
library(htmlTable)
library(flextable)
library(gt)
library(shinymaterial)
library(RColorBrewer)
library(lubridate)
library(renv)
library(data.table)
library(rsconnect)
library(tibble)
library(profvis)



options(java.parameters = "-Xss2560k")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("box", "shinydashboard")
conflicts_prefer(dplyr::last)
conflicts_prefer(dplyr::first)
conflicts_prefer(bs4Dash::column)
conflicts_prefer(bs4Dash::renderValueBox)
conflicts_prefer(bs4Dash::valueBox)
