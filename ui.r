library(shiny)
library(RPostgreSQL)
library(network)
library(RColorBrewer)
library(ndtv)
library(rvest)
library(plyr)
library(tweenr)
library(reshape2)
library(ggplot2)
library(plotly)
library(pool)
library(DT)
library(XML)
library(markdown)
library(glue)
library(colorspace)
library(readr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  
  includeCSS("www/css/six-weeks-style.css"),
  includeCSS("https://cdn.jsdelivr.net/gh/fancyapps/fancybox@3.5.6/dist/jquery.fancybox.min.css"),
  
  tags$head(
    tags$script(src = "js/six-weeks-js.js"),
    tags$script(src = "https://cdn.jsdelivr.net/gh/fancyapps/fancybox@3.5.6/dist/jquery.fancybox.min.js")
  ),
  titlePanel("Kushona"),
  hr(),
  
  sidebarLayout(sidebarPanel(
    tags$a(`data-fancybox`="gallery",href='img/Brightwood.png',img(src='img/Brightwood small.png')),
    tags$a(`data-fancybox`="gallery",href='img/Kushona Hex.png',img(src='img/Kushona Hex small.png')),
    tags$a(`data-fancybox`="gallery",href='img/Kroop\'s Landing.png',img(src='img/Kroop\'s Landing small.png'))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Player Experience", dataTableOutput('player_experience')),
      tabPanel("Adventures", dataTableOutput('list_of_adventures')),
      tabPanel("Interactions Graph", ndtv:::ndtvAnimationWidgetOutput("netPlot")),
      tabPanel("Magic Items",fixedRow(column(6,dataTableOutput('list_of_magic_items')),column(6,htmlOutput("magic_item_frame")))),
      tabPanel("DMs", HTML('<div id = "plotly_spacer" style="height: 50px;"></div>'), plotlyOutput("plotlyBarPlot", width = "auto"))
    )
  )
  )
))