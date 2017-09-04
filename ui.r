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

# Define UI for application that draws a histogram
shinyUI(fixedPage(
  # Application title
  includeCSS("www/css/six-weeks-style.css"),
  tags$head(
    tags$script(src = "js/six-weeks-js.js"),
    tags$script(
      'Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'
    )
  ),
  titlePanel(div("Kushona", align = "center")),
  hr(),
  
    div(htmlOutput("character_count")),
    fixedRow(
      column(
        4,
        img(src = "img/Person_combined_xform.svg", width = "300px"),
        img(src = "img/Classes_combined_xform.svg", width =
              "300px"),
        img(src = "img/gravestone.svg", width = "300px")
      ),
      column(
        8,
        tags$style(HTML(".tooltip {opacity: 1}")),
        ndtv:::ndtvAnimationWidgetOutput("netPlot")
      )
    ),
    hr(),
    
    fixedRow(
      htmlOutput("session_count"),
      column(6,HTML('<div id = "plotly_spacer" style="height: 50px;"></div>'),
             plotlyOutput("plotlyBarPlot", width = "auto")),
      column(6, dataTableOutput('player_experience'))
    ),
    hr(),
    fixedRow(
      htmlOutput("recount_word_count"),
      column(6, dataTableOutput('list_of_adventures')),
      column(6, div(id = "recount_container", htmlOutput("recount_frame")))
    )
    
))