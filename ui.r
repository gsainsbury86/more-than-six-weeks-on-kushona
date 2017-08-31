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
shinyUI(
  fixedPage(
    # Application title
    tags$head(tags$style(
      HTML("#plotlyBarPlot {border: 1px solid #000000;}"),
      HTML("#netPlot {border: 1px solid #000000;}"),
      HTML("body {background-color: #AAA;}"),
      HTML("hr {border-top: 10px solid #000000;}"),
      HTML(".shiny-image-output {height: auto;}"),
      HTML("img {margin: 0 auto;}")
    )),
    titlePanel(div("6 weeks on Kushona", align = "center")),
    hr(),
    
    fixedRow(div(htmlOutput("character_count")),
             fixedRow(
               column(4,
                      #uiOutput("players",height="auto")),
                      img(src = "img/Person_combined_xform.svg", width = "300px")),
               column(4,
                      img(src = "img/Classes_combined_xform.svg", width =
                            "300px")),
               column(4,
                      img(src = "img/gravestone.svg", width = "300px"))
             )),
    hr(),
    
    fixedRow(
      column(6, dataTableOutput(
        'player_experience'
      )),
      column(6,
             htmlOutput("session_count"),
             plotlyOutput("plotlyBarPlot")
             )),
    hr(),
    fixedRow(
      column(3, img(
        src = "img/scroll.svg",
        width = "200px",
        height = "auto"
      ),
      htmlOutput("recount_word_count")
      ),
      column(9, dataTableOutput('list_of_adventures'))
    ),
    hr(),
    
    # Sidebar with a slider input for the number of bins
    fixedRow(
      # Show a plot of the generated distribution
      column(
        12
        ,
        tags$style(HTML(".tooltip {opacity: 1}")) # stop boostrap css from messing up the tooltip in the widget
        ,
        ndtv:::ndtvAnimationWidgetOutput("netPlot")
      )
    )
  )
)