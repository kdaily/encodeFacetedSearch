
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fixedPage(

  # Application title
  titlePanel("ENCODE-like Faceted Search"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bar.css")
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=4,
                 uiOutput("selectTables")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      DT::dataTableOutput("Main")
    )
  )
))
