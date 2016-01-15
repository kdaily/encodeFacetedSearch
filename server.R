
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

mktbl <- function(x) {
  DT::renderDataTable(x,
                      options = list(lengthChange = FALSE, dom='t',
                                     ordering=FALSE,
                                     autoWidth = TRUE,
                                     nowrap=FALSE,
                                     columnDefs = list(list(width='200px',
                                                            targets=0),
                                                       list(width='100px',
                                                            targets=1))),
                      extensions = 'Responsive',
                      style = 'default',
                      class='compact',
                      rownames=FALSE,
                      escape=1)
}

library(shiny)

shinyServer(function(input, output, session) {
  
  output$Main <- DT::renderDataTable(
    DT::datatable(df)
  )
  
  v <- reactiveValues()
  
  observe({
    lapply(names(dfs), 
           function(x) {
             tmp <- dfs[[x]]
             colnames(tmp) <- NULL
             v[[x]] <- mktbl(tmp)
           })
  })
  
  observe({
    lapply(names(v), 
           function(x) {
             output[[x]] <- v[[x]]
           })
  })
  
  output$Dynamic <- renderUI({
    lapply(names(v), function(x) {tagList(h4(x), DT::dataTableOutput(x))})
  })
  

})
