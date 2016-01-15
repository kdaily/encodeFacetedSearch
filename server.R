
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
  
  values <- reactiveValues()

  observe({
    lapply(names(dfs),
           function(x) {
             cat(sprintf("Updating values table_%s\n", x), file=stderr())
             tmp <- dfs[[x]]
             colnames(tmp) <- NULL
             values[[x]] <- tmp
           })  
  })
  
  observe({
    lapply(names(values),
           function(x) {
             cat(sprintf("Updating output table_%s\n", x), file=stderr())
             output[[sprintf('table_%s', x)]] <- mktbl(values[[x]])
             return(x)
             })
    })
  
  output$Dynamic <- renderUI({
    lapply(names(dfs), 
           function(x) {
             dataTableOutput(sprintf("table_%s", x))
           })
  })

  output$Foo <- renderText({"Foo"})  

})
