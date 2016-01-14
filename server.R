
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$Main <- DT::renderDataTable(
    DT::datatable(df)
  )
  
  # values <- reactiveValues()
  # 
  # observe({
  #   lapply(names(dfs), 
  #          function(x) {
  #            tmp <- dfs[[x]]
  #            colnames(tmp) <- NULL
  #            output[[sprintf('table_%s', x)]] <- DT::renderDataTable(tmp,
  #                                                                    options = list(lengthChange = FALSE, dom='t',
  #                                                                                   ordering=FALSE,
  #                                                                                   autoWidth = TRUE,
  #                                                                                   nowrap=FALSE,
  #                                                                                   columnDefs = list(list(width='200px', 
  #                                                                                                          targets=0),
  #                                                                                                     list(width='100px', 
  #                                                                                                          targets=1))),
  #                                                                    extensions = 'Responsive',
  #                                                                    style = 'default',
  #                                                                    class='compact',
  #                                                                    rownames=FALSE,
  #                                                                    escape=1)
  #            values[[x]] <- tmp
  #            return(x)})
  #   })
  
  output$Dynamic <- renderUI({
    lapply(names(dfs), 
           function(x) {
             print(sprintf("table_", x)) 
             dataTableOutput(sprintf("table_", x))
           })
  })

  output$Foo <- renderText({"Foo"})  
  # output$tbl1 <- DT::renderDataTable(df1,
  #                                   options = list(lengthChange = FALSE, dom='t'),
  #                                   extensions = 'Responsive',
  #                                   style = 'bootstrap',
  #                                   rownames=FALSE,
  #                                   escape=1)
  # output$tbl2 <- DT::renderDataTable(df2,
  #                                   options = list(lengthChange = FALSE, dom='t'),
  #                                   extensions = 'Responsive',
  #                                   style = 'bootstrap',
  #                                   rownames=FALSE,
  #                                   escape=1)
  
})
