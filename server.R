
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
  output$Dynamic <- renderUI({
    ncol <- 2
    LL <- vector("list", ncol)
    print(LL)
    
    LL <- lapply(names(dfs), 
                 function(x) {
                   tmp <- dfs[[x]]
                   colnames(tmp) <- NULL
                   list(h5(x),
                   DT::renderDataTable(tmp,
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
                                       escape=1),
                   hr())})
    return(LL)
  })
  
  tableInfo <- renderText({
    input$Dynamic_DataTables_Table_7_cell_clicked[['row']]
  })
  
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
