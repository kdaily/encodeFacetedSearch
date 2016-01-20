
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
                      server=FALSE,
                      escape=1)
}

library(shiny)

shinyServer(function(input, output, session) {

  # Filter data based on selected rows in input tables
  dfFiltered <- reactive({
    
    # Respond to these inputs - the individual tables' selected rows
    tblInputs <- lapply(tableNames,
                        function(x) input[[sprintf('%s_rows_selected', x)]])
    
    names(tblInputs) <- tableNames
    
    # Start off with the full dataset
    df.filtered <- dfOrig
    
    # Filter on each table's selected rows
    for (x in names(tblInputs)) {
      
      selectedRows <- tblInputs[[x]]
      
      if (!is.null(selectedRows)) {
        
        # Get the values of the selected rows
        rowValues <- baseDF[[x]][selectedRows, "value"]
        
        df.filtered <- df.filtered[df.filtered[, x] %in% rowValues, ]
      }
    }
    
    df.filtered
    
  })
  
  # # The main data - filtered based on selected rows of input tables
  # output$Main <- DT::renderDataTable({
  #   df.filtered <- isolate(dfFiltered())
  #   
  #   DT::datatable(df.filtered)}
  # )
  
  output$Main <- renderText({"Foo"})
  
  dfs <- reactive({
    # Respond to these inputs - the individual tables' selected rows
    tblInputs <- lapply(tableNames,
                        function(x) input[[sprintf('%s_rows_selected', x)]])
    
    df.filtered <- isolate(dfFiltered())
    makeDFList(df.filtered)
  })
  
  output$selectTables <- renderUI({
    
    theDFs <- dfs()
    
    lapply(tableNames, 
           function(x) {
             tmp <- theDFs[[x]]
             colnames(tmp) <- NULL
             output[[x]] <- mktbl(tmp)
           })

    lapply(tableNames, function(x) {tagList(h4(x), DT::dataTableOutput(x))})
  })
  
})
