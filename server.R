
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
  
  # Filter data based on selected rows in input tables
  dfFiltered <- reactive({
    
    # Respond to these inputs - the individual tables' selected rows
    tblInputs <- lapply(names(dfs),
                        function(x) input[[sprintf('%s_rows_selected', x)]])
    names(tblInputs) <- names(dfs)
    
    # Start off with the full dataset
    df.filtered <- df
    
    # Filter on each table's selected rows
    for (x in names(tblInputs)) {
      
      selectedRows <- tblInputs[[x]]
      
      if (!is.null(selectedRows)) {
        
        # Get the values of the selected rows
        rowValues <- dfs[[x]][selectedRows, 1]
        
        df.filtered <- df.filtered[df.filtered[, x] %in% rowValues, ]
      }
    }
    
    df.filtered
    
  })
  
  # The main data - filtered based on selected rows of input tables
  output$Main <- DT::renderDataTable({
    df.filtered <- dfFiltered()
    DT::datatable(df.filtered)}
  )
  
  output$selectTables <- renderUI({
    dfs <- makeDFs(df)
    
    lapply(names(dfs), 
           function(x) {
             tmp <- dfs[[x]]
             colnames(tmp) <- NULL
             output[[x]] <- mktbl(tmp)
           })

    lapply(names(dfs), function(x) {tagList(h4(x), DT::dataTableOutput(x))})
  })
  
})
