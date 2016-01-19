
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
  
  dfFiltered <- reactive({
    tblInputs <- lapply(names(dfs),
                        function(x) input[[sprintf('%s_rows_selected', x)]])
    names(tblInputs) <- names(dfs)
    
    df.filtered <- df
    
    for (x in names(tblInputs)) {
      tblI <- tblInputs[[x]]
      
      cats <- dfs[[x]][, 1]
      if (!is.null(tblI)) {
        cats <- dfs[[x]][tblI, 1]
        
        df.filtered <- df.filtered[df.filtered[, x] %in% cats, ]
      }
    }
    
    df.filtered
    
  })
  
  output$Main <- DT::renderDataTable({
    df.filtered <- dfFiltered()
    DT::datatable(df.filtered)}
  )
  
  v <- reactiveValues()
  
  observe({
    
    dfs <- makeDFs(df)
    
    lapply(names(dfs), 
           function(x) {
             tmp <- dfs[[x]]
             colnames(tmp) <- NULL
             output[[x]] <- mktbl(tmp)
           })
  })
  
  output$Dynamic <- renderUI({
    lapply(names(dfs), function(x) {tagList(h4(x), DT::dataTableOutput(x))})
  })
  
})
