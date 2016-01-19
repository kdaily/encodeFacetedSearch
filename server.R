
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
    df
  })
  
  output$Main <- DT::renderDataTable({
    tblInputs <- lapply(names(dfs),
                  function(x) input[[sprintf('%s_rows_selected', x)]])
    names(tblInputs) <- names(dfs)

    df.filtered <- dfFiltered()

    for (x in names(tblInputs)) {
      tblI <- tblInputs[[x]]
      
      cats <- dfs[[x]][, 1]
      if (!is.null(tblI)) {
        cats <- dfs[[x]][tblI, 1]
        
        df.filtered <- df.filtered[df.filtered[, x] %in% cats, ]
      }
    }
    
    DT::datatable(df.filtered)}
  )
  
  v <- reactiveValues()
  
  tmp <- lapply(names(dfs), 
                function(x) {
                  v[[x]] <- NULL
                })
  
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
