library(shiny)
library(mcr)
library(shinydashboard)

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, 
             sep=input$sep, quote=input$quote)
  
    })
  
  output$varselect <- renderUI({
    if (identical(datasetInput(), '') || identical(datasetInput(), data.frame())) 
      return(NULL)
    
    selectInput("var1", h5("Method 1 (Reference)"),
                names(datasetInput()), names(datasetInput()), multiple =FALSE)            
  })
  
  output$varselect2 <- renderUI({
    if (identical(datasetInput(), '') || identical(datasetInput(), data.frame())) 
      return(NULL)
    
    selectInput("var2", h5("Method 2 (Test)"),
                names(datasetInput()), names(datasetInput()), multiple =FALSE)            
  })
  
  output$plot1 <- renderPlot({
    
    a <- datasetInput()[,c(input$var1,input$var2), drop=FALSE]
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c('M1', 'M2')
        data1 <- mcreg(a$M1,a$M2,
                      mref.name=input$var1, mtest.name=input$var2)
        MCResult.plotDifference(data1, plot.type=input$batype,
                                add.grid = TRUE)
      
      }
    
  })
  
#   output$info1 <- renderPrint({
#     
#     a <- datasetInput()[,c(input$var1,input$var2), drop=FALSE]
#     if (is.null(a)) {
#       return(NULL)} else {
#     nearPoints(a, input$plot_click1)}
#   })
  
  output$info2 <- renderPrint({
    
    a <- datasetInput()[,c(input$var1,input$var2), drop=FALSE]
    if (is.null(a)) {
      return(NULL)} else {
        nearPoints(a, input$plot_click2, xvar=input$var1, yvar=input$var2, maxpoints = 1)}
  })
  
  output$plot2 <- renderPlot({
    
    a <- datasetInput()[,c(input$var1,input$var2), drop=FALSE]
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1 <- mcreg(a$M1,a$M2, error.ratio = input$syx, 
                      method.reg = input$regmodel, method.ci = input$cimethod,
                      method.bootstrap.ci = input$metbootci)
        MCResult.plot(data1, ci.area=input$ciarea,
                      add.legend=input$legend, identity=input$identity,
                      add.cor=input$addcor, x.lab=input$var1,
                      y.lab=input$var2, cor.method=input$cormet,
                      equal.axis = TRUE, add.grid = TRUE)
        
      }
    
  })
  
  output$summary <- renderPrint({
    
    a <- datasetInput()[,c(input$var1,input$var2), drop=FALSE]
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1 <- mcreg(a$M1,a$M2, error.ratio = input$syx, 
                      method.reg = input$regmodel, method.ci = input$cimethod,
                      method.bootstrap.ci = input$metbootci,
                      mref.name = input$var1, mtest.name = input$var2)
        printSummary(data1)
      }
  
    })
  
  output$table <- DT::renderDataTable({
    
    a <- datasetInput()
    if (is.null(a)) {
      return(NULL)} else {
        DT::datatable(a)
      }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    
    }
     
  )
  
})