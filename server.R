library(shiny)
library(mcr)
library(DT)

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
    
    # Variable selection:    
    selectInput("vars", "Variables to use:",
                names(datasetInput()), names(datasetInput()), multiple =TRUE)            
  })
  
  output$plot1 <- renderPlot({
    
    a <- datasetInput()[,input$vars, drop=FALSE]
    if (is.null(a)) {
      return(NULL)} else {
#        a <- datasetInput()[,input$vars, drop=FALSE]
        names(a) <- c('M1', 'M2')
        data1 <- mcreg(a$M1,a$M2,
                      mref.name=input$xlab,
                      mtest.name=input$ylab)
        MCResult.plotDifference(data1, plot.type=input$batype)
      
      }
    
  })
  
  output$plot2 <- renderPlot({
    
    a <- datasetInput()[,input$vars, drop=FALSE]
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1 <- mcreg(a$M1,a$M2, error.ratio = input$syx, 
                      method.reg = input$regmodel, method.ci = input$cimethod,
                      method.bootstrap.ci = input$metbootci)
        MCResult.plot(data1, ci.area=input$ciarea,
#                      points.col = "#FF7F5060", points.pch = 19,
                      add.legend=input$legend, identity=input$identity,
                      add.cor=input$addcor, x.lab=input$xlab,
                      y.lab=input$ylab, cor.method=input$cormet)
        
      }
    
  })
  
  output$summary <- renderPrint({
    
    a <- datasetInput()[,input$vars, drop=FALSE]
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1 <- mcreg(a$M1,a$M2, error.ratio = input$syx, 
                      method.reg = input$regmodel, method.ci = input$cimethod,
                      method.bootstrap.ci = input$metbootci,
                      mref.name = input$xlab, mtest.name = input$ylab)
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