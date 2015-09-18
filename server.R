library(shiny)
library(mcr)

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, 
             sep=input$sep, quote=input$quote)
  
    })
  
  output$plot1 <- renderPlot({
    
    a <- datasetInput()
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1<- mcreg(a$M1,a$M2,
                      mref.name=input$xlab,
                      mtest.name=input$ylab)
        MCResult.plotDifference(data1, plot.type=input$batype)
      
      }
    
  })
  
  output$plot2 <- renderPlot({
    
    a <- datasetInput()
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        input$regmodel
        data1<- mcreg(a$M1,a$M2, error.ratio = input$syx, 
                      method.reg = input$regmodel, method.ci = input$cimethod,
                      method.bootstrap.ci = input$metbootci)
        MCResult.plot(data1, ci.area=input$ciarea, 
             add.legend=input$legend, identity=input$identity,
             add.cor=input$addcor, x.lab=input$xlab,
             y.lab=input$ylab, cor.method=input$cormet)
        
      }
    
  })
  
  output$summary <- renderPrint({
    
    a <- datasetInput()
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1<- mcreg(a$M1,a$M2, error.ratio = input$syx, 
                      method.reg = input$regmodel, method.ci = input$cimethod,
                      method.bootstrap.ci = input$metbootci,
                      mref.name = input$xlab, mtest.name = input$ylab)
        printSummary(data1)
      }
  
    })
  
  output$table <- renderTable({
    
    datasetInput()
    
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