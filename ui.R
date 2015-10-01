library(shiny)
library(mcr)

shinyUI(fluidPage(
  
  titlePanel("Method Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      
      fixedRow(
        column(7,
          fileInput('file1', h5('Choose CSV File'),
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))),
        column(3,
          checkboxInput('header', 'Header', TRUE))
      ),
      
      fixedRow(
        column(6,  
          radioButtons('sep', h5('Data Separator'),
                        c(Comma=',',
                        Semicolon=';',
                        Tab='\t'),
                        ',')),
        
        column(6,
          radioButtons('quote', h5('Quote'),
                      c(None='',
                      'Double Quote' = '"',
                      'Single Quote' = "'"),
                      '"'))
      ),
      
      htmlOutput('varselect'),
      
      fixedRow(
        column(6,  
          textInput('xlab', h5('Method 1 (Reference)'), value = 'Method1')),
        
        column(6,
          textInput('ylab', h5('Method 2 (Test)'), value = 'Method2'))
        ),
      
      selectInput('batype', h5('Bland-Altman Plot Type'), 
                  choices=list('0.5*(X+Y) vs. Y-X' = 3,
                               'X vs. Y-X' = 1,
                               'X vs. (Y-X)/X' = 2,
                               '0.5*(X+Y) vs. (Y-X)/X' = 4,
                               'rank(X) vs. Y-X' = 5,
                               'rank(X) vs. (Y-X)/X' = 6,
                               'sqrt(X*Y) vs. Y/X' = 7,
                               '0.5*(X+Y) vs. (Y-X) / (0.5*(X+Y))' = 8)),
      
      selectInput('regmodel', h5('Regression Model'), 
                  choices=list('Ordinary Least Square' = 'LinReg',
                               'Weighted Ordinary Least Square' = 'WLinReg',
                               'Deming' = 'Deming',
                               'Weighted Deming' = 'WDeming',
                               'Passing-Bablok' = 'PaBa',
                               'Passing-Bablok Large Data' = 'PaBaLarge')),
      
      fixedRow(
        column(6,  
          selectInput('cimethod', h5('CI Method'), 
                      choices=list('Analytical' = 'analytical',
                               'Jacknife' = 'jackknife',
                               'Bootstrap' = 'bootstrap',
                               'Nested Bootstrap' = 'nestedbootstrap'))),
        
        column(6,
               selectInput('metbootci',h5('Bootstrap CI Method'),
                           choices = list('Quantile'='quantile',
                                          'Student'='Student',
                                          'BCa'='BCa',
                                          'tBoot'='tBoot')))
        ),
      
      fixedRow(
        column(6,
          selectInput('cormet',h5('Correlation Method'),
                      choices = list('Pearson'='pearson',
                                  'Kendall'='kendall',
                                  'Spearman'='spearman')
               )),
        
        column(6,
               numericInput('syx', h5('Sy/Sx'), value=1))
      ),
      
      fixedRow(
        column(6,
          checkboxInput('identity', 'Add identity line',value = TRUE),
          checkboxInput('ciarea', 'Add CI Area',value = TRUE)),
        
        column(6,
          checkboxInput('legend', 'Add Legend',value = TRUE),
          checkboxInput('addcor', 'Add Correlation',value = TRUE))
      ),
      
      radioButtons('format', h5('Document format'), 
                   c('PDF', 'HTML', 'Word'),
                   inline = TRUE),

      downloadButton('downloadReport')
      ),

      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Bland-Altman Plot", plotOutput("plot1")),
                    tabPanel("Scatter Plot", plotOutput("plot2")),
                    tabPanel("Regression Analysis", verbatimTextOutput("summary")), 
                    tabPanel("Uploaded Data", DT::dataTableOutput("table")),
                    tabPanel("Editable Data", rHandsontableOutput('table2'))
      )
    )
  )
))