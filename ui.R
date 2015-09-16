library(shiny)
library(mcr)
shinyUI(fluidPage(
  titlePanel("Method Comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', h4('Choose CSV File'),
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      checkboxInput('header', 'Header', TRUE),
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
      fixedRow(
        column(6,
               selectInput('poicol',h5('Point Color'),
                           choices = list('Black' = 'Black',
                                          'Blue' = 'Blue',
                                          'Red' = 'Red')
                           )),
        column(6,
               selectInput('poipch',h5('Point Shape'),
                           choices = list('Circle' = 1,
                                          'Square' = 0,
                                          'Triangle' = 2,
                                          'Diamond' = 5)
               ))
      ),
      radioButtons('format', h5('Document format'), 
                   c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport')
      ),
      mainPanel(
        tabsetPanel(type = "tabs", 
                  tabPanel("Bland-Altman", plotOutput("plot1")),
                  tabPanel("Regression", plotOutput("plot2")),
                  tabPanel("Statistics", verbatimTextOutput("summary")), 
                  tabPanel("Data", tableOutput("table"))
      )
    )
  )
))