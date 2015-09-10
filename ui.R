library(shiny)
library(mcr)
shinyUI(fluidPage(
  titlePanel("Method Comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Data Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      selectInput('regmodel', 'Regression Model', 
                  choices=list('Ordinary Least Square'='LinReg',
                               'Weighted Ordinary Least Square'='WLinReg',
                               'Deming'='Deming',
                               'Weighted Deming'='WDeming',
                               'Passing-Bablok'='PaBa',
                               'Passing-Bablok Large Data'='PaBaLarge')),
      tags$hr(),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
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