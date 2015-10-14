library(shiny)
library(mcr)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Method Comparison"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Entry", tabName = "dataentry", 
        icon = icon("upload", "fa-lg")),
      menuItem("Data", tabName = "data", 
        icon = icon("table", "fa-lg")),
      menuItem("Method Selection", tabName = "dataopt",
        icon = icon("random", "fa-lg")),
      menuItem("Plots", tabName = "plots",
        icon = icon("line-chart", "fa-lg"),
          menuSubItem("BA Plot", tabName = "subitem1"),
          menuSubItem("Scatter Plot", tabName = "subitem2")),
      menuItem("Statistics", tabName = "stats",
        icon = icon("users", "fa-lg")),
      menuItem("Download", tabName = "download",
        icon = icon("download", "fa-lg"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dataentry",
        fileInput('file1', h5('Choose CSV File'),
        accept=c('text/csv', 
          'text/comma-separated-values,text/plain', 
          '.csv')),
        checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', h5('Data Separator'),
            c(Comma=',',
              Semicolon=';',
              Tab='\t'),
              ','),
          radioButtons('quote', h5('Quote'),
            c(None='',
            'Double Quote' = '"',
            'Single Quote' = "'"),
            '"')),
      tabItem(tabName = "dataopt",
              fixedRow(
                column(6,htmlOutput('varselect')),
                column(6,htmlOutput('varselect2')))
      ),
      tabItem(tabName = "data",
              DT::dataTableOutput("table")
      ),
      tabItem(tabName = "subitem1",
              box(title = "Bland-Altman Plot", status='info', 
              plotOutput("plot1")),
              box(title = "Bland-Altman Options", status='info',
              selectInput('batype', h5('Bland-Altman Plot Type'), 
                          choices=list('0.5*(X+Y) vs. Y-X' = 3,
                                       'X vs. Y-X' = 1,
                                       'X vs. (Y-X)/X' = 2,
                                       '0.5*(X+Y) vs. (Y-X)/X' = 4,
                                       'rank(X) vs. Y-X' = 5,
                                       'rank(X) vs. (Y-X)/X' = 6,
                                       'sqrt(X*Y) vs. Y/X' = 7,
                                       '0.5*(X+Y) vs. (Y-X) / (0.5*(X+Y))' = 8)))
      ),
tabItem(tabName = "subitem2",
        box(title = "Scatter Plot", status='info',
            plotOutput("plot2", click = 'plot_click2'),
            verbatimTextOutput("info2")),
            box(title = "Scatter Plot Options", status='info',
            selectInput('regmodel', h5('Regression Model'), 
                        choices=list('Ordinary Least Square' = 'LinReg',
                                     'Weighted Ordinary Least Square' = 'WLinReg',
                                     'Deming' = 'Deming',
                                     'Weighted Deming' = 'WDeming',
                                     'Passing-Bablok' = 'PaBa',
                                     'Passing-Bablok Large Data' = 'PaBaLarge')),
      fixedRow(
      column(6, selectInput('cimethod', h5('CI Method'), 
                        choices=list('Analytical' = 'analytical',
                                     'Jacknife' = 'jackknife',
                                     'Bootstrap' = 'bootstrap',
                                     'Nested Bootstrap' = 'nestedbootstrap'))),
                                  
      column(6, selectInput('metbootci',h5('Bootstrap CI Method'),
                        choices = list('Quantile'='quantile',
                                       'Student'='Student',
                                       'BCa'='BCa',
                                       'tBoot'='tBoot')))
      ),
      fixedRow(
      column(6, selectInput('cormet',h5('Correlation Method'),
                        choices = list('Pearson'='pearson',
                                       'Kendall'='kendall',
                                       'Spearman'='spearman')
                                         )),
      column(6, numericInput('syx', h5('Sy/Sx'), value=1))
                                ),
      fixedRow(
              column(6,  
              checkboxInput('identity', 'Add identity line', value = TRUE),
              checkboxInput('ciarea', 'Add CI Area', value = TRUE)),
              column(6,
              checkboxInput('legend', 'Add Legend', value = TRUE),
              checkboxInput('addcor', 'Add Correlation',value = TRUE))))
      ),
      tabItem(tabName = "stats",
              verbatimTextOutput("summary")
      ),
      tabItem(tabName = "download",
              radioButtons('format', h5('Document format'), 
                           c('PDF', 'HTML', 'Word'),
                           inline = TRUE),
              downloadButton('downloadReport')
      )
    )
  )
)
