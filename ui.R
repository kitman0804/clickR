library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title = "ggplot - click", 
    tabPanel(
      "Import", 
      fluidPage(
        h3("Import data"), 
        fluidRow(
          column(
            4, 
            wellPanel(
              fileInput("file1", "Choose a .csv/.txt File",
                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', ".txt"))
            )
          ), 
          column(
            4, 
            wellPanel(
              selectInput("sep", "Separator", choice = list('Comma ,' = ",", 'Semicolon ;' = ";", 'Space  ' = " ", 'Tab \t' = "\t"), ','), 
              selectInput("quote", "Quote", choice = list(None = "", 'Double Quote "' = '"', "Single Quote '" = "'"), '"')
            )
          ), 
          column(
            4, 
            wellPanel(
              selectInput("header", "Header", choice = list(None = "", 'Yes' = "TRUE", "No" = "FALSE"), "TRUE"), 
              selectInput("str_as_fac", "Convert Strings to Factors", choice = list(None = "", 'Yes' = "TRUE", "No" = "FALSE"), "FALSE") 
            )
          )
        ), 
        hr(), 
        verbatimTextOutput("dfx_smry")
      )
    ), 
    tabPanel(
      "View data", 
      fluidPage(
        fluidRow(
          DT::dataTableOutput('view_data')
        )
      )
    ), 
    tabPanel(
      "Variables", 
      h3("Variable type"), 
      wellPanel(
        htmlOutput("set_vrb_class"), 
        actionButton("set_class", "Apply")
      )
    ), 
    tabPanel(
      "Plot", 
      fluidPage(
        fluidRow(
          column(
            4, 
            wellPanel(
              selectInput("plot_type", "Chart Type", choice = list('Please select' = "", 'Bar char' = "b", 'Line chart' = "l", 'Point chart' = "p"), ""), 
              selectInput("vrb_x", "Variable X", ""), 
              conditionalPanel(
                condition = "input.plot_type == 'l' | input.plot_type == 'p'", 
                selectInput("vrb_y", "Variable Y", "")
              ), 
              conditionalPanel(
                condition = "input.plot_type == 'b'", 
                selectInput("vrb_weight", "Weight", "")
              ), 
              selectInput("vrb_col", "Group (Colour)", ""), 
              conditionalPanel(
                condition = "input.plot_type == 'l'", 
                selectInput("vrb_lty", "Group (Linetype)", "")
              ), 
              conditionalPanel(
                condition = "input.plot_type == 'b'", 
                selectInput("bar_position", "Position", choice = list("stack", "dodge"))
              ), 
              actionButton("plot_buttom", "Plot")
            )
          ), 
          column(
            8, 
            wellPanel(
              plotOutput("plot")
            )
          )
        )
      )
    )
  )
)