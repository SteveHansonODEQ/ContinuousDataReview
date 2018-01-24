library(shiny)

shinyUI(fluidPage(
  verticalLayout(
    titlePanel("Continuous data check"),
    wellPanel(uiOutput("selectStation"),
              uiOutput("selectRange"),
              uiOutput("selectRange2")),
    plotOutput('plot', 
               click = "plot_click", 
               dblclick = "plot1_dblclick",
               brush = brushOpts(id = "plot1_brush",
                                 resetOnNew = TRUE)),
    wellPanel(h4("Selected Observation Data"),
              verbatimTextOutput("obs.info"),
              h4("Audit Info"),
              verbatimTextOutput("audit.info"))
    )
  )
)