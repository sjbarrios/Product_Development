
library(shiny)

shinyUI(fluidPage(
  
  
  titlePanel("Grafica Interactiva Shiny"),
  
  tabPanel("Plot user Interactions",
           plotOutput("plot_click_options",
                      click = "clk",
                      dblclick = "dclk",
                      hover = "mhover",
                      brush = "mbrush"),
           h4("Informacion de Interaccion:"),
           verbatimTextOutput("click_data"),

           tabsetPanel(id="Panel",
                       tabPanel("Tabla de Puntos",
                                dataTableOutput("mtcars_tbl")
                       )
           )
           
           
           
  )
)
)
