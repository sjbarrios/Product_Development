#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Laboratorio No. 1"),
    
    tabsetPanel(
        tabPanel("Shiny Inputs",
        

                sidebarLayout(
                    
                    sidebarPanel(
                        
                        # Ejemplo de Slider Input Sencillo
                        sliderInput("Slider-Input",
                            "Mueva el slider:",
                            value = 50,
                            min = 0,
                            max = 100,
                            step = 10,
                            post = '%',
                            animate = TRUE
                        ),
                        
                        # Ejemplo de Slider Input con Rango
                        sliderInput("Slider-Input2",
                                    "Delimite un rango:",
                                    value = c(0,200),
                                    min = 0,
                                    max = 200,
                                    step = 10,
                                    post = '%',
                                    animate = TRUE
                        ),
                        
                        # Ejemplo de Select Input
                        selectInput("Select-Input",
                                    "Seleccione una Entrada:",
                                    choices=rownames(mtcars),
                                    selected = "Camaro Z28",
                                    multiple = FALSE
                        ),
                        
                        # Ejemplo de Select Input   
                        selectizeInput("Selectize-Input",
                                    "Seleccione varias Entradas:",
                                    choices=rownames(mtcars),
                                    selected = "Camaro Z28",
                                    multiple = TRUE
                        ),
                        
                        # Ejemplo de Fecha
                        dateInput("Date-Input",
                                    "Ingrese una fecha:",
                                    value=today(),
                                    min=today()-60,
                                    max = today()+30,
                                    language = 'es',
                                    weekstart = 1
                        ),
                        
                        # Ejemplo de Rangos de Fecha
                        dateRangeInput("DateRange-Input",
                                     "Ingrese Rango de Fechas:",
                                     start = Sys.Date() - 2, 
                                     end = Sys.Date() + 2
                        ),
                        
                        # Ejemplo de Entrada Numerica
                        numericInput("Numeric-Input",
                                       "Ingrese un numero (0-100):",
                                       value = 10,
                                       min = 0,
                                       max = 100,
                                       step = 1
                        ),
                        
                        # Ejemplo de CheckBox
                        checkboxInput("checkbox-Input",
                                     "Seleccione para Verdadero",
                                     value = FALSE
                        ),
                        
                        # Ejemplo de CheckBox Multiple
                        checkboxGroupInput("groupbox-Input",
                                      "Seleccione Opciones:",
                                      choices = LETTERS[1:5]
                        ),
                        
                        # Ejemplo de TextBox
                        textInput("textbox-Input",
                                           "Ingrese texto:"
                        ),
                        
                        # Ejemplo de Text Area
                        textAreaInput("textarea-Input",
                                  "Ingrese parrafo:"
                        ),
                        
                        # Ejemplo de Action Button
                        actionButton("action_button",
                                      "ok"
                        ),
                        
                        # Ejemplo de Action Button
                        actionLink("action_link",
                                     "Siguiente"
                        ),
                        
                        # Ejemplo de Radio Button
                        radioButtons("radio_buttons",
                                   "Seleccione Genero",
                                   choices = c("masculino","femenino")
                        ),
                        
                        # Ejemplo de Entrada Password
                        passwordInput("Password-Input",
                                     "Ingrese su password:"
                        ),
                        
                        # Submit Button
                        submitButton(text = "Submit"
                        )

                    ),
                
                    # Panel Principal
                    mainPanel(
                        
                        p("Ejemplos de Entradas en Shiny App.", style = "font-family: 'times'; font-si16pt"),
                        p("(Utilizando Submit Button al final)", style = "font-family: 'times'; font-si5pt"),
                        
                        h2("Slider Input Sencillo"),
                        verbatimTextOutput("slider-io"),
                        
                        h2("Slider Input con Rango"),
                        verbatimTextOutput("slider-io2"),
                        
                        h2("Select Input"),
                        verbatimTextOutput("select-io"),
                        
                        h2("Selectize Input (Multiple)"),
                        verbatimTextOutput("selectize-io"),
                        
                        h2("Date Input"),
                        verbatimTextOutput("date-io"),
                        
                        h2("DateRange Input"),
                        verbatimTextOutput("daterange-io"),
                        
                        h2("Numeric Input"),
                        verbatimTextOutput("numeric-io"),
                        
                        h2("CheckBox Input"),
                        verbatimTextOutput("checkbox-io"),
                        
                        h2("GroupBox Input"),
                        verbatimTextOutput("groupbox-io"),
                        
                        h2("TextBox Input"),
                        verbatimTextOutput("textbox-io"),
                        
                        h2("TextArea Input"),
                        verbatimTextOutput("textarea-io"),
                        
                        h2("Action Button"),
                        verbatimTextOutput("actionbutton-io"),
                        
                        h2("Action Link"),
                        verbatimTextOutput("actionlink-io"),
                        
                        h2("Radio Buttons"),
                        verbatimTextOutput("radiobuttons-io"),
                        
                        h2("Password Input"),
                        verbatimTextOutput("password-io")
                    )
                ),
        )
    )
))
