
library(shiny)
library(shinydashboard)
library(lubridate)

# USER INTERFACE
shinyUI(
    
    dashboardPage(
        dashboardHeader(title = "Product Development",titleWidth = 300),
        
        dashboardSidebar(
            width = 300,
            
            
            ## Menu Lateral
            
            sidebarMenu(
                
                menuItem("General Dashboard", tabName = "dashboard"), 
                menuItem("Raw data", tabName = "rawdata"),
                menuItem("Filtered data", tabName = "filterdata"),
                menuItem("Filtered Histograms", tabName = "filterhist"),
                menuItem("URL Queries", tabName = "urlqueries"),
                menuItem("Plots interactivos", tabName = "interactiveplots")
            ),
            
            # Slider Input Sencillo
            sliderInput("Slider-Input", "Rango de CO (ppms):", value = c(0,10), min = 0, max = 10, step = 0.1, animate = TRUE),
            sliderInput("Slider-Input2", "Rango de NMHC (ppms):", value = c(0,1200), min = 0, max = 1200, step = 1, animate = TRUE),
            sliderInput("Slider-Input3", "Rango de C6H6 (ppms):", value = c(0,40), min = 0, max = 40, step = 0.5, animate = TRUE),
            sliderInput("Slider-Input4", "Rango de NOx (ppms):", value = c(0,500), min = 0, max = 500, step = 1, animate = TRUE),
            sliderInput("Slider-Input5", "Rango de NO2 (ppms):", value = c(0,200), min = 0, max = 200, step = 1, animate = TRUE),
            sliderInput("Slider-Input6", "Rango de Temperatura (Celsius):", value = c(0,30), min = 0, max = 30, step = 0.5, animate = TRUE),
            sliderInput("Slider-Input7", "Rango de Humedad Relativa (%):", value = c(10,90), min = 10, max = 90, step = 1, animate = TRUE),
            sliderInput("Slider-Input8", "Rango de Humedad Absoluta:", value = c(0.4,1.5), min = 0.4, max = 1.5, step = 0.01, animate = TRUE),
            
            # Date Input con Rango
            dateRangeInput("Date-Input", "Seleccione Rangos de Fecha:", start = '2004-03-10', end = '2005-04-04', max = '2005-04-04', min = '2004-03-10', separator = 'a')
        ),
        
        dashboardBody(
        
            tabItems(
            
                tabItem("dashboard",
                
                fluidRow(
                    valueBoxOutput("vbox")  
                ),                
                        
            
                fluidRow(
                    infoBoxOutput("Observaciones"),
                    infoBoxOutput("media_CO"),
                    infoBoxOutput("media_NMHC"),
                    infoBoxOutput("media_C6H6"),
                    infoBoxOutput("media_NOx"),
                    infoBoxOutput("media_NO2"),
                    infoBoxOutput("media_Temp"),
                    infoBoxOutput("media_HumRel"),
                    infoBoxOutput("media_HumAbs"),
                        ),
            
                fluidRow(column(width=12, plotOutput('Histogramas1'))),
                fluidRow(column(width=12, plotOutput('Histogramas2')))
            
                
                ),
                
                tabItem("rawdata",
                    fluidRow(
                            dataTableOutput("CalidadAire")
                    )   
                ),
                
                tabItem("filterdata",
                        fluidRow(
                            dataTableOutput("CalidadAireFiltrada")
                        )   
                ),
                
                tabItem("filterhist",
                        fluidRow(column(width=12, plotOutput('Histogramas3'))),
                        fluidRow(column(width=12, plotOutput('Histogramas4')))  
                ),
                
                tabItem("urlqueries",
                        fluidRow(column(width=12, textInput('urlqueryCO','Query Rango CO (ppms)',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryNMHC','Query Rango NMHC (ppms)',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryC6H6','Query Rango C6H6 (ppms)',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryNOx','Query Rango NOx (ppms)',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryNO2','Query Rango NO2 (ppms)',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryTemp','Query Rango Temperatura (Celsius)',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryHumRel','Query Rango Humedad Relativa (%)',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryHumAbs','Query Rango Humedad Absoluta',value = "", width = '425px'))),
                        fluidRow(column(width=12, textInput('urlqueryDate','Query Rango Fechas (yyyy-mm-dd)',value = "", width = '425px')))
                ),
                
                tabItem("interactiveplots",
                        plotOutput('air_graph',
                                   click = 'click_air_graph',
                                   dblclick = 'dblclck_air_graph',
                                   hover = 'hover_air_graph',
                                   brush = 'brush_air_graph'
                        ),
                        DT::dataTableOutput('air_table'),
                        plotOutput('no2_temp_graph',
                                   click = 'click_no2_temp_graph',
                                   dblclick = 'dblclck_no2_temp_graph',
                                   hover = 'hover_no2_temp_graph',
                                   brush = 'brush_no2_temp_graph'
                        ),
                        DT::dataTableOutput('no2_temp_table')
                )
                
            )
        )
    )
)