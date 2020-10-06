#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$'slider-io' <- renderText({
        
        paste0(c('Output Slider Input: ',input$`Slider-Input`),
               collapse = '')
    })
    
    output$'slider-io2' <- renderText({
        
        paste0(c('Output Slider Input: ',input$`Slider-Input2`),
               collapse = ' ')
    })
    
    output$'select-io' <- renderText({
        
        paste0(c('Output Select Input: ',input$`Select-Input`),
               collapse = ' ')
    })
    
    output$'selectize-io' <- renderText({
        
        paste0(c('Output Select Input: ',paste(c(input$`Selectize-Input`),collapse=', ')),
               collapse = ' ')
        
        
    })
    
    output$'date-io' <- renderText({
        
       as.character(input$`Date-Input`)
    })
    
    output$'daterange-io'  <- renderText({
        paste("Rango de Fechas:", 
              paste(as.character(input$`DateRange-Input`), collapse = " a ")
        )
    })
    
    output$'numeric-io'  <- renderText({
        paste("Numero ingresado:", 
              paste(input$`Numeric-Input`)
        )
    })
    
    output$'checkbox-io' <- renderText({
        
        as.character(input$`checkbox-Input`)
    })
    
    output$'groupbox-io' <- renderText({
        
        as.character(input$`groupbox-Input`)
    })
    
    output$'textbox-io' <- renderText({
        
        as.character(input$`textbox-Input`)
    })
    
    output$'textarea-io' <- renderText({
        
        as.character(input$`textarea-Input`)
    })
    
    output$'actionbutton-io' <- renderText({
        
        as.character(input$`action_button`)
    })
    
    output$'actionlink-io' <- renderText({
        
        as.character(input$`action_link`)
    })
    
    output$'radiobuttons-io' <- renderText({
        
        as.character(input$`radio_buttons`)
    })
    
    output$'password-io'  <- renderText({
        paste("Password ingresado:", 
              paste(input$`Password-Input`)
        )
    })

})
