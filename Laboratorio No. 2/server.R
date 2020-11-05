
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)


shinyServer(function(input, output) {
  
  # Inicializacion de Variables
  mensaje <- "Interactuar con Grafica"
  t <- mtcars
  p <- ggplot(data=mtcars, mapping=aes(x=wt,y=mpg), color='black',shape=18,size=3)+
    geom_point()+
    ylab("mpg")+
    xlab("wt")+
    ggtitle("Rendimiento vrs. Peso")
 
  # Estado Inicial del Dataframe
  output$plot_click_options <- renderPlot({
    capas()
  })
  
  # Reacciones de Capas ggplot2
  capas <-reactive({
    
    # Layer Click
    if(!is.null(input$clk$x)){
      puntos_click <- nearPoints(mtcars,input$clk,xvar="wt",yvar="mpg",threshold=15)
      p <<- p + geom_point(data=puntos_click, mapping = aes(x=wt,y=mpg), color='green',shape=18, size=3)
    }
    
    # Layer Doble Click
    if(!is.null(input$dclk$x)){
      puntos_dclick <- nearPoints(mtcars,input$dclk,xvar="wt",yvar="mpg",threshold = 10)
      p <<- p + geom_point(data=puntos_dclick, mapping = aes(x=wt,y=mpg), color='black',shape=18, size=3)
    }
    
    # Layer Brush
    if(!is.null(input$mbrush$xmin)){
      puntos_brush <- brushedPoints(mtcars,input$mbrush,xvar="wt",yvar="mpg")
      p <<- p + geom_point(data=puntos_brush, mapping = aes(x=wt,y=mpg), color='purple',shape=18, size=3)
    }
    
    # Layer Hover
    if(!is.null(input$mhover$x)){
      puntos_mhover <- nearPoints(mtcars,input$mhover,xvar="wt",yvar="mpg",threshold = 5)
      p <<- p + geom_point(data=puntos_mhover, mapping = aes(x=wt,y=mpg), color='gray',shape=18, size=3)
      
    }
    
    # Retorno de Layers
    p
  })

  
  
  # Reacciones de Texto  
  texto <-reactive({
    
    # Reaccion al Click
    if(!is.null(input$clk$x)){
      mensaje <<- paste0("CLICK: Coordenada x=", round(input$clk$x,2),
                        " Coordenada y=", round(input$clk$y,2))
    }
    
    # Reaccion al Doble Click
    if(!is.null(input$dclk$x)){
      mensaje <<- paste0("DOBLE CLICK: Coordenada x=", round(input$dclk$x,2),
        "; Coordenada y=", round(input$dclk$y,2))
    }
    
    # Reaccion a un Brush
    if(!is.null(input$mbrush$xmin)){
      brushx <- paste0("(",input$mbrush$xmin,",",input$mbrush$xmax,")")
      brushy <- paste0("(",input$mbrush$ymin,",",input$mbrush$ymax,")")
      mensaje <<- paste("BRUSH: Rango en x:[",brushx,"];","Rango en y:[",brushy,"]")
    }
    
    # Reaccion al Hover
    if(!is.null(input$mhover$x)){
      mensaje <<- paste0("HOVER: Coordenada x=", round(input$mhover$x,2),
                         " Coordenada y=", round(input$mhover$y,2))
    }
    
    # Mensaje Final
    mensaje
     
  })
  
  # Actualizacion de Texto
  output$click_data <- renderPrint({
    texto()
    
  })
  
  # Reacciones de Tabla
  tabla <- reactive({
    
    # Tabla con Informacion de Click
    if(!is.null(input$clk$x)){

      mtcars_df <- cbind(carname=row.names(mtcars),mtcars)
      t <<- nearPoints(mtcars_df,input$clk,xvar="wt",yvar="mpg")
    }
    
    # Tabla con Informacion de Brush
    if(!is.null(input$mbrush$xmin)){
      mtcars_df <- cbind(carname=row.names(mtcars),mtcars)
      t <<- brushedPoints(mtcars_df,input$mbrush,xvar="wt",yvar="mpg")
    }
    
    # Tabla Final
    t
  })
  
  # Actualizacion de Tabla
  output$mtcars_tbl <- DT::renderDataTable({
    tabla()
  })
    
})
