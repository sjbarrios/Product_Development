library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)




Air_Quality <- read.csv(file = 'data/AirQualityUCI.csv', sep = ",")

Air_Quality$BORRAR <- NULL
Air_Quality$X <- NULL
Air_Quality$PT08.S1.CO. <- NULL
Air_Quality$PT08.S2.NMHC. <- NULL
Air_Quality$PT08.S3.NOx. <- NULL
Air_Quality$PT08.S4.NO2. <- NULL
Air_Quality$PT08.S5.O3. <- NULL
Air_Quality$Date <- as.Date(Air_Quality$Date,format = "%d/%m/%Y")



colnames(Air_Quality) <- c("Fecha","Hora","CO","NMHC","C6H6","NOx","NO2","Temp","HumRel","HumAbs")

Air_Quality <- as.data.frame(Air_Quality)

Air_Quality <- Air_Quality[Air_Quality$CO != -200 & Air_Quality$NMHC != -200 & Air_Quality$C6H6 != -200 & Air_Quality$NOx != -200 
                           & Air_Quality$NO2 != -200 & Air_Quality$Temp != -200 & Air_Quality$HumRel != -200 
                           & Air_Quality$HumAbs != -200, ] 



# Estadisticas
Numero_Observaciones <- nrow(Air_Quality)

#Medias
mediaCO <- round(mean(Air_Quality$CO,na.rm = TRUE),2)
mediaNMHC <- round(mean(Air_Quality$NMHC,na.rm = TRUE),2)
mediaC6H6 <- round(mean(Air_Quality$C6H6,na.rm = TRUE),2)
mediaNOx <- round(mean(Air_Quality$NOx,na.rm = TRUE),2)
mediaNO2 <- round(mean(Air_Quality$NO2,na.rm = TRUE),2)
mediaTemp <- round(mean(Air_Quality$Temp,na.rm = TRUE),2)
mediaHumRel <- round(mean(Air_Quality$HumRel,na.rm = TRUE),2)
mediaHumAbs <- round(mean(Air_Quality$HumAbs,na.rm = TRUE),2)

data_filtrada <- Air_Quality
puntos_click<- NULL
puntos_hover<-NULL

NO2_Temp_Quality <- Air_Quality[Air_Quality$NO2 != -200 & Air_Quality$Temp != -200, ] 

puntos_click_2<- NULL
puntos_hover_2<-NULL


shinyServer(function(input, output, session) {
    

    output$Observaciones <- renderInfoBox({ infoBox("Total de Muestras",Numero_Observaciones,icon = icon("plus")) })
    output$media_CO <- renderInfoBox({ infoBox("Media CO (ppm)",mediaCO,icon = icon("biohazard")) })
    output$media_NMHC <- renderInfoBox({ infoBox("Media NMHC (ppm)",mediaNMHC,icon = icon("atom")) })
    output$media_C6H6 <- renderInfoBox({ infoBox("Media C6H6 (ppm)",mediaC6H6,icon = icon("burn")) })
    output$media_NOx <- renderInfoBox({ infoBox("Media NOx (ppm)",mediaNOx,icon = icon("fire")) })
    output$media_NO2 <- renderInfoBox({ infoBox("Media NO2 (ppm)",mediaNO2,icon = icon("disease")) })
    output$media_Temp <- renderInfoBox({ infoBox("Media Temperatura (Celsius)",mediaTemp,icon = icon("temperature-high")) })
    output$media_HumRel <- renderInfoBox({ infoBox("Media Humedad Relativa (%)",mediaHumRel,icon = icon("umbrella")) })
    output$media_HumAbs <- renderInfoBox({ infoBox("Media Humedad Absoluta",mediaHumAbs,icon = icon("cloud-rain")) })
    
    
    # Plots de Contaminantes
    output$ScatterPlot<-renderPlot({
        p1 <- ggplot(Air_Quality, aes(x = as.numeric(row.names(Air_Quality)), y = NMHC))+ 
            geom_point(size=2.5,color='lightblue') + theme_dark()
        p2 <- ggplot(Air_Quality, aes(x = CO, y = C6H6))+ 
            geom_point(size=2.5,color='lightgray') + theme_dark()
        p3 <- ggplot(Air_Quality, aes(x = CO, y = NOx))+ 
            geom_point(size=2.5,color='pink') + theme_dark()
        grid.arrange(p1,p2,p3,ncol=3)
    })
    
    # Histogramas Datos Originales
    output$Histogramas1<-renderPlot({
        h1 <- ggplot(Air_Quality, aes(x = CO))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey()
        h2 <- ggplot(Air_Quality, aes(x = NMHC)) +
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey() 
        h3 <- ggplot(Air_Quality, aes(x = C6H6))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey()
        h4 <- ggplot(Air_Quality, aes(x = NOx))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey()
        grid.arrange(h1,h2,h3,h4,ncol=4)
    })
    
    output$Histogramas2<-renderPlot({
        h1 <- ggplot(Air_Quality, aes(x = NO2))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey()
        h2 <- ggplot(Air_Quality, aes(x = Temp))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey()
        h3 <- ggplot(Air_Quality, aes(x = HumRel))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey()
        h4 <- ggplot(Air_Quality, aes(x = HumAbs))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#66B3FF") + theme_grey()
        grid.arrange(h1,h2,h3,h4,ncol=4)
    })
    

    output$vbox <- renderValueBox({
        valueBox(
            "Air Quality Data Set",
            "ENEA - National Agency for New Technologies, Energy and Sustainable Economic Development",
            icon = icon("wind")
        )
    })
    
    # Render de Tabla Datos RAW
    output$CalidadAire <- renderDataTable({
        Air_Quality
    })
    
    
    # Filtros Reactivos
    filtro_tabla <- reactive({
        
        data_filtrada <<- Air_Quality
        
        if(!is.null(input$`Slider-Input`)){
            data_filtrada <<- data_filtrada %>% filter(CO >= input$`Slider-Input`[1] & CO <= input$`Slider-Input`[2])
        }
        
        if(!is.null(input$`Slider-Input2`)){
            data_filtrada <<- data_filtrada %>% filter(NMHC >= input$`Slider-Input2`[1] & NMHC <= input$`Slider-Input2`[2])
        }
        
        if(!is.null(input$`Slider-Input3`)){
            data_filtrada <<- data_filtrada %>% filter(C6H6 >= input$`Slider-Input3`[1] & C6H6 <= input$`Slider-Input3`[2])
        }
        
        if(!is.null(input$`Slider-Input4`)){
            data_filtrada <<- data_filtrada %>% filter(NOx >= input$`Slider-Input4`[1] & NOx <= input$`Slider-Input4`[2])
        }
        
        if(!is.null(input$`Slider-Input5`)){
            data_filtrada <<- data_filtrada %>% filter(NO2 >= input$`Slider-Input5`[1] & NO2 <= input$`Slider-Input5`[2])
        }
        
        if(!is.null(input$`Slider-Input6`)){
            data_filtrada <<- data_filtrada %>% filter(Temp >= input$`Slider-Input6`[1] & Temp <= input$`Slider-Input6`[2])
        }
        
        if(!is.null(input$`Slider-Input7`)){
            data_filtrada <<- data_filtrada %>% filter(HumRel >= input$`Slider-Input7`[1] & HumRel <= input$`Slider-Input7`[2])
        }
        
        if(!is.null(input$`Slider-Input8`)){
            data_filtrada <<- data_filtrada %>% filter(HumAbs >= input$`Slider-Input8`[1] & HumAbs <= input$`Slider-Input8`[2])
        }
        
        if(!is.null(input$`Date-Input`)){
            data_filtrada <<- data_filtrada %>% filter(Fecha >= input$`Date-Input`[1] & Fecha <= input$`Date-Input`[2])
        }
        
        
        return(data_filtrada)
        
    })
    
    # Salida Tabla Filtrada
    output$CalidadAireFiltrada <- renderDataTable({
        filtro_tabla()
    })
    
    
    # Histogramas Datos Filtrados
    filtered_dist <- reactive({
        if(!is.null(input$`Slider-Input`) & !is.null(input$`Slider-Input2`) & !is.null(input$`Slider-Input3`) & !is.null(input$`Slider-Input4`) & 
           !is.null(input$`Slider-Input5`) & !is.null(input$`Slider-Input6`) & !is.null(input$`Slider-Input7`) & !is.null(input$`Slider-Input8`) ){
            
            return(filtro_tabla()) 
        }
        
    })
    
    output$Histogramas3<-renderPlot({
        h1 <- ggplot(filtered_dist(), aes(x = CO))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(CO,na.rm = TRUE)),col='red',size=0.5) + theme_grey()
        h2 <- ggplot(filtered_dist(), aes(x = NMHC)) +
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(NMHC,na.rm = TRUE)),col='red',size=0.5) + theme_grey() 
        h3 <- ggplot(filtered_dist(), aes(x = C6H6))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(C6H6,na.rm = TRUE)),col='red',size=0.5) + theme_grey()
        h4 <- ggplot(filtered_dist(), aes(x = NOx))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(NOx,na.rm = TRUE)),col='red',size=0.5) + theme_grey()
        grid.arrange(h1,h2,h3,h4,ncol=4)
    })
    
    output$Histogramas4<-renderPlot({
        h1 <- ggplot(filtered_dist(), aes(x = NO2))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(NO2,na.rm = TRUE)),col='red',size=0.5) + theme_grey()
        h2 <- ggplot(filtered_dist(), aes(x = Temp))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(Temp,na.rm = TRUE)),col='red',size=0.5) + theme_grey()
        h3 <- ggplot(filtered_dist(), aes(x = HumRel))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(HumRel,na.rm = TRUE)),col='red',size=0.5) + theme_grey()
        h4 <- ggplot(filtered_dist(), aes(x = HumAbs))+ 
            geom_histogram(aes(y=..density..)) + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept = mean(HumAbs,na.rm = TRUE)),col='red',size=0.5) + theme_grey()
        grid.arrange(h1,h2,h3,h4,ncol=4)
    })
    
    

    ### -----------------------------------------------------------------------------------------------------
    ### FILTROS URL
    ### -----------------------------------------------------------------------------------------------------

    # Filtro por URL de Niveles de CO & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        COmin <- query[["COmin"]]
        COmax <- query[["COmax"]]
        if(!is.null(COmin) & !is.null(COmax)){
            COmin <- as.integer(COmin)
            COmax <- as.integer(COmax)
            print(COmin)
            updateSliderInput(session,"Slider-Input",value=c(COmin,COmax))
        }
    })
    
    # Filtro por URL de Niveles de NMHC & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        NMHCmin <- query[["NMHCmin"]]
        NMHCmax <- query[["NMHCmax"]]
        if(!is.null(NMHCmin) & !is.null(NMHCmax)){
            NMHCmin <- as.integer(NMHCmin)
            NMHCmax <- as.integer(NMHCmax)
            updateSliderInput(session,"Slider-Input2",value=c(NMHCmin,NMHCmax))
        }
    })
    
    # Filtro por URL de Niveles de C6H6 & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        C6H6min <- query[["C6H6min"]]
        C6H6max <- query[["C6H6max"]]
        if(!is.null(C6H6min) & !is.null(C6H6max)){
            C6H6min <- as.integer(C6H6min)
            C6H6max <- as.integer(C6H6max)
            updateSliderInput(session,"Slider-Input3",value=c(C6H6min,C6H6max))
        }
    })
    
    
    # Filtro por URL de Niveles de NOx & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        NOxmin <- query[["NOxmin"]]
        NOxmax <- query[["NOxmax"]]
        if(!is.null(NOxmin) & !is.null(NOxmax)){
            NOxmin <- as.integer(NOxmin)
            NOxmax <- as.integer(NOxmax)
            updateSliderInput(session,"Slider-Input4",value=c(NOxmin,NOxmax))
        }
    })
    
    # Filtro por URL de Niveles de NO2 & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        NO2min <- query[["NO2min"]]
        NO2max <- query[["NO2max"]]
        if(!is.null(NO2min) & !is.null(NO2max)){
            NO2min <- as.integer(NO2min)
            NO2max <- as.integer(NO2max)
            updateSliderInput(session,"Slider-Input5",value=c(NO2min,NO2max))
        }
    })
    
    # Filtro por URL de Niveles de Temperatura & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        Tempmin <- query[["Tempmin"]]
        Tempmax <- query[["Tempmax"]]
        if(!is.null(Tempmin) & !is.null(Tempmax)){
            Tempmin <- as.integer(Tempmin)
            Tempmax <- as.integer(Tempmax)
            updateSliderInput(session,"Slider-Input6",value=c(Tempmin,Tempmax))
        }
    })
    
    # Filtro por URL de Niveles de Humedad Relativa & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        HumRelmin <- query[["HumRelmin"]]
        HumRelmax <- query[["HumRelmax"]]
        if(!is.null(HumRelmin) & !is.null(HumRelmax)){
            HumRelmin <- as.integer(HumRelmin)
            HumRelmax <- as.integer(HumRelmax)
            updateSliderInput(session,"Slider-Input7",value=c(HumRelmin,HumRelmax))
        }
    })
    
    # Filtro por URL de Niveles de Humedad Absoluta & Update de Slider
    observe({
        query <- parseQueryString(session$clientData$url_search)
        HumAbsmin <- query[["HumAbsmin"]]
        HumAbsmax <- query[["HumAbsmax"]]
        if(!is.null(HumAbsmin) & !is.null(HumAbsmax)){
            HumAbsmin <- as.integer(HumAbsmin)
            HumAbsmax <- as.integer(HumAbsmax)
            updateSliderInput(session,"Slider-Input8",value=c(HumAbsmin,HumAbsmax))
        }
    })
    
    # Filtro por URL de Fechas & Update de DateRangeInput
    observe({
        query <- parseQueryString(session$clientData$url_search)
        Datemin <- query[["Datemin"]]
        Datemax <- query[["Datemax"]]
        if(!is.null(Datemin) & !is.null(Datemax)){
            Datemin <- as.Date(Datemin)
            Datemax <- as.Date(Datemax)
            updateDateRangeInput(session,"Date-Input",start = Datemin, end = Datemax)
        }
    })
    
    ### -----------------------------------------------------------------------------------------------------
    ### DESPLIEGUE DE FILTROS URL
    ### -----------------------------------------------------------------------------------------------------   
    observe({
        URLCOmin<-input$'Slider-Input'[1]
        URLCOmax<-input$'Slider-Input'[2]
        link_urlCO<-paste0("http://",session$clientData$url_hostname,":",
                         session$clientData$url_port,
                         session$clientData$url_pathname,
                         "?COmin=",URLCOmin,"&",
                         "COmax=",URLCOmax)
        updateTextInput(session,"urlqueryCO",value=link_urlCO)
    })
    
    observe({
        URLNMHCmin<-input$'Slider-Input2'[1]
        URLNMHCmax<-input$'Slider-Input2'[2]
        link_urlNMHC<-paste0("http://",session$clientData$url_hostname,":",
                           session$clientData$url_port,
                           session$clientData$url_pathname,
                           "?NMHCmin=",URLNMHCmin,"&",
                           "NMHCmax=",URLNMHCmax)
        updateTextInput(session,"urlqueryNMHC",value=link_urlNMHC)
    })
    
    observe({
        URLC6H6min<-input$'Slider-Input3'[1]
        URLC6H6max<-input$'Slider-Input3'[2]
        link_urlC6H6<-paste0("http://",session$clientData$url_hostname,":",
                             session$clientData$url_port,
                             session$clientData$url_pathname,
                             "?C6H6min=",URLC6H6min,"&",
                             "C6H6max=",URLC6H6max)
        updateTextInput(session,"urlqueryC6H6",value=link_urlC6H6)
    })
    
    observe({
        URLNOxmin<-input$'Slider-Input4'[1]
        URLNOxmax<-input$'Slider-Input4'[2]
        link_urlNOx<-paste0("http://",session$clientData$url_hostname,":",
                             session$clientData$url_port,
                             session$clientData$url_pathname,
                             "?NOxmin=",URLNOxmin,"&",
                             "NOxmax=",URLNOxmax)
        updateTextInput(session,"urlqueryNOx",value=link_urlNOx)
    })
    
    observe({
        URLNO2min<-input$'Slider-Input5'[1]
        URLNO2max<-input$'Slider-Input5'[2]
        link_urlNO2<-paste0("http://",session$clientData$url_hostname,":",
                            session$clientData$url_port,
                            session$clientData$url_pathname,
                            "?NO2min=",URLNO2min,"&",
                            "NO2max=",URLNO2max)
        updateTextInput(session,"urlqueryNO2",value=link_urlNO2)
    })
    
    observe({
        URLTempmin<-input$'Slider-Input6'[1]
        URLTempmax<-input$'Slider-Input6'[2]
        link_urlTemp<-paste0("http://",session$clientData$url_hostname,":",
                            session$clientData$url_port,
                            session$clientData$url_pathname,
                            "?Tempmin=",URLTempmin,"&",
                            "Tempmax=",URLTempmax)
        updateTextInput(session,"urlqueryTemp",value=link_urlTemp)
    })
    
    observe({
        URLHumRelmin<-input$'Slider-Input7'[1]
        URLHumRelmax<-input$'Slider-Input7'[2]
        link_urlHumRel<-paste0("http://",session$clientData$url_hostname,":",
                             session$clientData$url_port,
                             session$clientData$url_pathname,
                             "?HumRelmin=",URLHumRelmin,"&",
                             "HumRelmax=",URLHumRelmax)
        updateTextInput(session,"urlqueryHumRel",value=link_urlHumRel)
    })
    
    observe({
        URLHumAbsmin<-input$'Slider-Input8'[1]
        URLHumAbsmax<-input$'Slider-Input8'[2]
        link_urlHumAbs<-paste0("http://",session$clientData$url_hostname,":",
                               session$clientData$url_port,
                               session$clientData$url_pathname,
                               "?HumAbsmin=",URLHumAbsmin,"&",
                               "HumAbsmax=",URLHumAbsmax)
        updateTextInput(session,"urlqueryHumAbs",value=link_urlHumAbs)
    })
    
    observe({
        URLDatemin<-input$'Date-Input'[1]
        URLDatemax<-input$'Date-Input'[2]
        link_urlDate<-paste0("http://",session$clientData$url_hostname,":",
                               session$clientData$url_port,
                               session$clientData$url_pathname,
                               "?Datemin=",URLDatemin,"&",
                               "Datemax=",URLDatemax)
        updateTextInput(session,"urlqueryDate",value=link_urlDate)
    })
    
    
    # Plot interactivo
    
    
    #Grafico reactivo NO2 y NOx
    output$air_graph <- renderPlot({
      Air_Quality_plot()
    })
    
    
    #Cambios en los puntos por reaccion
    
    Air_Quality_plot <- reactive({
      #Despliege del grafico
      plot(Air_Quality$NO2,
           Air_Quality$NOx,
           main="Oxidos de Nitrogeno (NO2 y NOx)",
           xlab="NO2",
           ylab="NOx") 
      puntos <- nuevos_puntos()
      
      #Cambios en click
      if(!is.null(puntos_click)){
        #Dibuja los puntos 
        points(puntos_click[,1], #x
               puntos_click[,2], #y
               pch=19,  #Rombo peq
               col="blue", #color azul
               cex=1) #Sin cambio tam
        
        #cambio en hover
      } else if(!is.null(puntos_hover)){
        #Dibuja los puntos
        points(puntos_hover[,1],
               puntos_hover[,2],
               pch=16, #Rombo peq
               bg="yellow",
               col="red", #Color Rojo
               cex=1) #Sin cambio tam
      }  
    })
    
    #Seleccion de puntos en click o hover
    nuevos_puntos <- reactive({
      #Para click
      if(!is.null(input$click_air_graph$x)){
        puntos_sel<-nearPoints(Air_Quality,
                               input$click_air_graph,
                               threshold = 10,
                               xvar='NO2',
                               yvar='NOx')
        out <- puntos_sel %>% 
          select(NO2,NOx)
        #Agrega el nuevo punto en click
        puntos_click <<- rbind(puntos_click,out) %>% distinct()
        return(out)
        
        #Para puntos en doble click
      } else if(!is.null(input$dblclck_air_graph$x)){
        puntos_sel<-nearPoints(Air_Quality,
                               input$dblclck_air_graph,
                               threshold = 10,
                               xvar='NO2',
                               yvar='NOx')
        out <- puntos_sel %>% 
          select(NO2,NOx)
        
        #Elimina el punto del data table 
        puntos_click <<- setdiff(puntos_click,out)
        return(puntos_hover)
        
        #Para puntos en hover
      } else if(!is.null(input$hover_air_graph$x)){
        puntos_sel<-nearPoints(Air_Quality,
                               input$hover_air_graph,
                               threshold = 10,
                               xvar='NO2',
                               yvar='NOx')
        out <- puntos_sel %>% 
          select(NO2,NOx)
        
        #Muestra punto en hover
        puntos_hover <<- out
        return(puntos_hover)
        
        #Para puntos en brush
      } else if(!is.null(input$brush_air_graph)){
        puntos_sel<-brushedPoints(Air_Quality,
                                  input$brush_air_graph,
                                  xvar='NO2',
                                  yvar='NOx')
        out <- puntos_sel %>% 
          select(NO2,NOx)
        #Agrega los nuevos puntos en brush
        puntos_click <<- rbind(puntos_click,out) %>% dplyr::distinct()
        return(puntos_hover)
      }
      
    })
    
    
    #Tabla de puntos
    click_table <- reactive({
      input$click_air_graph$x
      input$dblclck_air_graph$x
      input$brush_air_graph
      puntos_click
    })
    
    #Tabla reactiva NO2 y NOx
    output$air_table <- DT::renderDataTable({
      click_table() %>% DT::datatable()
    })
    
    
    
    
    
    #########################
    #Grafico reactivo NO2 y Temp
    #########################
    
    output$no2_temp_graph <- renderPlot({
      Air_Quality_plot_2()
    })
    
    
    #Cambios en los puntos por reaccion
    
    Air_Quality_plot_2 <- reactive({
      #Despliege del grafico
      plot(NO2_Temp_Quality$NO2,
           NO2_Temp_Quality$Temp,
           main="Oxido de Nitrogeno y Temperatura (NO2 y Temp)",
           xlab="NO2",
           ylab="Temp") 
      puntos <- nuevos_puntos_no2_temp()
      
      #Cambios en click
      if(!is.null(puntos_click_2)){
        #Dibuja los puntos 
        points(puntos_click_2[,1], #x
               puntos_click_2[,2], #y
               pch=19,  #Rombo peq
               col="blue", #color azul
               cex=1) #Sin cambio tam
        
        #cambio en hover
      } else if(!is.null(puntos_hover_2)){
        #Dibuja los puntos
        points(puntos_hover_2[,1],
               puntos_hover_2[,2],
               pch=16, #Rombo peq
               bg="yellow",
               col="red", #Color Rojo
               cex=1) #Sin cambio tam
      }  
    })
    
    #Seleccion de puntos en click o hover
    nuevos_puntos_no2_temp <- reactive({
      #Para click
      if(!is.null(input$click_no2_temp_graph$x)){
        puntos_sel<-nearPoints(NO2_Temp_Quality,
                               input$click_no2_temp_graph,
                               xvar='NO2',
                               threshold = 10,
                               yvar='Temp')
        out <- puntos_sel %>% 
          select(NO2,Temp)
        #Agrega el nuevo punto en click
        puntos_click_2 <<- rbind(puntos_click_2,out) %>% distinct()
        return(out)
        
        #Para puntos en doble click
      } else if(!is.null(input$dblclck_no2_temp_graph$x)){
        puntos_sel<-nearPoints(NO2_Temp_Quality,
                               input$dblclck_no2_temp_graph,
                               threshold = 10,
                               xvar='NO2',
                               yvar='Temp')
        out <- puntos_sel %>% 
          select(NO2,Temp)
        
        #Elimina el punto del data table 
        puntos_click_2 <<- setdiff(puntos_click_2,out)
        return(puntos_hover_2)
        
        #Para puntos en hover
      } else if(!is.null(input$hover_no2_temp_graph$x)){
        puntos_sel<-nearPoints(NO2_Temp_Quality,
                               input$hover_no2_temp_graph,
                               threshold = 10,
                               xvar='NO2',
                               yvar='Temp')
        out <- puntos_sel %>% 
          select(NO2,Temp)
        
        #Muestra punto en hover
        puntos_hover_2 <<- out
        return(puntos_hover_2)
        
        #Para puntos en brush
      } else if(!is.null(input$brush_no2_temp_graph)){
        puntos_sel<-brushedPoints(NO2_Temp_Quality,
                                  input$brush_no2_temp_graph,
                                  xvar='NO2',
                                  yvar='Temp')
        out <- puntos_sel %>% 
          select(NO2,Temp)
        #Agrega los nuevos puntos en brush
        puntos_click_2 <<- rbind(puntos_click_2,out) %>% dplyr::distinct()
        return(puntos_hover_2)
      }
      
    })
    
    
    #Tabla de puntos
    click_table_no2_temp <- reactive({
      input$click_no2_temp_graph$x
      input$dblclck_no2_temp_graph$x
      input$brush_no2_temp_graph
      puntos_click_2
    })
    
    #Tabla reactiva NO2 y Temp
    output$no2_temp_table <- DT::renderDataTable({
      click_table_no2_temp() %>% DT::datatable()
    })
  
})