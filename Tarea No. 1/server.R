
library(shiny)
library(readr)
library(DT)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  # Carga de Archivo No. 1
  archivo_carga_1 <- reactive({
    
    if(is.null(input$upload_file_1)){
      return(NULL)
    }

    ext<-strsplit(input$upload_file_1$name,split = "[.]")[[1]][2]
    
    if(ext == 'csv'){
      file_data <- read_csv(input$upload_file_1$datapath)
      return(file_data)
    }
    if(ext == 'tsv'){
      file_data <- read_tsv(input$upload_file_1$datapath)
      return(file_data)
    }
    return(NULL)

  })
  
  # Salida Tabla Archivo No. 1
  output$contenido_archivo_1 <- renderTable({
    archivo_carga_1()
  })
  
  
  # Carga de Archivo No. 1
  archivo_carga_2 <- reactive({
    
    if(is.null(input$upload_file_2)){
      return(NULL)
    }

    ext<-strsplit(input$upload_file_2$name,split = "[.]")[[1]][2]
    
    if(ext == 'csv'){
      file_data <- read_csv(input$upload_file_2$datapath)
      return(file_data)
    }
    if(ext == 'tsv'){
      file_data <- read_tsv(input$upload_file_2$datapath)
      return(file_data)
    }
    return(NULL)
    
  })
  
  # Salida Tabla con Pagineo Archivo No. 2
  output$contenido_archivo_2 <- renderDataTable({
    archivo_carga_2() %>% datatable(filter="bottom")
  })
  
  # Tabla No. 1 Pestana DT Option
  output$tabla1<-renderDataTable({
    diamonds %>% 
      datatable() %>%
      formatCurrency("price") %>% 
      formatString(c("x","y","z"),suffix = " mm")
  })
  
  # Tabla No. 2 Pestana DT Option
  output$tabla2<-renderDataTable({
    mtcars %>% 
      datatable(options = list(pageLength=5,
                                lengthMenu=c(5,10,15)
                               ),
                              filter = "top"
                              )
  })
  
  # Tabla No. 3 Pestana DT Option
  output$tabla3<-renderDataTable({
    iris %>% 
      datatable(extensions = "Buttons",
                options = list(dom="Bfrtip",
                               buttons=("csv")
                               ),
                rownames = FALSE
                )
  })
  
  # Tabla No. 4 Pestana Clicks en Tabla
  output$tabla4<-renderDataTable({
    mtcars %>% 
      datatable(selection = "single")

  })
  
  # Tabla No. 4 Pestana Clicks en Tabla (Fila seleccionada)
  output$tabla_4_single_click<-renderText({
    input$tabla4_rows_selected
    
  })

  # Tabla No. 5 Pestana Clicks en Tabla
  output$tabla5<-renderDataTable({
    mtcars %>% 
      datatable()
    
  })
  
  # Tabla No. 5 Pestana Clicks en Tabla (Filas seleccionadas)
  output$tabla_5_multi_click<-renderText({
    input$tabla5_rows_selected
    
  })  

  # Tabla No. 6 Pestana Clicks en Tabla
  output$tabla6<-renderDataTable({
    mtcars %>% 
      datatable(selection = list(mode = "single", target = "column"))
    
  })
  
  # Tabla No. 6 Pestana Clicks en Tabla (Columna seleccionada)
  output$tabla_6_single_click<-renderText({
    input$tabla6_columns_selected
    
  })
  
  # Tabla No. 7 Pestana Clicks en Tabla
  output$tabla7<-renderDataTable({
    mtcars %>% 
      datatable(selection = list(mode = "multiple", target = "column"))
    
  })
  
  # Tabla No. 7 Pestana Clicks en Tabla (Columnas seleccionadas)
  output$tabla_7_multi_click<-renderText({
    input$tabla7_columns_selected
    
  })
  
  
  # Tabla No. 8 Pestana Clicks en Tabla
  output$tabla8<-renderDataTable({
    mtcars %>% 
      datatable(selection = list(mode = "single", target = "cell"))
    
  })
  
  # Tabla No. 8 Pestana Clicks en Tabla (Celda seleccionada)
  output$tabla_8_single_click<-renderPrint({
    input$tabla8_cells_selected
    
  })
  
  # Tabla No. 9 Pestana Clicks en Tabla
  output$tabla9<-renderDataTable({
    mtcars %>% 
      datatable(selection = list(mode = "multiple", target = "cell"))
    
  })
  
  # Tabla No. 9 Pestana Clicks en Tabla (Celdas seleccionadas)
  output$tabla_9_multi_click<-renderPrint({
    input$tabla9_cells_selected
    
  })

})
