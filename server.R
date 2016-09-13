library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate

# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.metronhomo.com.mx/">Metronhomo</a>'
      ) %>%
      setView(lng = -116.9, lat = 32.6, zoom = 10)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  datosEnRango <- reactive({
    if (is.null(input$map_bounds))
      return(datos[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(datosAno,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
 
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    

      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- datos$percentil
      pal <- colorFactor(rainbow(4), colorData)
    
      
    leafletProxy("map", data = datos) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=300, layerId=~ID,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title='Montos',
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(ID, lat, lng) {
    selectedZip <- datos[datos$ID == ID,]
    content <- as.character(tagList(
      tags$h4(selectedZip$envioSucursalNombre),
      sprintf("Monto promedio: %s", round(selectedZip$montoPromedio,0)),tags$br(),
      sprintf("Porcentaje de comisión promedio: %s", round(selectedZip$comisionPromedio*100,2)), tags$br(),
      sprintf("Estado: %s", selectedZip$estado), tags$br(),
      sprintf("Localidad: %s", selectedZip$ciudad), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ID)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  output$histMontos <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(datosEnRango()) == 0)
      return(NULL)
    ggplot(datosEnRango()[datosEnRango()$monto<=5000,],aes(x=monto))+
      geom_histogram(fill='blue')+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")+
      xlab("Monto")+ylab('Conteo')
  })
  
  output$barDestino <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(datosEnRango()) == 0)
      return(NULL)
    
    temp<-datosEnRango() %>%
      select(contains(input$color))
    
    names(temp)<-'var'
    
    temp<-temp
    resultado<-temp %>%
      group_by(var) %>%
      summarise(envios=n()) %>%
      arrange(-envios) %>%
      as.data.frame
    
    resultado[,1]<-factor(resultado[,1],levels=resultado[,1])
      
      
    
    
    ggplot(resultado[1:10,],aes(x=var,y=envios,fill=var))+
      geom_bar(stat = "identity")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")+
      xlab("Monto")+ylab('Conteo')
  })
  
  
  output$serie <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(datosEnRango()) == 0)
      return(NULL)
    
    temp<-datosEnRango()
    
    
    temp<-temp %>%
      select(contains('envioMes'),contains(input$varSerie))
    
    names(temp)[2]<-'var'
    if(input$varSerie!='montoP'){
      resultado<-temp %>%
        group_by(envioMes) %>%
        summarise(var=sum(var))
    }
    if(input$varSerie=='montoP'){
      resultado<-temp %>%
        group_by(envioMes) %>%
        summarise(var=mean(var))
    }
      
    
    if(input$varSerie=='monto')resultado$var<-resultado$var/1000000
    
    
    ggplot(resultado,aes(x=envioMes,y=var))+
      geom_smooth()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")+
      xlab("mes")+ylab('total')
  
  })
  
  
  
##########################################################
#Banco mundial--------------------------------------------
##########################################################
  
  ############################################################    
  variables1 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[1:2])
  })
  output$plot1 <- renderPlot({
    variables1()
  })
  ############################################################
  variables2 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[3:4])
  })
  output$plot2 <- renderPlot({
    variables2()
  })
  ############################################################
  variables3 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[5:6])
  })
  output$plot3 <- renderPlot({
    variables3()
  })
  ############################################################
  variables4 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[7:8])
  })
  output$plot4 <- renderPlot({
    variables4()
  })
  ############################################################
  variables5 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[9:10])
  })
  output$plot5 <- renderPlot({
    variables5()
  })
  ############################################################
  variables6 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[11:12])
  })
  output$plot6 <- renderPlot({
    variables6()
  })
  ############################################################
  variables7 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[13:14])
  })
  output$plot7 <- renderPlot({
    variables7()
  })
  ############################################################
  variables8 <- reactive({
    cruces_descriptivo(datosBanco ,variables =    input$variables[15:16])
  })
  output$plot8 <- renderPlot({
    variables8()
  })
  
  
  
})
