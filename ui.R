library(shiny)
library(leaflet)

vars=c("Top estados"="pagoEstadoNombre",
       "Top municipios"="pagoCiudadNombre")

vars2=c("monto total (en millones de pesos)"="monto",
        "envíos totales"="dummy",
        "monto promedio de los envíos"="montoP")


shinyUI(navbarPage("Metronhomo", id="nav",
                   
                   tabPanel("Mapa Interactivo",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                           

                                              # plotOutput("histMontos", height = 150),
                                              selectInput("varSerie", "Variable", vars2),
                                              plotOutput("serie",height=200),
                                              selectInput("color", "Flujo", vars),
                                              plotOutput("barDestino",height=200)
                                              
                                ),
                                
                                tags$div(id="cite",
                                         '       Datos obtenidos de ', tags$em('Dinero Express'), ' Dex(2015).'
                                )
                            )
                   ),      
                   
                   # tabPanel("FINDEX",
                   #          fluidPage(
                   #            
                   #            # Application title.
                   #            
                   #            sidebarLayout(
                   #              sidebarPanel(
                   #                h2('Variables'),
                   #               
                   #                
                   #                checkboxGroupInput("variables", "Selecciona variables para gráficar",
                   #                                   choices = names(datosBanco), inline = TRUE),
                   #                
                   #                checkboxGroupInput("variables_filtro", "Selecciona variables para filtrar",
                   #                                   choices = names(datosBanco))
                   #              ),
                   #              mainPanel(
                   #                plotOutput("plot1"),
                   #                plotOutput("plot2"),
                   #                plotOutput("plot3"),
                   #                plotOutput("plot4"),
                   #                plotOutput("plot5"),
                   #                plotOutput("plot6"),
                   #                plotOutput("plot7"),
                   #                plotOutput("plot8")
                   #              )
                   #            )
                   #          )
                   # ),
             
                   conditionalPanel("false", icon("crosshair"))
))
