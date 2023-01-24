
# rm(list=ls())
#setwd("e:/CDC_MINSA_PNUMA/Shiny_app/Disease_MINSA/")
library(rsconnect)
library(recipes)
library(shiny)
library(leaflet)
library(shinythemes)
library(sf)
library(htmltools)
library(tidyverse)


#Path shape file SECTORS

file_shape <- rgdal::readOGR("./SECTORES/SECTORES.shp")
# MERGE DF with SPATIALDF
#shp_fusion <- sp::merge(file_shape, df, by.x = "SECTOR", by.y = "sec")
umbral <- read.table("UMBRAL_v2.txt", header = T,sep = "\t")

ui <- bootstrapPage(navbarPage(title = tags$a(href ="/",
                                              img(class = "logo", src = "LOGO_CDC.png",#)), 
                                                  style ="margin-top: -8px; padding-right:5px",#;
                                                  #     padding-bottom: 10px", 
                                                  height = "53px", width="575px")),
                               windowTitle = tags$b("DASHBOARD CDC DENGUE"),
                               theme = shinytheme("flatly"),
                               tabPanel(title = tags$h5(tags$b("DENGUE")),
                                        actionButton(inputId = "PISCO", label = "PISCO - DATASET"),
                                        actionButton(inputId = "ERA", label = "ERA - DATASET"),
                                        br(),
                                        fluidPage(br(),
                                                  fluidRow(
                                                    column(width = 3, #"TEXTO COMPRENSION",
                                                           wellPanel(fluidRow(includeMarkdown("r_text.md")),
                                                                     style = "height:300px; background-color: #FDF5E6;"),
                                                           wellPanel(fluidRow(HTML(paste("<p style='font-family: arial, helvetica, sans-serif;
                                                                                         text-align: center; font-size: 12px; 
                                                                                         font-style: normal; line-height: normal; 
                                                                                         color: #e60328;'><b>DESCRIPCION DE UMBRALES POR SECTOR CLIMÁTICO ...</b></p>")),
                                                                              htmlOutput("text")),
                                                                     style = "height:500px; background-color: #FDF5E6;")),
                                                    column(width = 9, #"MAPA INTERACTIVO",
                                                           leafletOutput(outputId = "mymap", height = "800px")))
                                        )),
                               tabPanel(title = tags$h5(tags$b("DENGUE2")),
                                        actionButton(inputId = "PISCO", label = "PISCO - DATASET"),
                                        actionButton(inputId = "ERA", label = "ERA - DATASET")),
                               tabPanel(title = tags$h5(tags$b("DENGUE3")),
                                        actionButton(inputId = "PISCO", label = "PISCO - DATASET"),
                                        actionButton(inputId = "ERA", label = "ERA - DATASET")),
                               tabPanel(title = tags$h5(tags$b("DENGUE4")),
                                        actionButton(inputId = "PISCO", label = "PISCO - DATASET"),
                                        actionButton(inputId = "ERA", label = "ERA - DATASET"))
)

)

server <- function(input, output){
  
  #--- Palete Color Map
  pal <- str_sort(unique(file_shape$SECTOR))
  sec_col <- c("#ff9900","#ffcc00","#ffcc66",
               "#001a00","#003300","#006600","#009900","#00cc00","#00ff00",
               "#1a0d00","#4d2600","#804000","#b35900","#cc6600","#ff8000")
  factpal <- colorFactor(palette = sec_col, domain = pal)
  #--- DisplayMap and Clicking on it to get Dynamic TEXT
  ##--- Use reactive values to store the id from observing the shape click
  rv <- reactiveVal()
  
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom = 5, dragging = TRUE)) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng = -80.14, lat = -9.45, zoom = 5.5) %>%
      setMaxBounds(lng1 = -80.14 + .05, 
                   lat1 = -9.45 + .05, 
                   lng2 = -80.14 - .05, 
                   lat2 = -9.45 - .05) %>%
      addPolygons(data = file_shape,
                  fillColor = ~factpal(pal),
                  weight = 0.5,
                  color = "white",
                  fillOpacity = 0.4,
                  popup = ~paste(SECTOR),
                  highlight = highlightOptions(weight = 2, fillOpacity = 0.7,
                                               bringToFront = T, color = "red"),
                  layerId = ~SECTOR) %>%
      addLegend(position = "bottomleft",
                title = "SECTORES CLIMATICOS",
                pal = factpal,
                values = pal
      )
    
  })
  #--- Click evento to display text
  observeEvent(input$mymap_shape_click, {
    rv(input$mymap_shape_click$id)
  })
  #--- Now you can add the message on the ID region selected
  observeEvent(input$PISCO, {
    output$text <- renderUI({
      str1 <- umbral[umbral$ID %in% rv(),]$PISCO_MESS
      HTML(paste(str1, sep = '<br/>'))
    })
  })
  
  observeEvent(input$ERA, {
    output$text <- renderUI({
      str1 <- umbral[umbral$ID %in% rv(),]$ERA_MESS
      HTML(paste(str1, sep = '<br/>'))
    })
  })
  
  
  
}

shinyApp(ui, server)
