
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rsconnect)
library(recipes)
library(leaflet)
library(shiny)
library(shinythemes)
library(sf)
library(htmltools)
library(tidyverse)
library(rmapshaper)
library(data.table)






#Path shape file SECTORS
file_shape <- st_read("./SECTORES/SECTORES.shp")
file_shape2 <- rmapshaper::ms_simplify(file_shape,keep = 0.05, keep_shapes = TRUE)

#--- Palete Color Map
pal <- str_sort(unique(file_shape2$SECTOR))
sec_col <- c("#ff9900","#ffcc00","#ffcc66",
             "#001a00","#003300","#006600","#009900","#00cc00","#00ff00",
             "#1a0d00","#4d2600","#804000","#b35900","#cc6600","#ff8000")
factpal <- colorFactor(palette = sec_col, domain = pal)


# Load Diseases data in html
umbral_dengue <- fread("./DENGUE_v3.txt", header = T,sep = "\t")
umbral_malaria <- fread("./MALARIA_v2.txt", header = T,sep = "\t")


ui <- navbarPage(title = tags$a(href = "./www/",img(class = "logo", 
                                                    src = "LOGO_CDC.png", 
                                                    style = "margin-top:-5px; padding-right:15px",
                                                    height = "70px",width="605px")),
                 windowTitle = tags$b("CDC_DASH"),
                 theme = shinytheme("flatly"),
                 tabPanel(title = tags$h5(tags$b("DENGUE")), 
                          actionButton(inputId = "PISCO1", label = "PISCO-DATASET"),
                          actionButton(inputId = "ERA1", label = "ERA5-DATASET"),
                          fluidRow(column(width = 3, 
                                          wellPanel(fluidRow(includeMarkdown("r_text.md")),
                                                    style = "height:300px; background-color: #FDF5E6;"),
                                          wellPanel(fluidRow(HTML(
                                            paste("<p style='text-align: justify;'><b><span style='font-family: arial, helvetica, sans-serif;font-size: small;'>DESCRIPCION DE UMBRALES POR SECTOR CLIMATICO</span></b></p>")),
                                            htmlOutput("text")), style = "height:500px; background-color: #FDF5E6;")),
                                   column(width = 9,
                                          leafletOutput(outputId = "mymap", height = "800px")))),
                 navbarMenu(title = tags$h5(tags$b("MALARIA")),
                            tabPanel(title = tags$h6(tags$em("FALCIPARUM")),
                                     actionButton(inputId = "PISCO2", label = "PISCO-DATASET"),
                                     actionButton(inputId = "ERA2", label = "ERA5-DATASET"),
                                     fluidRow(column(width = 3,
                                                     wellPanel(fluidRow(includeMarkdown("r_text.md")),
                                                               style = "height:300px; background-color: #FDF5E6;"),
                                                     wellPanel(fluidRow(HTML(
                                                       paste("<p style='text-align: justify;'><b><span style='font-family: arial, helvetica, sans-serif;font-size: small;'>DESCRIPCION DE UMBRALES POR SECTOR CLIMATICO</span></b></p>")),
                                                       htmlOutput("text2")), style = "height:500px; background-color: #FDF5E6;")),
                                              column(width = 9,
                                                     leafletOutput(outputId = "mymap2", height = "800px")))),
                            tabPanel(title = tags$h6(tags$em("VIVAX")), 
                                     actionButton(inputId = "PISCO3", label = "PISCO-DATASET"),
                                     actionButton(inputId = "ERA3", label = "ERA5-DATASET"),
                                     fluidRow(column(width = 3,
                                                     wellPanel(fluidRow(includeMarkdown("r_text.md")),
                                                               style = "height:300px; background-color: #FDF5E6;"),
                                                     wellPanel(fluidRow(HTML(
                                                       paste("<p style='text-align: justify;'><b><span style='font-family: arial, helvetica, sans-serif;font-size: small;'>DESCRIPCION DE UMBRALES POR SECTOR CLIMATICO</span></b></p>")),
                                                       htmlOutput("text3")), style = "height:500px; background-color: #FDF5E6;")),
                                              column(width = 9,
                                                     leafletOutput(outputId = "mymap3", height = "800px")))) )
)


server <- function(input, output, session){
  
  
  ###########################################################################################
  
  
  #--- DisplayMap and Clicking on it to get Dynamic TEXT
  ##--- Use reactive values to store the id from observing the shape click
  
  
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom = 5, dragging = TRUE)) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng = -80.14, lat = -9.45, zoom = 5.5) %>%
      setMaxBounds(lng1 = -80.14 + .05, 
                   lat1 = -9.45 + .05, 
                   lng2 = -80.14 - .05, 
                   lat2 = -9.45 - .05) %>%
      addPolygons(data = file_shape2,
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
 
  
  
  # #--- Click evento to display text
  
  
  observeEvent(input$mymap_shape_click,{
    click <- input$mymap_shape_click
    selected <- umbral_dengue[ID %in% click$id,]$PISCO_MESS
    observeEvent(input$PISCO1,{
      output$text <- renderUI({
        HTML(paste(selected, sep = '<br/>'))
      })
    })
  })
  
  observeEvent(input$mymap_shape_click,{
    click <- input$mymap_shape_click
    selected <- umbral_dengue[ID %in% click$id,]$ERA_MESS
    observeEvent(input$ERA1,{
      output$text <- renderUI({
        HTML(paste(selected, sep = '<br/>'))
      })
    })
  })
  
  
###########################################################################################
  
  
  output$mymap2 <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom = 5, dragging = TRUE)) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng = -80.14, lat = -9.45, zoom = 5.5) %>%
      setMaxBounds(lng1 = -80.14 + .05, 
                   lat1 = -9.45 + .05, 
                   lng2 = -80.14 - .05, 
                   lat2 = -9.45 - .05) %>%
      addPolygons(data = file_shape2,
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
  
  output$mymap3 <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom = 5, dragging = TRUE)) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng = -80.14, lat = -9.45, zoom = 5.5) %>%
      setMaxBounds(lng1 = -80.14 + .05, 
                   lat1 = -9.45 + .05, 
                   lng2 = -80.14 - .05, 
                   lat2 = -9.45 - .05) %>%
      addPolygons(data = file_shape2,
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
  
  ######################## FILE "TXT MALARIA ###################################
  #### MALARIA FALCIPARUM
  observeEvent(input$mymap2_shape_click,{
    click <- input$mymap2_shape_click
    selected <- umbral_malaria[ID %in% click$id,]$ERA_FALCI
    observeEvent(input$ERA2,{
      output$text2 <- renderUI({
        HTML(paste(selected, sep = '<br/>'))
      })
    })
  })
  
  
  observeEvent(input$mymap2_shape_click,{
    click <- input$mymap2_shape_click
    selected <- umbral_malaria[ID %in% click$id,]$PISCO_FALCI
    observeEvent(input$PISCO2,{
      output$text2 <- renderUI({
        HTML(paste(selected, sep = '<br/>'))
      })
    })
  })
  #### MALARIA VIVAX
  observeEvent(input$mymap3_shape_click,{
    click <- input$mymap3_shape_click
    selected <- umbral_malaria[ID %in% click$id,]$ERA_VIVAX
    observeEvent(input$ERA3,{
      output$text3 <- renderUI({
        HTML(paste(selected, sep = '<br/>'))
      })
    })
  })
  
  observeEvent(input$mymap3_shape_click,{
    click <- input$mymap3_shape_click
    selected <- umbral_malaria[ID %in% click$id,]$PISCO_VIVAX
    observeEvent(input$PISCO3,{
      output$text3 <- renderUI({
        HTML(paste(selected, sep = '<br/>'))
      })
    })
  })
  
  
 
  
  #############################################################################
  
}



shinyApp(ui, server)





