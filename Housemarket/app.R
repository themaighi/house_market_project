#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(RSelenium)
library(httr)
library(ggmap)
library(rvest)
#library(plyr)
library(XML)
library(shinycssloaders)
library(htmltools)
library(h2o)
library(curl)
# Function ----------------------------------------------------------------


trim <- function (x) {a <- gsub("^\\s+|\\s+$", "", x)
b <- gsub("\\s+", " ", a)
return(b)}

getColor <- function(x) {
  sapply(x$residual, function(mag) {
    if(mag < 0) {
      "green"
    } else if(mag == 0) {
      "orange"
    } else {
      "red"
    } })
}


source("x - widget.R")
source("m - modelling.R")
#source("f - webscrap.R")


## Get the directory
nl <- getwd()

# User interface ----------------------------------------------------------


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  dashboardHeader(title = "Home Price Project"),
  dashboardSidebar(sidebarMenu(menuItem("Table", tabName = "table", icon = icon("dashboard")),
                               menuItem("Maps", tabName = "maps", icon = icon("th")),
                               menuItem("Variable Selection", tabName = "var"))),

  
  dashboardBody(
    tabItems(
     
      tabItem(tabName = "table",
                     fluidRow(
                   
                       box(
                         selectInput(
                           "import", "Select which type of data source:",
                           c(Webscrap = "Webscrap",
                             UploadData = "File upload"),selected = c('UploadData')),
                       wellPanel(
                       conditionalPanel(condition = "input.import == 'Webscrap'",
                                        
                       textInput("city", label = "Which city", value = "rotterdam"),
                       numericInput(inputId = "day", label = "How many days to model", value = 14, min = 1, max = 100),
                       actionButton("recalc", "New points")),
                       conditionalPanel(condition = "input.import == 'File upload'",
                         fileInput("upload", label = "Upload your file here"),
                         actionButton("use_data", "Submit the data")),
                 
                       actionButton("map_show", "Show points on the map")
                       ))),
                     fluidRow(
                       withSpinner(dataTableOutput("table"), type = 8)
                     )),
      tabItem(tabName = "var",
              fluidRow(uiOutput("selection_data"),
              actionButton("run_model", "Run Model with selected variable")),
              fluidRow(withSpinner(dataTableOutput("table_model"), type = 8))
             
             ),
      

             tabItem(tabName = "maps",
                     leafletOutput(outputId = "mymap")))
 
  
))


# Server ------------------------------------------------------------------


server <- function(input, output, session) {

     points <- eventReactive(input$recalc, {
    
    #funda_webscrap(input$city, input$day)
    
  })
     points <- eventReactive(input$use_data, {
       file <- input$upload
       fread(file$datapath)
     })
  
  
  output$mymap <- renderLeaflet({ #This creates a maps of the city where the several points are marked
    
    print_data <- map_data(points())
    
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(print_data)
    )

    leaflet(print_data) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addAwesomeMarkers(~lon, ~lat,
                        icon = icons,
                        popup = ~htmlEscape(`SalePrice`))
  })
  
  # output$map <- renderUI(box(withSp
  # inner(leafletOutput("mymap"), type = 8), width = 15))
  
  output$table <- renderDataTable(points())
  output$selection_data <- renderUI({
    names_variable <- names(points())
    selectizeInput("names_variable", "Choose variable", names_variable,
                   multiple = T, selected = names_variable[1])
  })
  
  
  
  output$table_model <- renderDataTable({
    print(input$names_variable)
    points()[, .SD, .SDcols = input$names_variable]})
}

shinyApp(ui, server)

