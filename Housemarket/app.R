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
source("m - simple model.R")
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
                               menuItem("Variable Selection", tabName = "var"),
                               menuItem("Input House Characteristics", tabName = "model"))),

  
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
              fluidRow(uiOutput("selection_data")),
              fluidRow(withSpinner(dataTableOutput("table_model"), type = 8))
             
             ),
      tabItem(tabName = "model",
             
              fluidRow( column(uiOutput("variable_selection"), width = 6),
                       column(
                actionButton('run_model', 'Train Model'), 
                width = 6),
                verbatimTextOutput("summary"))
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
  
  
  inVars <- reactive({
    unique(input$names_variable)
  })

  output$variable_selection <- renderUI({
    pvars <- inVars()
      
      div(
        lapply(pvars, function(i) {
            if(class(points()[[i]]) %like%  'numeric|integer'){
              numericInput(inputId = i,label = paste('Input ', i),
                           value = 1)}else if(
                class(points()[[i]]) == 'character'){
                selectInput(inputId = i, label = paste('Input ', i),
                            choices = points()[[i]] %>% unique()
                            )
            }
        })
      )

  })
  
  output$table_model <- renderDataTable({
    #print(inVars())
    if(is.null(inVars())){
      print('No Variable Selection')
    }else{
      points()[, .SD, .SDcols = inVars()]  
    }
    
  })
  
  
  ## Running model
  model_results <-  eventReactive(input$run_model, {
    print('ciao')
    print(inVars())
    lm_results <- simple_model(points(), inVars(), 'SalePrice')
    summary(lm_results)
    return(lm_results)
  })

  

  output$summary <- renderPrint({
    summary(model_results())
  })

  

  
}

shinyApp(ui, server)

