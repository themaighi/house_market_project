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
  x[,colors_sel := as.character(cut(residuals, quantile(residuals), include.lowest = T,
                                    labels = c('green', 'lightgreen', 
                                               'lightred', 'red')))]
  # print(quant_values)
  return(x$colors_sel)
}


source("x - widget.R")
source("m - modelling.R")
source("m - simple model.R")
source('t - buttons.R')
#source("f - webscrap.R")


## Get the directory
nl <- getwd()

# User interface ----------------------------------------------------------


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  dashboardHeader(title = "Home Price Project"),
  dashboardSidebar(sidebarMenu(menuItem("Table", tabName = "table", icon = icon("dashboard")),
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
      tabItem(tabName = "model",
              fluidPage(
              tabsetPanel(id = 'tabBox_next_previous',
                tabPanel('Location',
                         fluidRow(
                           textInput("city2", label = "Which city", value = "rotterdam"),
                           textInput("address", label = "Which address", value = "")
                           
                         ),
                         box(width = 12,leafletOutput(outputId = "housemap"))
                         ),
                tabPanel('Variable Selection',
                         fluidRow(
                           column(8, align="center", offset = 2,
                                  uiOutput("selection_data")
                           )),
                         fluidRow(column(uiOutput("variable_selection"), width = 12))),
                tabPanel('Analyze',
                         fluidRow(column(6,selectInput('model_selection',
                                                     label = 'Select Model',
                                              choices = c('Default',
                                                          'Random Forest',
                                                          'GBM'),
                                              selected = 'Default'
                                              )),
                                              column(
                                                actionButton('run_model', 'Train Model'), 
                                                width = 4)
                                                ),
                         tags$style(type='text/css', "#run_model { width:100%; margin-top: 25px;}"),
                         fluidRow(column(12,verbatimTextOutput("summary")))),
                tabPanel('Map',leafletOutput(outputId = "mymap")),
                tags$script("
    $('body').mouseover(function() {
    list_tabs=[];
    $('#tabBox_next_previous li a').each(function(){
   list_tabs.push($(this).html())
   });
   Shiny.onInputChange('List_of_tab', list_tabs);})
  ")
              ),
              uiOutput("Next_Previous"))
      )
      
    
  ))
)


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
    
    print_data <- map_data(data_for_map())
    
    
    icons <- awesomeIcons(
      icon = 'home',
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
  
  ## From your address get the 
  
  your_house <- reactive({
    dt <- data.table('city' = input$city2, 
                     'address' = input$address)
    dt$lat <- 44.8309568
    dt$lon <- 11.620288
    return(dt)
  })
  
  output$housemap <- renderLeaflet({ #This creates a maps of the city where the several points are marked
    
    print_data <- map_data(your_house())
    
    
    icons <- awesomeIcons(
      icon = 'home',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'cadetblue'
    )
    
    leaflet(print_data) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addAwesomeMarkers(~lon, ~lat,
                        icon = icons)
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
         fluidRow(
           column(6,numericInput(inputId = i,label = paste('Input ', i),
                       value = 1)),
          column(6, p(paste0('Description ', i, ':'))))
          }else if(
                         class(points()[[i]]) == 'character'){
                         fluidRow(
                         column(6, selectInput(inputId = i, label = paste('Input ', i),
                                     choices = points()[[i]] %>% unique())),
                         column(6, p(paste0('Description ', i, ':')))
                         
                         )
                       }
      })
    )
    
  })
 

  
  
  ## Running model
  model_results <-  eventReactive(input$run_model, {
    lm_results <- simple_model(points(), inVars(), 'SalePrice')
    summary(lm_results)
    return(lm_results)
  })
  
  
  data_for_map <- reactive({
    errors_vector <- points()$SalePrice - predict(model_results())
    dt <- points()[, .(SalePrice, lat, lon)]
    dt$residuals <- errors_vector
    return(dt)
    
  })
  
  output$summary <- renderPrint({
    summary(model_results())
  })
  
  output$Next_Previous=renderUI({
    tab_list=input$List_of_tab[-length(input$List_of_tab)]
    nb_tab=length(tab_list)
    if (which(tab_list==input$tabBox_next_previous)==nb_tab)
      column(1,offset=1,Previous_Button)
    else if (which(tab_list==input$tabBox_next_previous)==1)
      column(1,offset = 10,Next_Button)
    else
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
    
  })
  
  observeEvent(input$Prev_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
               }
  )
  observeEvent(input$Next_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
               })
  
}

shinyApp(ui, server)

