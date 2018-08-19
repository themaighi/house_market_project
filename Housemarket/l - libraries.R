list.of.packages <- c(
  "shiny",
  "shinydashboard",
  "data.table",
  "leaflet",
  "RSelenium",
  "httr",
  "ggmap",
  "rvest",
  #"plyr",
  "XML",
  "shinycssloaders",
  "htmltools",
  "h2o",
  "curl")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages[(list.of.packages %in% installed.packages()[,"Package"])],library, c = TRUE)

