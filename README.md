
# House Market Analysis Dashboard

### Author: [Your Name]

## Overview

This project provides an **R Shiny Dashboard** for analyzing house prices. It enables users to visualize housing market trends and explore data interactively. The goal is to simplify data analysis for real estate professionals, researchers, and enthusiasts.

This project is not maintained and was never finished due to Funda not allowing webscraping.

## Features

- **Webscraping** The tool allows to webscrape Funda (The webscraping is old so probably will not work)
- **Interactive Charts and Maps:** The houses that are sold are visualized in an interactive map that gets different color, red if is overvalued and green if it is undervalued. Overevaluation is addressed thorugh a model.


## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/themaighi/house_market_project.git
   ```
2. Open the project directory in RStudio.

3. Install the required R packages:
   ```R
   install.packages(c(
       "shiny", "shinydashboard", "data.table", "leaflet", 
       "RSelenium", "httr", "ggmap", "rvest", 
       "XML", "shinycssloaders", "htmltools", 
       "h2o", "curl", "randomForest"
   ))
   ```

4. Run the app:
   ```R
   shiny::runApp("Housemarket")
   ```

## Dashboard Preview

```markdown
![House Market Dashboard](HousePrice-Demo.png)
```

