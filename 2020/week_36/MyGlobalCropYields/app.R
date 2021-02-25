library(shiny)
library(tidyverse)
library(fullPage)
library(echarts4r)


##------------------------------------------------------------------------------
## SERVER
app_server <- function( input, output, session ) {
    callModule(my_mod_trend_server, "trend")
    callModule(my_mod_map_server, "map")
    callModule(my_mod_comp_server, "comp")
}


##------------------------------------------------------------------------------
## UI
app_ui <- function(request) {
    
    tagList(
        tags$head(includeCSS("www/css.css")),
        navbarPage("Global Crop Yields",
                   tabPanel("Temporal Trends",
                            my_mod_trend_ui("trend")),
                   tabPanel("Spatiotemporal Trends",
                            my_mod_map_ui("map")),
                   tabPanel("Comparison Yields 2018",
                            my_mod_comp_ui("comp"))
                   )
    )
}

##------------------------------------------------------------------------------
## RUN APP
shinyApp(app_ui, app_server)