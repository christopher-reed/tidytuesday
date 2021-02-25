my_mod_trend_ui <- function(id){
  ns <- NS(id)
  
  fullPage::pageContainer(
    pageContainer(
      h3("Crop Yields over Time"),
      h4("(in tonnes per hectare)"),
      br(),
      br(),
      fluidRow(
        column(8, uiOutput(ns("country_select_generated"))),
        column(4, uiOutput(ns("crop_select_generated")))
      ),
      shinycssloaders::withSpinner(type = 5, color = 'black', echarts4r::echarts4rOutput(ns("trend"), height = "50vh")),
      br(), br(), br(),
      p("Inspiration: ",
        tags$a(href = "https://www.cedricscherer.com/", "Cédric Scherer"),
        "  •  Shiny App: ",
        tags$a(href="https://www.linkedin.com/in/christopher--reed/", "Christopher Reed"),
        "  •  Data Source: ",
        tags$a(href="https://ourworldindata.org/crop-yields", "Our World in Data"))
    )
  )
      
}

my_mod_trend_server <- function(input, output, session) {
  
  ## MANAGE NAMESPACE
  #  Now we are referring to the output id names defined in the above UI
  ns <- session$ns 
  
  ## DATA
  #  Basic cleaning to go from wide to long data as well as other tidying
  crops <- 
    readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv') %>% 
    tidyr::pivot_longer(
      cols = c(-Entity, -Year, -Code),
      names_to = "Crop",
      values_to = "Yield"
    ) %>% 
    dplyr::mutate(
      Year = as.character(Year),
      Crop = stringr::str_remove(Crop, "\\s\\(tonnes per hectare\\)"),
      Yield = format(round(Yield, 2), nsmall = 2)
    )
  
  ## SELECT CROP
  #  renderUI
  output$crop_select_generated <- renderUI({
    
    # Crops to pick from
    cnsA <- crops %>%
      dplyr::distinct(Crop) %>%
      dplyr::pull(Crop)
    
    # UI component
    selectizeInput(
      ns("crop_select"),
      "Choose a crop",
      choices = cnsA,
      selected = c("Cocoa beans")
    )
  })
  
  ## SELECT COUNTRY
  #  render UI
  output$country_select_generated <- renderUI({
    
    # Countries to pick from
    cnsB <- crops %>%
      dplyr::distinct(Entity) %>%
      dplyr::pull(Entity)
    
    # UI component
    selectizeInput(
      ns("country_select"),
      "Search a country, region or continent",
      choices = cnsB,
      selected = c("Africa", "South America", "Asia"),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  ## CHART TREND
  #  Use inputs to filter data and plot interactive chart
  output$trend <- echarts4r::renderEcharts4r({
    req(input$country_select)
    
    echarts4r::e_common(
      font_family = "Overpass",
      theme = NULL
    )
    
    # requires a country selected
    validate(
      need(length(input$country_select) > 0, message = "Select at least one region")
    )
    # requires a crop selected
    validate(
      need(length(input$crop_select) > 0, message = "Select a crop")
    )
    
    # filter selected and match with color
    dat <- crops %>% 
      dplyr::filter(
        Entity %in% input$country_select,
        Crop %in% input$crop_select
      ) %>%
      tidyr::pivot_wider(
        id_cols = c(Entity, Code, Year),
        names_from = Crop,
        values_from = Yield
      )
    
    ## plot
    dat %>% 
      dplyr::group_by(Entity) %>% 
      echarts4r::e_charts(Year) %>% 
      echarts4r::e_line_(input$crop_select) %>% 
      echarts4r::e_tooltip(trigger = "axis") %>% 
      echarts4r::e_mark_point(input$country_select, data = list(type = "max")) %>% 
      echarts4r::e_legend(type = "scroll") %>% 
      echarts4r::e_toolbox(bottom = 0) %>% 
      echarts4r::e_toolbox_feature(feature = "dataZoom") %>% 
      echarts4r::e_toolbox_feature(feature = "dataView") 
  })
}