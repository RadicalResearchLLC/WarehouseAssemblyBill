## This is an alpha version of an analysis dashboard for AB 1000 and AB 1748
## It includes summary statistics, a warehouse map, and buffers representing setbacks
## proposed by AB 1000 and AB 1748
## Authored by Mike McCarthy, Radical Research LLC
## First created April 2023

library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(tidyverse)
library(markdown)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Inland Empire Warehouse Assembly Bill Comparison"),
    tabsetPanel(
      tabPanel('Dashboard',
        fluidRow(
          column(3, align = 'center', 
            numericInput(
              inputId = 'FAR', 
              label = 'Select an average Floor Area Ratio for Warehouses', 
              value = 0.55,
              min = 0.4, 
              max = 0.65, 
              step = 0.05, 
              width = '200px')
          ),
          column(width = 9, 
          align = 'center', 
            dataTableOutput('Summary'),
            leafletOutput("map", height = 500),
            dataTableOutput('SchoolList')
          )
        )
     ),
      tabPanel('Readme',
               div(style = 'width: 90%; margin: auto;',
                   fluidRow(includeMarkdown("README.md")),
               )
      )
    )
)
       
# Define server logic required to draw a histogram
server <- function(input, output) {

  

  
  output$map <- renderLeaflet({
    map1 <- leaflet() %>%
      addTiles() %>% 
      setView(lat = 33.85, lng = -117.20, zoom = 10) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
      addProviderTiles(providers$CartoDB.Positron, group = 'Basemap') %>% 
      addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                       overlayGroups = c('Warehouses', 'AB 1000', 
                                         'AB 1748', 'Schools near warehouses'),
                       options = layersControlOptions(collapsed = FALSE),
                       position = 'topright'
      ) %>% 
      hideGroup(c('AB 1000', 'AB 1748', 'Schools near warehouses')) %>%
      addLegend(data = WH_100k,
                pal = pal100,
                title = 'Category',
                values = ~category) %>% 
      addLegend(data = narrow_schools,
                pal = palSchool,
                title = 'School Proximity',
                values = ~legis)
  })

pal100 <- colorFactor(palette = c('red', 'maroon'),
                      domain = WH_100k$category)
      
    observe({
      leafletProxy("map", data = HundredK()) %>%
        clearGroup(group = 'Warehouses') %>%
        addPolygons(data = HundredK(),
                    color = ~pal100(category), #'#A94915',
                    weight = 1,
                    fillOpacity = 0.5,
                    group = 'Warehouses',
                    label = ~htmlEscape(paste(apn, category, 
                                              round(shape_area, -3), ' sq.ft.'))) #%>% 

    })

    # AB 1000 Buffer
    observe({
      leafletProxy("map", data = AB1000_B()) %>%
        clearGroup(group = 'AB 1000') %>%
        addPolygons(data = AB1000_B(),
                    color = 'red',
                    stroke = FALSE,
                    fillOpacity = 0.2,
                    group = 'AB 1000'
        )
    })
  
    # AB 1748 Buffer
    observe({
      leafletProxy("map", data = AB1748_B()) %>%
        clearGroup(group = 'AB 1748') %>%
        addPolygons(data = AB1748_B(),
                    color = 'grey20',
                    weight = 1,
                    fillOpacity = 0.2,
                    group = 'AB 1748'
        )
    })
    # Schools near Warehouses
    palSchool <-colorFactor(palette = c('blue', 'darkblue'), domain = narrow_schools$legis)
    
    observe({
      leafletProxy("map", data = narrow_schools) %>% 
        clearGroup(group = 'Schools near warehouses') %>% 
        addPolygons(data = narrow_schools,
                    color = ~palSchool(legis),
                    weight = 1,
                    fillOpacity = 0.5,
                    group = 'Schools near warehouses',
                    label = ~School)
    })
    
  
    
SumStats <- reactive({
  req(input$FAR)
  
  tempTbl <- wh_nogeom_all %>% 
    mutate(floorSpace.sq.ft = round(shape_area * as.numeric(input$FAR), - 3)) %>% 
    mutate(sizeBin = case_when(
      floorSpace.sq.ft < 100000 ~ 'Less than 100,000 sq.ft.',
      floorSpace.sq.ft >= 400000 ~ 'More than 400,000 sq.ft.',
      TRUE ~ 'Between 100,000 and 400,000 sq.ft.'
    )) %>% 
    group_by(sizeBin, category) %>% 
    summarize(count = n(), totalBldgArea = round(sum(floorSpace.sq.ft), -6),
              .groups = 'drop') %>% 
    arrange(category) %>% 
    rename('Size Category' = sizeBin,
           'Total Warehouse Area in sq.ft.' = totalBldgArea) #%>%
  
  return(tempTbl)
})

output$SchoolList <- DT::renderDataTable({
  DT::datatable(
    narrow_schools %>% 
      st_set_geometry(value = NULL) %>% 
      select(School, District, GradesServed, AB1000, AB1748),
                caption  = 'Schools within 1,000 feet of warehouses',
                rownames = FALSE, 
                options = list(dom = 'Btp'),
                filter = list(position = 'top', clear = FALSE)
  )},   server = FALSE
)

HundredK <- reactive({
  hundred <- WH_100k %>% 
    filter(shape_area >= 100000/as.numeric(input$FAR))
  return(hundred)
})

fourHundredK <- reactive({
  fourHundred <- WH_100k %>% 
    filter(shape_area >= 400000/as.numeric(input$FAR))
})

AB1000_B <- reactive({
  buffer1 <- AB1000_buffer %>% 
    filter(shape_area >= 100000/as.numeric(input$FAR))
})
AB1748_B <- reactive({ 
  buffer2 <- AB1748_buffer %>% 
  filter(shape_area >= 400000/as.numeric(input$FAR))
})

## Generate a data table of summary stats
output$Summary <- DT::renderDataTable({
  
  DT::datatable(SumStats(),
    caption  = 'Inland Empire Warehouse summary statistics by size bin',
    rownames = FALSE, 
    options = list(dom = 'Btp')
  )},   server = FALSE
)
    
}
# Run the application 
shinyApp(ui = ui, server = server)
