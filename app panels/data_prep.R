###############################################################################
# Data Preperation Panel's Module Code
###############################################################################
PANEL.NAMESPACE <- "data_prep"
MINUTE_IN_SECONDS <- 60

###############################################################################
# Panel's UI Modules
###############################################################################
floor_map_ui <- function(namespace, floor.id) {
  ### Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  tagList(
    plotOutput(ns(floor.id))
  )
}

data_prep_settings_ui <- function(namespace) {
  ### Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  # Your component UI code here, currently using the file import module code
  # from the tutorial
  tagList(
    h2("Data Parameters"),
    p("Generates number of visitor counted in each area of interest based on the time interval set, for all 3 days."),
    uiOutput(ns("area_interest_checkboxes")),
    selectInput(ns("time_interval"), "Time Interval", c(
      "1 minute" = MINUTE_IN_SECONDS,
      "5 minutes" = 5*MINUTE_IN_SECONDS,
      "10 minutes" = 10*MINUTE_IN_SECONDS,
      "15 minutes" = 15*MINUTE_IN_SECONDS,
      "30 minutes" = 30*MINUTE_IN_SECONDS,
      "1 hour" = 60*MINUTE_IN_SECONDS
      )
    ),
    shinyTree(ns("tree"), checkbox = TRUE),
    actionButton(ns("generate_data"), "Generate Data")
  )
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
data_prep_panel <- tabPanel(
  title = "Data Preparation",
  fluidRow(
    h2("Floor Map")
  ),
  fluidRow(
    column(6, floor_map_ui(PANEL.NAMESPACE, "floor1")),
    column(6, floor_map_ui(PANEL.NAMESPACE, "floor2"))
  ),
  fluidRow(
    column(12, data_prep_settings_ui(PANEL.NAMESPACE))
    #column(10, h2("Data Table"), dataTableOutput("table"))#,
    #column(7, h2("Graphical Plot"))
  )
)

# call this function to add the tab panel
ADD_PANEL(data_prep_panel)

###############################################################################
# Panel's Server Modules
###############################################################################
floor_map <- function(input, output, session) {
  ns <- session$ns
  
  sensor_location <- reactive({
    return(SENSORS)
  })
  
  output$floor1 <- renderPlot({
    sensor_location() %>%
      filter(floor == 1) %>%
      ggplot(aes(x = px, y = py)) +
      annotation_custom(FLOOR1,
                        xmin=-0.5, xmax=29.5, ymin=-0.5, ymax=15.5) + # Shiny can handle the loading of background images
      geom_tile(aes(fill = area), alpha = 0.5) +
      scale_x_continuous(breaks = seq(0, 30, 1)) +
      scale_y_continuous(breaks = seq(0, 20, 1))
  })
  
  output$floor2 <- renderPlot({
    sensor_location() %>%
      filter(floor == 2) %>%
      ggplot(aes(x = px, y = py)) +
      annotation_custom(FLOOR2,
                        xmin=-0.5, xmax=29.5, ymin=-0.5, ymax=15.5) + # Shiny can handle the loading of background images
      geom_tile(aes(fill = area), alpha = 0.5) +
      scale_x_continuous(breaks = seq(0, 30, 1)) +
      scale_y_continuous(breaks = seq(0, 20, 1)) +
      coord_cartesian(xlim = c(0,30))
  })
}

data_prep_settings <- function(input, output, session) {
  ns <- session$ns
  
  output$area_interest_checkboxes <- renderUI({
    area.names <- ZONES$area %>% unique() %>% sort()
    div(
      checkboxGroupInput(ns("area_interest"), "Areas of Interest",
                         choices = area.names, inline = T, selected = INITIAL_AREAS)
    )
  })
  
  output$tree <- renderTree({ 
    list(  'I lorem impsum'= list( 
      'I.1 lorem impsum'   =  structure(list('I.1.1 lorem impsum'='1', 'I.1.2 lorem impsum'='2'),stselected=TRUE),  
      'I.2 lorem impsum'   =  structure(list('I.2.1 lorem impsum'='3'), stselected=TRUE))) 
  })
  
  observeEvent(input$generate_data, {
    print("Start Generation")
    
    areas.selected <- input$area_interest
    time.interval <- as.integer(input$time_interval)
    
    # Test
    print("Generating Day 1 Data")
    create_daily_data(DAY1, areas.selected, time.interval, "day1")
    print("Generating Day 2 Data")
    create_daily_data(DAY2, areas.selected, time.interval, "day2")
    print("Generating Day 3 Data")
    create_daily_data(DAY3, areas.selected, time.interval, "day3")
    
    print("End Generation")
    
    #TODO: create a done notification
  })
}

###############################################################################
# Add the tab panel's server code here
###############################################################################
data_prep_server <- substitute({
  # Server code below
  callModule(floor_map, PANEL.NAMESPACE)
  callModule(data_prep_settings, PANEL.NAMESPACE)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(data_prep_server)
