###############################################################################
# Data Preperation Panel's Module Code
###############################################################################
PANEL.NAMESPACE <- "data_prep"
MINUTE_IN_SECONDS <- 60

###############################################################################
# Panel's UI Modules
###############################################################################
floor_map_ui <- function(namespace) {
  ### Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  box(
    title = "Floor Map", width = 12, id = ns("floor_box"), collapsible = T,
    tagList(
      div(actionButton(ns("toggle_box"), label = "Show/Hide")),
      column(6, plotOutput(ns("floor1"), height = "auto")),
      column(6, plotOutput(ns("floor2"), height = "auto"))
    )
  )
}

data_prep_settings_ui <- function(namespace) {
  ### Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  # Your component UI code here, currently using the file import module code
  # from the tutorial
  div(
    #h2("Data Parameters"),
    p("Generates number of visitor counted in each area of interest based on the time interval set, for all 3 days."),
    uiOutput(ns("area_interest_checkboxes")),
    selectInput(ns("time_interval"), "Time Interval", width = 200, c(
      "1 minute" = MINUTE_IN_SECONDS,
      "5 minutes" = 5*MINUTE_IN_SECONDS,
      "10 minutes" = 10*MINUTE_IN_SECONDS,
      "15 minutes" = 15*MINUTE_IN_SECONDS,
      "30 minutes" = 30*MINUTE_IN_SECONDS,
      "1 hour" = 60*MINUTE_IN_SECONDS
      )
    ),
    #shinyTree(ns("tree"), checkbox = TRUE),
    actionButton(ns("generate_data"), "Generate Data"),
    actionButton(ns("reload_data"), "Reload Data")
  )
}

data_table_3days_ui <- function(namespace, title) {
  ns <- NS(namespace)
  
  tagList(
    div(actionButton(ns("toggle_data_box"), label = paste("Show/Hide", title))),
    div(id = ns("data_table_box"),
      tabBox(
        title = title, width = 12,
        tabPanel(title = "Day 1", dataTableOutput(ns("day1_df"))),
        tabPanel(title = "Day 2", dataTableOutput(ns("day2_df"))),
        tabPanel(title = "Day 3", dataTableOutput(ns("day3_df")))
      )
    ),
    br()
  )
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
tab.name <- "data"
data_prep_panel <- tabItem(
  tabName = tab.name,
  fluidRow(
    box(width = 12, h2("Data Preparation"))
  ),
  fluidRow(
    floor_map_ui(PANEL.NAMESPACE)
  ),
  fluidRow(
    box(
      title = "Data Settings", width = 12,
      data_prep_settings_ui(PANEL.NAMESPACE)
    )
  ),
  fluidRow(
    data_table_3days_ui(PANEL.NAMESPACE, "Visitor Data"),
    data_table_3days_ui(paste(PANEL.NAMESPACE, "SIMPLIFIED", sep = "_"), "Simplified Visitors Data"),
    data_table_3days_ui(paste(PANEL.NAMESPACE, "AREA", sep = "_"), "Area Crowd Data"),
    data_table_3days_ui(paste(PANEL.NAMESPACE, "MOVEMENT", sep = "_"), "Movement Data"),
    data_table_3days_ui(paste(PANEL.NAMESPACE, "CONNECTION", sep = "_"), "Visitors' Connection Data")
  )
)

# call this function to add the tab panel
ADD_PANEL(data_prep_panel, panel.label = "Data Preparation", panel.name = tab.name)

###############################################################################
# Panel's Server Modules
###############################################################################
floor_map <- function(input, output, session) {
  ns <- session$ns
  
  rv <- reactiveValues(show = T, plot_height = 400)
  
  sensor_location <- reactive({
    return(SENSORS)
  })
  
  plot_height <- reactive({
    if (rv$show) {
      return(400)
    } else {
      return(1)
    }
  })
  
  observeEvent(input$toggle_box, {
    print("Toggle floor_box clicked")
    rv$show = !rv$show
  })
  
  output$floor1 <- renderPlot({
    if (rv$show) {
      sensor_location() %>%
        filter(floor == 1) %>%
        ggplot(aes(x = px, y = py)) +
        annotation_custom(FLOOR1,
                          xmin=-0.5, xmax=29.5, ymin=-0.5, ymax=15.5) + # Shiny can handle the loading of background images
        geom_tile(aes(fill = area), alpha = 0.5) +
        scale_x_continuous(breaks = seq(0, 30, 1)) +
        scale_y_continuous(breaks = seq(0, 20, 1))
    }
  }, height = function() plot_height())
  
  output$floor2 <- renderPlot({
    if (rv$show) {
      sensor_location() %>%
        filter(floor == 2) %>%
        ggplot(aes(x = px, y = py)) +
        annotation_custom(FLOOR2,
                          xmin=-0.5, xmax=29.5, ymin=-0.5, ymax=15.5) + # Shiny can handle the loading of background images
        geom_tile(aes(fill = area), alpha = 0.5) +
        scale_x_continuous(breaks = seq(0, 30, 1)) +
        scale_y_continuous(breaks = seq(0, 20, 1)) +
        coord_cartesian(xlim = c(0,30))
    }
  }, height = function() plot_height())
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
  
  #output$tree <- renderTree({ 
  #  list(  'I lorem impsum'= list( 
  #    'I.1 lorem impsum'   =  structure(list('I.1.1 lorem impsum'='1', 'I.1.2 lorem impsum'='2'),stselected=TRUE),  
  #    'I.2 lorem impsum'   =  structure(list('I.2.1 lorem impsum'='3'), stselected=TRUE))) 
  #})
  
  observeEvent(input$generate_data, {
    print("Start Generation")
    
    areas.selected <- input$area_interest
    time.interval <- as.integer(input$time_interval)
    
    # Test
    print("Generating Day 1 Data")
    create_daily_data(DAYS[[1]], areas.selected, time.interval, "day1")
    print("Generating Day 2 Data")
    create_daily_data(DAYS[[2]], areas.selected, time.interval, "day2")
    print("Generating Day 3 Data")
    create_daily_data(DAYS[[3]], areas.selected, time.interval, "day3")
    
    print("End Generation")
    
    #TODO: create a done notification
  })
  
  observeEvent(input$reload_data, {
    LOAD_DATA()
  })
}

data_table_3days <- function(input, output, session, days.data) {
  ns <- session$ns
  
  rv <- reactiveValues(show = T)
  
  observeEvent(input$toggle_data_box, {
    #print("toggle_data_box pressed")
    rv$show = !rv$show
    if (rv$show) {
      #print(paste("showing", ns("data_table_box")))
      shinyjs::show("data_table_box")
    } else {
      #print(paste("hiding", ns("data_table_box")))
      shinyjs::hide("data_table_box")
    }
  })
  
  output$day1_df <- renderDataTable({
    if (class(days.data[1]) != "logical") {
      days.data[[1]]
    }
  }, options = list(pageLength = 10))
  
  output$day2_df <- renderDataTable({
    if (class(days.data[2]) != "logical") {
      days.data[[2]]
    }
  }, options = list(pageLength = 10))
  
  output$day3_df <- renderDataTable({
    if (class(days.data[3]) != "logical") {
      days.data[[3]]
    }
  }, options = list(pageLength = 10))
}

###############################################################################
# Add the tab panel's server code here
###############################################################################
data_prep_server <- substitute({
  # Server code below
  callModule(floor_map, PANEL.NAMESPACE)
  callModule(data_prep_settings, PANEL.NAMESPACE)
  callModule(data_table_3days, PANEL.NAMESPACE, DAYS)
  callModule(data_table_3days, paste(PANEL.NAMESPACE, "SIMPLIFIED", sep = "_"), DAYS_SIMPLIFIED)
  callModule(data_table_3days, paste(PANEL.NAMESPACE, "AREA", sep = "_"), DAYS_AREA)
  callModule(data_table_3days, paste(PANEL.NAMESPACE, "MOVEMENT", sep = "_"), DAYS_MOVEMENT)
  callModule(data_table_3days, paste(PANEL.NAMESPACE, "CONNECTION", sep = "_"), DAYS_CONNECTION)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(data_prep_server)
