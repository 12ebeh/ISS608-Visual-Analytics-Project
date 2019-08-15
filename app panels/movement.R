###############################################################################
# Network Analysis Panel's Module Code
###############################################################################
PANEL.NAMESPACE <- "movement"

###############################################################################
# Panel's UI Modules
###############################################################################
chord_and_sunburst_ui <- function(namespace) {
  ns <- NS(namespace)
  
  fluidRow(
    box(
      title = "Sunburst Diagram", collapsible = T, width = 6,
      sunburstOutput(ns("sunburst_plot"), height = "550px")
    ),
    box(
      title = "Chord Diagram", collapsible = T, width = 6,
      chorddiagOutput(ns("chord_plot"), height = "550px")
    )
  )
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
tab.name <- "movement"
movement_panel <- tabItem(
  tabName = tab.name,
  #fluidRow(
  h2("Movement Analysis"),
  #),
  fluidRow(
    box(width = 12, title = "Movement Analysis Controls",
        column(2, selectInput("movement_day_select", label = "Select Day",
                              choices =  c("Day 1" = 1, "Day 2" = 2, "Day 3" = 3))),
        column(10, uiOutput("movement_time_control"))
    )
  ),
  chord_and_sunburst_ui(PANEL.NAMESPACE)
)

# call this function to add the tab panel
ADD_PANEL(movement_panel, panel.label = "Movement Analysis", panel.name = tab.name)

###############################################################################
# Panel's Server Modules
###############################################################################
chord_and_sunburst <- function(input, output, session, prv) {
  ns <- session$ns
  
  data_simplified <- reactive({
    prv <- prv()
    return(DAYS_SIMPLIFIED[[prv$day_selected]])
  })
  
  data_area <- reactive({
    prv <- prv()
    return(DAYS_AREA[[prv$day_selected]])
  })
  
  data_sunburst <- reactive({
    prv <- prv()
    #areas <- data_area() %>% .$area %>% unique()
    create_sunburst_data_wide(DAYS_SUNBURST[[prv$day_selected]], prv$start_time, prv$end_time) %>%
      return()
  })
  
  data_chord <- reactive({
    prv <- prv()
    areas <- data_area() %>% .$area %>% unique()
    create_chord_data(data_simplified(), areas, prv$start_time, prv$end_time) %>%
      return()
  })
  
  output$sunburst_plot <- renderSunburst({
    sunburst(data_sunburst())#, height = "500px")
  })
  
  output$chord_plot <- renderChorddiag({
    colors <- SELECT_COLORS(rev(viridis(15,1)),magma(30,1))
    chorddiag(
      data = data_chord(),
      groupnamePadding = 10,
      groupPadding = 4,
      groupnameFontsize = 13,
      groupColors=colors,
      showTicks = FALSE,
      margin=120,
      tooltipGroupConnector = "    &#x25B6;    ",
      chordedgeColor = "#B3B6B7"
    )
  })
}
###############################################################################
# Add the tab panel's server code here
###############################################################################
movment_server <- substitute({
  rv <- reactiveValues(
    day_selected = 1,
    start_time = 0,
    end_time = 86400,
    graph_measure = "Closeness"
  )
  
  data_simplified <- reactive({
    return(DAYS_SIMPLIFIED[[rv$day_selected]])
  })
  
  movememt_day_time_settings <- reactive({
    df <- data_simplified()
    return(c(min(df$time), max(df$time)))
  })
  
  observeEvent(input$movement_day_select, {
    rv$day_selected <- as.integer(input$movement_day_select)
    print(paste("day_selected changed:", rv$day_selected))
  })
  
  observeEvent(input$movement_time_range, {
    if (!is.null(input$movement_time_range[1])) {
      rv$start_time <- as.integer(input$movement_time_range[1])
    }
    if (!is.null(input$movement_time_range[2])) {
      rv$end_time <- as.integer(input$movement_time_range[2])
    }
    print(paste("start_time changed:", rv$start_time))
    print(paste("end_time changed:", rv$end_time))
  })
  
  output$movement_time_control <- renderUI({
    slider_settings = movememt_day_time_settings()
    print("slider_settings_triggered")
    print(slider_settings)
    slider_min <- as.POSIXct(slider_settings[1], origin = "1970-01-01",tz = "GMT")
    slider_max <- as.POSIXct(slider_settings[2], origin = "1970-01-01",tz = "GMT")
    
    tagList(
      sliderInput("movement_time_range", "Time Range", 
                  min = slider_min, max = slider_max, 
                  value = c(slider_min, slider_max),
                  timeFormat = "%H:%M", timezone = "UTC")
    )
  })
  
  callModule(chord_and_sunburst, PANEL.NAMESPACE, function() return(rv))
})

ADD_SERVER_LOGIC(movment_server)