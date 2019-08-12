###############################################################################
# Ridgelines Panel's Module Code
###############################################################################
PANEL.NAMESPACE <- "crowd"

###############################################################################
# Panel's UI Modules
###############################################################################
choropleth_ui <- function(namespace) {
  ns <- NS(namespace)
  
  box(
    title = "Floor Choropleth Map", width = 12, id = ns("floor_box"), collapsible = T,
    tagList(
      div(style="display:inline;",
        #actionButton(ns("toggle_box"), label = "Show/Hide"),
        checkboxInput(ns("group_area"), label = "Show as Area")
      ),
      column(6, plotlyOutput(ns("floor1"), height = "auto")),
      column(6, plotlyOutput(ns("floor2"), height = "auto"))
    )
  )
}

ridgelines_ui <- function(namespace) {
  ns <- NS(namespace)
  
  box(title = "Ridgeline Plot", width = 10,
    tagList(
      plotOutput(ns("ridgeline_plot"))
    )
  )
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
tab.name <- "crowd"
crowd_panel <- tabItem(
  tabName = tab.name,
  fluidRow(
    box(width = 12, h2("Crowd Analysis"))
  ),
  
  fluidRow(
    choropleth_ui(PANEL.NAMESPACE)
  ),
  fluidRow(
    box(
      title = "Day and Time", width = 12,
      column(2, selectInput("crowd_day_select", label = "Day Select",
                            choices = c("Day 1" = 1, "Day 2" = 2, "Day 3" = 3))),
      column(10, uiOutput("crowd_time_select"))
    )
  ),
  fluidRow(
    box(title = "Area Control", width = 2, uiOutput("crowd_areas_select")),
    ridgelines_ui(PANEL.NAMESPACE)
  )
  #ridgelines_ui(PANEL.NAMESPACE)
)

ADD_PANEL(crowd_panel, panel.label = "Crowd Analysis", panel.name = tab.name)

###############################################################################
# Panel's Server Modules
###############################################################################
choropleth <- function (input, output, session, parent_rv) {
  ns <- session$ns
  
  sensor_location <- reactive({
    return(SENSORS)
  })

  area_dataframe <- reactive({
    prv <- parent_rv()
    DAYS_AREA[[prv$day_selected]] %>%
      mutate(visitor_count = log10(visitor_count + 1)) %>%
      return()
  })
  
  time_dataframe <- reactive({
    prv <- parent_rv()
    area_dataframe() %>%
      filter(start_time == prv$time_selected) %>%
      select(floor, px, py, visitor_count) %>%
      return()
  })
  
  zmax <- reactive({
     area_dataframe() %>%
      .$visitor_count %>%
      max() %>%
      return()
  })
  
  output$floor1 <- renderPlotly({
    sensor_location() %>%
      filter(floor == 1) %>%
      left_join(time_dataframe(), by = c("floor", "px", "py")) %>%
      mutate(visitor_count = ifelse(is.na(visitor_count), 0, visitor_count)) %>%
      create_floor_map_plot(1, ~visitor_count, 0, zmax())
  })
  
  output$floor2 <- renderPlotly({
    sensor_location() %>%
      filter(floor == 2) %>%
      left_join(time_dataframe(), by = c("floor", "px", "py")) %>%
      mutate(visitor_count = ifelse(is.na(visitor_count), 0, visitor_count)) %>%
      create_floor_map_plot(2, ~visitor_count, 0, zmax())
  })
}

ridgeline <- function (input, output, session, parent_rv) {
  ns <- session$ns
  
  area_dataframe <- reactive({
    prv <- parent_rv()
    return(DAYS_AREA[[prv$day_selected]])
  })
  
  output$ridgeline_plot <- renderPlot({
    prv <- parent_rv()
    create_daily_ridgeline_plot(area_dataframe(), prv$day_selected, prv$areas_selected)
  })
}

###############################################################################
# Add the tab panel's server code here
###############################################################################
crowd_server <- substitute({
  rv <- reactiveValues(
    day_selected = 1,
    time_selected = 0,
    areas_selected = c()
  )
  
  sensor_location <- reactive({
    return(SENSORS)
  })
  
  area_dataframe <- reactive({
    day.selected <- as.integer(rv$day_selected)
    return(DAYS_AREA[[day.selected]])
  })
  
  day_time_settings <- reactive({
    df <- area_dataframe()
    
    return(c(min(df$start_time),
             max(df$start_time), 
             df$end_time[1] - df$start_time[1] + 1))
  })
  
  observeEvent(input$crowd_time_select_slider, {
    rv$time_selected <- as.integer(input$crowd_time_select_slider)
    print(paste("time_selected changed:", rv$time_selected))
  })
  
  observeEvent(input$crowd_day_select, {
    rv$day_selected <- as.integer(input$crowd_day_select)
    print(paste("day_selected changed:", rv$day_selected))
  })
  
  observeEvent(input$crowd_areas_select, {
    rv$areas_selected <- input$crowd_areas_select
    print(paste("areas_selected changed:", rv$crowd_areas_select))
  })
  
  output$crowd_time_select <- renderUI({
    slider_settings = day_time_settings()
    print(slider_settings)
    tagList(
      sliderInput("crowd_time_select_slider", "Time of Day", 
                  min = slider_settings[1], max = slider_settings[2],
                  step = slider_settings[3], value = slider_settings[1],
                  animate = animationOptions(interval = 1000, loop = F))
    )
  })
  
  output$crowd_areas_select <- renderUI({
    areas.avaiable <- area_dataframe() %>% .$area %>% unique()
    checkboxGroupInput("crowd_areas_select", label = "Selected Areas",
                       choices = areas.avaiable, selected = areas.avaiable)
  })
  
  # Server code below
  callModule(choropleth, PANEL.NAMESPACE, function () return(rv))
  callModule(ridgeline, PANEL.NAMESPACE, function () return(rv))
  #callModule(ridgeline, PANEL.NAMESPACE)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(crowd_server)
