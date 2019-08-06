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
    title = "Floor Choropleth Map", width = 12, id = ns("floor_box"),
    tagList(
      div(style="display:inline;",
        actionButton(ns("toggle_box"), label = "Show/Hide"),
        checkboxInput(ns("group_area"), label = "Show as Area")
      ),
      column(6, plotOutput(ns("floor1"), height = "auto")),
      column(6, plotOutput(ns("floor2"), height = "auto"))
    )
  )
}

ridgelines_ui <- function(namespace) {
  ns <- NS(namespace)
  
  tagList(
    plotOutput(ns("ridgeline_plot"))
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
      selectInput("crowd_day_select", label = "Day Select",
                  choices = c("Day 1" = 1, "Day 2" = 2, "Day 3" = 3)),
      uiOutput("crowd_time_select")
    )
  ),
  fluidRow(
    box(title = "Area Control", width = 2, uiOutput("crowd_areas_select")),
    box(title = "Ridgeline Plot", width = 10, ridgelines_ui(PANEL.NAMESPACE))
  )
  #ridgelines_ui(PANEL.NAMESPACE)
)

ADD_PANEL(crowd_panel, panel.label = "Crowd Analysis", panel.name = tab.name)

###############################################################################
# Panel's Server Modules
###############################################################################
choropleth <- function (input, output, session, parent_rv) {
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
  
  area_dataframe <- reactive({
    prv <- parent_rv()
    DAYS_AREA[[prv$day_selected]] %>%
      filter(start_time == prv$time_selected) %>%
      select(floor, px, py, visitor_count) %>%
      return()
  })
  
  observeEvent(input$toggle_box, {
    rv$show = !rv$show
  })
  
  output$floor1 <- renderPlot({
    prv <- parent_rv()
    
    sensor_location() %>%
      filter(floor == 1) %>%
      left_join(area_dataframe(), by = c("floor", "px", "py")) %>%
      mutate(visitor_count = ifelse(is.na(visitor_count), 0, visitor_count)) %>%
      ggplot(aes(x = px, y = py)) +
      annotation_custom(FLOOR1,
                        xmin=-0.5, xmax=29.5, ymin=-0.5, ymax=15.5) + # Shiny can handle the loading of background images
      geom_tile(aes(fill = log10(visitor_count+1)), alpha = 0.5) +
      scale_x_continuous(breaks = seq(0, 30, 1)) +
      scale_y_continuous(breaks = seq(0, 20, 1))
  }, height = function() plot_height())
  
  output$floor2 <- renderPlot({
    prv <- parent_rv()
    
    sensor_location() %>%
      filter(floor == 2) %>%
      left_join(area_dataframe(), by = c("floor", "px", "py")) %>%
      mutate(visitor_count = ifelse(is.na(visitor_count), 0, visitor_count)) %>%
      ggplot(aes(x = px, y = py)) +
      annotation_custom(FLOOR2,
                        xmin=-0.5, xmax=29.5, ymin=-0.5, ymax=15.5) + # Shiny can handle the loading of background images
      geom_tile(aes(fill = log10(visitor_count+1)), alpha = 0.5) +
      scale_x_continuous(breaks = seq(0, 30, 1)) +
      scale_y_continuous(breaks = seq(0, 20, 1)) +
      coord_cartesian(xlim = c(0,30))
  }, height = function() plot_height())
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
  
  observeEvent(input$crowd_time_select, {
    rv$time_selected <- as.integer(input$crowd_time_select)
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
      sliderInput("crowd_time_select", "Time of Day", 
                  min = slider_settings[1], max = slider_settings[2],
                  step = slider_settings[3], value = slider_settings[1])
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
