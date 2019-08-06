###############################################################################
# Ridgelines Panel's Module Code
###############################################################################
PANEL.NAMESPACE <- "crowd"

###############################################################################
# Panel's UI Modules
###############################################################################
ridgelines_ui <- function(namespace) {
  ns <- NS(namespace)
  
  fluidRow(
    column(3, {
      uiOutput(ns("ridgeline_settings"))
    }),
    column(9, {
      plotOutput(ns("ridgeline_plot"))
    })
  )
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
tab.name <- "crowd"
crowd_panel <- tabItem(
  tabName = tab.name,
  fluidRow(
    h2("Ridgeline Plot")
  ),
  ridgelines_ui(PANEL.NAMESPACE)
)

ADD_PANEL(crowd_panel, panel.label = "Crowd Analysis", panel.name = tab.name)

###############################################################################
# Panel's Server Modules
###############################################################################
ridgeline <- function (input, output, session) {
  ns <- session$ns
  
  ridgeline_plot_height <- reactive({
    return(max(400, 400 * length(input$day_select)))
  })
  
  output$ridgeline_settings <- renderUI({
    areas <- ZONES$area %>% unique() %>% sort()
    days_choice = c("Day 1", "Day 2", "Day 3")
    tagList(
      checkboxGroupInput(ns("day_select"), "Days to Display",
                         choices = days_choice,
                         selected = days_choice),
      checkboxGroupInput(ns("area_select"), "Areas to Display",
                         choices = areas, selected = INITIAL_AREAS)
    )
  })
  
  output$ridgeline_plot <- renderPlot({
    plots <- c()
    days_chosen <- input$day_select
    
    if ("Day 1" %in% days_chosen) {
      plots[[(length(plots) + 1)]] <- create_daily_ridgeline_plot(DAY1_AREA, 1, input$area_select)
    }
    
    if ("Day 2" %in% days_chosen) {
      plots[[(length(plots) + 1)]] <- create_daily_ridgeline_plot(DAY2_AREA, 2, input$area_select)
    }
    
    if ("Day 3" %in% days_chosen) {
      plots[[(length(plots) + 1)]] <- create_daily_ridgeline_plot(DAY3_AREA, 3, input$area_select)
    }
    
    gridExtra::marrangeGrob(grobs = plots, nrow = length(days_chosen), ncol = 1)
  }, height = function() ridgeline_plot_height())
}

###############################################################################
# Add the tab panel's server code here
###############################################################################
ridgeline_server <- substitute({
  # Server code below
  callModule(ridgeline, PANEL.NAMESPACE)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(ridgeline_server)
