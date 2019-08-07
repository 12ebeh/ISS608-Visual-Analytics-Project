###############################################################################
# Network Analysis Panel's Module Code
###############################################################################
PANEL.NAMESPACE <- "network"

###############################################################################
# Panel's UI Modules
###############################################################################
network_visualization <- function(namespace) {
  ns <- NS(namespace)
  
  box(
    title = "Network Graph", collapsible = T,
    column(3, uiOutput(ns("network_vis_control"))),
    column(9, visNetworkOutput(ns("network_vis_plot")), height = "600px")   
  )
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
tab.name <- "network"
network_panel <- tabItem(
  tabName = tab.name,
  fluidRow(
    box(width = 12, h2("Network Analysis"))
  ),
  fluidRow(
    network_visualization(PANEL.NAMESPACE),
    box(title = "Chord Diagram", h2("Chord Diagram"))
  )
)

# call this function to add the tab panel
ADD_PANEL(network_panel, panel.label = "Network Analysis", panel.name = tab.name)

###############################################################################
# Panel's Server Modules
###############################################################################
network_visualization <- function(input, output, session) {
  ns <- session$ns
  
  day_selected <- reactive({
    if (!is.null(input$day_select)) {
      return(as.integer(input$day_select))
    } else {
      return(1)
    }
  })
  
  nodes <- reactive({
    return(DAYS_NODE[[day_selected()]])
  })
  
  edges <- reactive({
    return(DAYS_MOVEMENT[[day_selected()]])
  })
  
  output$network_vis_control <- renderUI({
    n <- nodes()
    tagList(
      selectInput(ns("day_select"), label = "Select Day",
                  choices =  c("Day 1" = 1, "Day 2" = 2, "Day 3" = 3)),
      checkboxGroupInput(ns("area_select"), label = "Select Areas",
                         choices = n$area, selected = n$area)
    )
  })
  
  output$network_vis_plot <- renderVisNetwork({
    print(input$area_select)
    if (length(input$area_select) > 0) {
      n <- 
        nodes() %>%
        select(id, area, category) %>%
        filter(area %in% input$area_select) %>%
        rename(label=area, group=category)
      
      e <-
        edges() %>%
        filter((source %in% input$area_select) & (target %in% input$area_select)) %>%
        left_join(select(n, -group), by = c("source" = "label")) %>%
        rename(from=id) %>%
        left_join(select(n, -group), by = c("target" = "label")) %>%
        rename(to=id)
      
      visNetwork(n, e) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visEdges(arrows = "to", smooth = list(enabled = TRUE, type = "curvedCW")) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        return()
    }
  })
}

###############################################################################
# Add the tab panel's server code here
###############################################################################
network_server <- substitute({
  callModule(network_visualization, PANEL.NAMESPACE)
})

ADD_SERVER_LOGIC(network_server)