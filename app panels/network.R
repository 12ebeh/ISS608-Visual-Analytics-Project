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
    title = "Network Graph", collapsible = T, width = 12,
    fluidRow(
      column(1, selectInput(ns("day_select"), label = "Select Day",
                            choices =  c("Day 1" = 1, "Day 2" = 2, "Day 3" = 3))),
      column(2, selectInput(ns("measure_select"), label = "Select Measure",
                            choices =  c("Closeness", "Betweenness", "In-degree", "Out-degree"))),
      column(9, uiOutput(ns("network_vis_control")))
    ),
    visNetworkOutput(ns("network_vis_plot"), height = "auto")
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
    d <- is.null(input$day_select)
    if (d) {
      return(1)
    }
    
    return(as.integer(input$day_select))
  })
  
  data_simplified <- reactive({
    return(DAYS_SIMPLIFIED[[day_selected()]])
  })
  
  data_area <- reactive({
    return(DAYS_AREA[[day_selected()]])
  })
  
  day_time_settings <- reactive({
    df <- data_simplified()
    
    return(c(min(df$time), max(df$time)))
  })
  
  day_movement <- reactive({
    areas <- data_area() %>% .$area %>% unique()
    create_daily_movement(data_simplified(), areas, input$time_range[1], input$time_range[2]) %>%
      return()
  })
  
  output$network_vis_control <- renderUI({
    slider.settings <- day_time_settings()
    print(slider.settings)
    sliderInput(ns("time_range"), label = "Time Range",
                min = slider.settings[1], max = slider.settings[2],
                value = c(slider.settings[1], slider.settings[2]))
  })
  
  output$network_vis_plot <- renderVisNetwork({
    df <- day_movement()
    print(nrow(df))
    edges <- create_location_edges(df)
    nodes <- create_location_nodes(edges)
    location_graph <- create_location_graph(edges, nodes)
    
    nodes_coord <- left_join(select(nodes, "id"), select(ZONES, "area", "x", "y"), by = c("id" = "area"))
    colnames(nodes_coord) <- c("id", "x", "y")
    
    edges <- igraph::as_data_frame(location_graph, what="edges")
    nodes <- igraph::as_data_frame(location_graph, what="vertices")
    nodes$title <- V(location_graph)$name
    colnames(nodes)[1] <- "id"
    print(input$measure_select)
    if (is.null(input$measure_select) | input$measure_select == "Closeness") {
      nodes$value <- V(location_graph)$closeness
    } else if (input$measure_select == "In-degree") {
      nodes$value <- V(location_graph)$indegree
    } else if (input$measure_select == "Out-degree") {
      nodes$value <- V(location_graph)$outdegree
    } else {
      nodes$value <- V(location_graph)$betweenness
    }
    edges$width = E(location_graph)$weight/600
    coords <- as.matrix(nodes_coord[,2:3])
    
    visNetwork(nodes, edges, main = "Network Diagram of Event Locations") %>%
      visIgraphLayout(layout = "layout.norm", layoutMatrix = coords) %>%
      visNodes(font = list(size=20)) %>%
      visEdges(arrows = list(to = list(enabled=TRUE, scaleFactor=0.5)), color="grey", smooth = list(enabled = TRUE, type = "curvedCW")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      #visLegend(width = 0.2, position = "right", main = "Area Groups", useGroups =TRUE, stepX = 50) %>%
      visInteraction(navigationButtons = TRUE, dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)
  })
  
}

###############################################################################
# Add the tab panel's server code here
###############################################################################
network_server <- substitute({
  callModule(network_visualization, PANEL.NAMESPACE)
})

ADD_SERVER_LOGIC(network_server)