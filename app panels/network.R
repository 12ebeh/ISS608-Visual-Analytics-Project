###############################################################################
# Network Analysis Panel's Module Code
###############################################################################
PANEL.NAMESPACE <- "network"

###############################################################################
# Panel's UI Modules
###############################################################################
network_visualization_ui <- function(namespace) {
  ns <- NS(namespace)
  
  box(
    title = "Network Graph", collapsible = T, width = 12,
    visNetworkOutput(ns("network_vis_plot"), height = "auto")
  )
}

chord_and_sunburst_ui <- function(namespace) {
  ns <- NS(namespace)
  
  fluidRow(
    box(
      title = "Sunburst Diagram", collapsible = T, width = 12,
      sunburstOutput(ns("sunburst_plot"))
    ),
    box(
      title = "Chord Diagram", collapsible = T, width = 12,
      chorddiagOutput(ns("chord_plot"))
    )
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
    box(width = 12, title = "Network Controls",
      column(2, selectInput("network_day_select", label = "Select Day",
                            choices =  c("Day 1" = 1, "Day 2" = 2, "Day 3" = 3))),
      column(2, selectInput("network_measure_select", label = "Select Network Measure",
                            choices =  c("Closeness", "Betweenness", "In-degree", "Out-degree"))),
      column(8, uiOutput("network_time_control"))
    )
  ),
  fluidRow(
    network_visualization_ui(PANEL.NAMESPACE)
  ),
  chord_and_sunburst_ui(paste(PANEL.NAMESPACE,"2", sep = "_"))
)

# call this function to add the tab panel
ADD_PANEL(network_panel, panel.label = "Network Analysis", panel.name = tab.name)

###############################################################################
# Panel's Server Modules
###############################################################################
network_visualization <- function(input, output, session, prv) {
  ns <- session$ns
  
  data_simplified <- reactive({
    prv <- prv()
    return(DAYS_SIMPLIFIED[[prv$day_selected]])
  })
  
  data_area <- reactive({
    prv <- prv()
    return(DAYS_AREA[[prv$day_selected]])
  })
  
  day_movement <- reactive({
    prv <- prv()
    areas <- data_area() %>% .$area %>% unique()
    create_daily_movement(data_simplified(), areas, prv$start_time, prv$end_time) %>%
      return()
  })
  
  output$network_vis_plot <- renderVisNetwork({
    df <- day_movement()
    edges <- create_location_edges(df)
    nodes <- create_location_nodes(edges)
    location_graph <- create_location_graph(edges, nodes)
    
    nodes_coord <- left_join(select(nodes, "id"), select(ZONES, "area", "x", "y"), by = c("id" = "area"))
    colnames(nodes_coord) <- c("id", "x", "y")
    
    edges <- igraph::as_data_frame(location_graph, what="edges")
    nodes <- igraph::as_data_frame(location_graph, what="vertices")
    nodes$title <- V(location_graph)$name
    colnames(nodes)[1] <- "id"
    
    prv <- prv()
    print(prv$graph_measure)
    if (is.null(prv$graph_measure) | prv$graph_measure == "Closeness") {
      nodes$value <- V(location_graph)$closeness
    } else if (prv$graph_measure == "In-degree") {
      nodes$value <- V(location_graph)$indegree
    } else if (prv$graph_measure == "Out-degree") {
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
    areas <- data_area() %>% .$area %>% unique()
    create_sunburst_data(data_simplified(), areas, prv$start_time, prv$end_time) %>%
      return()
  })
  
  data_chord <- reactive({
    prv <- prv()
    areas <- data_area() %>% .$area %>% unique()
    create_chord_data(data_simplified(), areas, prv$start_time, prv$end_time) %>%
      return()
  })
  
  output$sunburst_plot <- renderSunburst({
    sunburst(data_sunburst())
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
network_server <- substitute({
  rv <- reactiveValues(
    day_selected = 1,
    start_time = 0,
    end_time = 86400,
    graph_measure = "Closeness"
  )
  
  data_simplified <- reactive({
    return(DAYS_SIMPLIFIED[[rv$day_selected]])
  })
  
  day_time_settings <- reactive({
    df <- data_simplified()
    return(c(min(df$time), max(df$time)))
  })
  
  observeEvent(input$network_day_select, {
    rv$day_selected <- as.integer(input$network_day_select)
    print(paste("day_selected changed:", rv$day_selected))
  })
  
  observeEvent(input$network_measure_select, {
    rv$graph_measure <- input$network_measure_select
    print(paste("graph_measure changed:", rv$graph_measure))
  })
  
  observeEvent(input$network_time_range, {
    rv$start_time <- as.integer(input$network_time_range[1])
    rv$end_time <- as.integer(input$network_time_range[2])
    print(paste("start_time changed:", rv$start_time))
    print(paste("end_time changed:", rv$end_time))
  })
  
  output$network_time_control <- renderUI({
    slider.settings <- day_time_settings()
    sliderInput("network_time_range", label = "Time Range",
                min = slider.settings[1], max = slider.settings[2],
                value = c(slider.settings[1], slider.settings[2]))
  })
  
  callModule(network_visualization, PANEL.NAMESPACE, function() return(rv))
  callModule(chord_and_sunburst, paste(PANEL.NAMESPACE, "2", sep = "_"), function() return(rv))
})

ADD_SERVER_LOGIC(network_server)