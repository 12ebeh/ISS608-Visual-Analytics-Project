###############################################################################
# Include Libraries
###############################################################################
packages <- c("tidyverse" = T, "tidygraph" = T, "lubridate" = T,
              "shiny" = T, "shinyTree"= T, "shinydashboard" = T, "shinyjs" = T, "plotly" = T, 
              "igraph" = T, "visNetwork" = T,
              "funModeling" = F, "imager" = F, "grid" = F, "sqldf" = F, "ggridges" = F,
              "gridExtra" = F, "networkD3" = F, "ggraph" = F)
installed <- installed.packages()
for (p in 1:length(packages)) {
  package.name <- names(packages[p])
  if (!(package.name %in% installed)) {
    install.packages(package.name)
  }
  if (packages[p] == T) {
    print(paste("Loading", package.name))
    library(package.name, character.only = T)
  }
}

###############################################################################
# External Source Files
###############################################################################
source("logic.R")

###############################################################################
# Core Data Files
###############################################################################
LOAD_DATA()

SENSORS <<- clean_sensors(SENSORS)
for (i in 1:length(DAYS)) {
  DAYS[[i]] <<- clean_days(DAYS[[1]])
}

###############################################################################
# Panel Files
###############################################################################
# Panels to display
PANELS <<- c()
SIDEBAR_MENU <<- c()
ADD_PANEL <<- function(new.panel, panel.label, panel.name, icon = NULL) {
  if (class(new.panel) != "shiny.tag") {
    print(paste("New Panel is not a shiny.tag, class =", class(new.panel)))
    return()
  }
  
  PANELS[[(length(PANELS) + 1)]] <<- new.panel
  
  SIDEBAR_MENU[[(length(SIDEBAR_MENU) + 1)]] <<-
    menuItem(panel.label, tabName = panel.name, icon = icon)
}

# Server code for respective panels
SERVER.LOGIC <<- c()
ADD_SERVER_LOGIC <<- function(new.logic) {
  if (class(new.logic) != "{") {
    print(paste("New Server Logic is not a closure, class =", class(new.logic)))
    return()
  }
  
  SERVER.LOGIC[[(length(SERVER.LOGIC) + 1)]] <<- new.logic
}

#source("./app panels/template_panel.R")
source("./app panels/data_prep.R", local = T)
source("./app panels/crowd.R", local = T)
source("./app panels/network.R", local = T)
#source("./app panels/choropleth.R", local = T)

###############################################################################
# APP UI
###############################################################################
ui <- dashboardPage(
  dashboardHeader(title = "MoVis"),
  dashboardSidebar(
    do.call(sidebarMenu, SIDEBAR_MENU)
  ),
  dashboardBody(
    useShinyjs(),
    do.call(tabItems, PANELS)
  )
)

###############################################################################
# APP SERVER
###############################################################################
server <- function(input, output, session) {
  for (logic in SERVER.LOGIC) {
    #print(logic)
    eval(logic)
  }
}

shiny::shinyApp(ui, server)