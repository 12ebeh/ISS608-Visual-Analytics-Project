###############################################################################
# Include Libraries
###############################################################################
packages <- c("tidyverse" = T, "lubridate" = T,
              "shiny" = T, "shinyTree"= T,
              "funModeling" = F, "imager" = F, "grid" = F, "sqldf" = F, "ggridges" = F,
              "gridExtra" = F, "networkD3" = F)
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
DAY1 <<- read_csv("./data/day1.csv")
DAY2 <<- read_csv("./data/day2.csv")
DAY3 <<- read_csv("./data/day3.csv")
DAY1_SIMPLIFIED <<- read_csv("./data/day1_simplified.csv")
DAY2_SIMPLIFIED <<- read_csv("./data/day2_simplified.csv")
DAY3_SIMPLIFIED <<- read_csv("./data/day3_simplified.csv")
DAY1_AREA <<- read_csv("./data/day1_area_visitors.csv")
DAY2_AREA <<- read_csv("./data/day2_area_visitors.csv")
DAY3_AREA <<- read_csv("./data/day3_area_visitors.csv")
SENSORS <<- read_csv("./data/sensor location.csv")
ZONES <<- read_csv("./data/zones.csv")
FLOOR1 <<- grid::rasterGrob(imager::load.image("./floor plan/floor 1.jpg"),
                           width = unit(1,"npc"), height = unit(1,"npc"))
FLOOR2 <<- grid::rasterGrob(imager::load.image("./floor plan/floor 2.jpg"),
                           width = unit(1,"npc"), height = unit(1,"npc"))

SENSORS <<- clean_sensors(SENSORS)
DAY1 <- clean_days(DAY1)
DAY2 <- clean_days(DAY2)
DAY3 <- clean_days(DAY3)

###############################################################################
# Panel Files
###############################################################################
# Panels to display
PANELS <<- c()
ADD_PANEL <<- function(new.panel) {
  if (class(new.panel) != "shiny.tag") {
    print(paste("New Panel is not a shiny.tag, class =", class(new.panel)))
    return()
  }
  
  PANELS[[(length(PANELS) + 1)]] <<- new.panel
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
source("./app panels/ridgelines.R", local = T)
source("./app panels/choropleth.R", local = T)

###############################################################################
# APP UI
###############################################################################
ui <- shiny::fluidPage(
  fluidRow(h1("ChinaVis 2019")),
  do.call(tabsetPanel, PANELS)
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