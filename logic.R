INITIAL_AREAS <<- c("main convention", "exhibition hall a", "exhibition hall b", "exhibition hall c", "exhibition hall d", "exhibition hall",
                    "poster area", "room 1", "room 2", "room 3", "room 4", "room 5", "room 6", "restaurant", "rest area")

DATA_SUFFIXES <<- c("SIMPLIFIED"="_simplified", "AREA"="_area_visitors", "MOVEMENT"="_movement", "CONNECTION"="_time_connection")

LOAD_FILES <<- function(files) {
  ret <- c()
  for (f in 1:length(files)) {
    if (file.exists(files[f])) {
      ret[[f]] <- read_csv(files[f])
    } else {
      ret[[f]] <- F
    }
  }
  return(ret)
}

LOAD_DATA <<- function() {
  day.list <- c("day1", "day2", "day3")
  
  files <- paste("./data/", day.list, ".csv", sep = "")
  DAYS <<- LOAD_FILES(files)
  
  files <- paste("./data/", day.list, DATA_SUFFIXES["SIMPLIFIED"], ".csv", sep = "")
  DAYS_SIMPLIFIED <<- LOAD_FILES(files)
  
  files <- paste("./data/", day.list, DATA_SUFFIXES["AREA"], ".csv", sep = "")
  DAYS_AREA <<- LOAD_FILES(files)
  
  files <- paste("./data/", day.list, DATA_SUFFIXES["MOVEMENT"], ".csv", sep = "")
  DAYS_MOVEMENT <<- LOAD_FILES(files)
  
  files <- paste("./data/", day.list, DATA_SUFFIXES["CONNECTION"], ".csv", sep = "")
  DAYS_CONNECTION <<- LOAD_FILES(files)
  
  SENSORS <<- read_csv("./data/sensor location.csv")
  ZONES <<- read_csv("./data/zones.csv")
  FLOOR1 <<- grid::rasterGrob(imager::load.image("./floor plan/floor 1.jpg"),
                              width = unit(1,"npc"), height = unit(1,"npc"))
  FLOOR2 <<- grid::rasterGrob(imager::load.image("./floor plan/floor 2.jpg"),
                              width = unit(1,"npc"), height = unit(1,"npc"))
}

clean_sensors <- function(df) {
  df <-
    df %>%
    #mutate(sid = as.factor(sid)) %>%
    mutate_if(is.numeric, as.integer) %>%
    mutate(px = y,
           py = max(x) - x)
    
  df <- sqldf::sqldf("
  SELECT `df`.sid, `df`.floor, `df`.x, `df`.y, `df`.px, `df`.py, `ZONES`.area
  FROM `df`
  LEFT JOIN `ZONES`
  ON `df`.floor = `ZONES`.floor
  AND `df`.px >= `ZONES`.start_px AND `df`.px <= `ZONES`.end_px
  AND `df`.py >= `ZONES`.start_py AND `df`.py <= `ZONES`.end_py
  ")
  df$area[is.na(df$area)] <- "common"
  return(df)
}

clean_days <- function(df) {
  df %>%
    mutate(id = as.factor(id),
           sid = as.integer(sid),
           time = as.integer(time),
           time_of_day = seconds_to_period(time)) %>%
    return()
}

simplify_day <- function(df) {
  df %>%
    arrange(id, time) %>%
    select(id, time, area, floor, px, py) %>%
    group_by(id) %>%
    mutate(previous_area = lag(area),
           area_change = ifelse(!is.na(previous_area) & area != previous_area, 1, 0),
           area_index = cumsum(area_change)) %>%
    ungroup() %>%
    arrange(id, time) %>%
    group_by(id, area_index, area, floor, px, py) %>%
    summarise(time = min(time)) %>%
    ungroup() %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(time_end = lead(time) - 1) %>%
    mutate(time_end = ifelse(is.na(time_end), time + 60, time_end),
           time_stayed = time_end - time) %>%
    ungroup() %>%
    return()
}

calculate_area_visitors <- function(simplified.df, areas.include, time.interval) {
  timeslots <- data.frame(start_time = seq(as.integer(min(simplified.df$time)/time.interval) * time.interval,
                                           as.integer(max(simplified.df$time_end)/time.interval) * time.interval,
                                           time.interval))
  timeslots$end_time <- lead(timeslots$start_time) - 1
  timeslots$join <- 1
  timeslots <-
    SENSORS %>%
    select(floor, px, py, area) %>%
    filter(area %in% areas.include) %>%
    mutate(join = 1) %>%
    left_join(timeslots, by = "join") %>%
    select(floor, px, py, area, start_time, end_time)
  
  sqldf::sqldf("
    SELECT `timeslots`.area, `timeslots`.floor, `timeslots`.px, `timeslots`.py,`timeslots`.start_time, `timeslots`.end_time, `simplified.df`.id
    FROM `timeslots` LEFT JOIN `simplified.df`
    ON `timeslots`.floor = `simplified.df`.floor
    AND `timeslots`.px = `simplified.df`.px
    AND `timeslots`.py = `simplified.df`.py
    AND `timeslots`.area = `simplified.df`.area
    AND NOT (`timeslots`.start_time > `simplified.df`.time_end OR `timeslots`.end_time < `simplified.df`.time)") %>%
    drop_na() %>%
    return()
}

create_daily_movement <- function(simplified.df, areas.include) {
  simplified.df %>%
    filter(area %in% areas.include) %>%
    group_by(id, area_index, area) %>%
    summarise(time = min(time),
              time_end = max(time_end),
              time_stayed = sum(time_stayed)) %>%
    group_by(id) %>%
    mutate(previous_area = lag(area)) %>%
    filter(area != previous_area) %>%
    mutate(area_count = 1,
           area_index = cumsum(area_count)) %>%
    select(id, area_index, previous_area, area) %>%
    group_by(previous_area, area) %>%
    summarise(movement_count = n(),
              unique_visitors = length(unique(id))) %>%
    ungroup() %>%
    rename(source = previous_area, target = area) %>%
    return()
}

create_time_connection <- function(day.area.df, time.interval) {
  # Time
  time.df <-
    day.area.df %>%
    select(-area, -end_time) %>%
    arrange(start_time, floor, px, py) %>%
    group_by(start_time, floor, px, py, id) %>%
    summarise(present = 1) %>%
    group_by(start_time, floor, px, py) %>%
    mutate(multi = n()) %>%
    filter(multi > 1)
  
  time.df <- 
    full_join(time.df, time.df, by=c("start_time", "floor", "px", "py")) %>%
    group_by(id.x, id.y) %>%
    summarise(time_together = n()*time.interval) %>%
    return()
}

create_daily_data <- function(day.df, areas.include, time.interval, file_prefix) {
  day.simplified.df <-
    day.df %>%
    inner_join(SENSORS, by = "sid") %>%
    simplify_day()
  
  day.simplified.df %>%
    group_by(id, area_index, area) %>%
    summarise(time = min(time),
              time_end = max(time_end),
              time_stayed = sum(time_stayed)) %>%
    write_csv( paste(file_prefix, "simplified.csv", sep = "_"))
  
  day.movement.df <- create_daily_movement(day.simplified.df, areas.include)
  write_csv(day.movement.df, paste("./data/", file_prefix, DATA_SUFFIXES["MOVEMENT"], ".csv", sep = ""))
    
  day.area.df <- calculate_area_visitors(day.simplified.df, areas.include, time.interval)
  day.area.df %>%
    group_by(area, floor, px, py, start_time, end_time) %>%
    summarise(visitor_count = length(unique(id))) %>%
    write_csv(paste("./data/", file_prefix, DATA_SUFFIXES["AREA"], ".csv", sep = ""))
  
  create_time_connection(day.area.df, time.interval) %>%
    write_csv(paste("./data/", file_prefix, DATA_SUFFIXES["CONNECTION"], ".csv", sep = ""))
}

create_daily_ridgeline_plot <- function(day.area, day, areas.include) {
  day.area %>%
    filter(area %in% areas.include) %>%
    group_by(area, start_time) %>%
    summarise(visitor_count_index = log10(sum(visitor_count))) %>%
    ggplot(aes(x = start_time, y = as.factor(area))) +
    ggridges::geom_ridgeline(aes(height = visitor_count_index, group = as.factor(area)), size = 0.2, alpha = 0.5) +
    labs(x = "Time in Seconds", y ="Area", title = paste("Area Visitor over Time on Day", day)) %>%
    return()
}