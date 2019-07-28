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
    mutate_at(vars(id, sid), as.factor) %>%
    mutate(time_of_day = seconds_to_period(time)) %>%
    return()
}

simplify_day <- function(df) {
  df %>%
    select(id, time, area) %>%
    group_by(id) %>%
    mutate(previous_area = lag(area),
           area_change = ifelse(!is.na(previous_area) & area != previous_area, 1, 0),
           area_index = cumsum(area_change)) %>%
    ungroup() %>%
    arrange(id, time) %>%
    group_by(id, area_index, area) %>%
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
    data.frame(area = areas.include, join = 1) %>%
    left_join(timeslots, by = "join") %>%
    select(area, start_time, end_time)
  sqldf::sqldf("
               SELECT `timeslots`.area, `timeslots`.start_time, `timeslots`.end_time, `simplified.df`.id
               FROM `timeslots` LEFT JOIN `simplified.df`
               ON `timeslots`.area = `simplified.df`.area
               AND NOT (`timeslots`.start_time > `simplified.df`.time_end OR `timeslots`.end_time < `simplified.df`.time)
               ") %>%
    drop_na() %>%
    group_by(area, start_time, end_time) %>%
    summarise(visitor_count = length(unique(id))) %>%
    return()
}

create_daily_data <- function(day.df, areas.include, time.interval, file_suffix) {
  day.df <-
    day.df %>%
    inner_join(SENSORS, by = "sid") %>%
    arrange(id, time)
  #print(day.df)
  day.simplified.df <- simplify_day(day.df)
  day.area.df <- calculate_area_visitors(day.simplified.df, areas.include, time.interval)
  
  #write_csv(day.simplified.df, paste(file_suffix, "simplified.csv", sep = "_"))
  write_csv(day.area.df, paste("./data/", file_suffix, "_area_visitors.csv", sep = ""))
}