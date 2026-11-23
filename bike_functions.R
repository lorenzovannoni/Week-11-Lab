library(tidyverse)

# Helper functions from Week 10 ----------------------------------------------

#' Turn trips into events
#'
#' @description Each customer trip becomes a -1 at its start station and a +1 at
#' its end station. Each rebalancing row ("R") becomes one +1 or -1. The priority
#' column decides the order of events that happen at the same time.
#'
#' @param trips data frame of trips
#' @return data frame of events sorted by time
make_events <- function(trips) {
  customers <- trips %>% filter(start_station != "R", end_station != "R")
  added <- trips %>% filter(start_station == "R")
  removed <- trips %>% filter(end_station == "R")

  bind_rows(
    added %>% transmute(row_id, station = end_station, time = end_time,
                        inventory_change = 1, priority = 1),
    customers %>% transmute(row_id, station = start_station, time = start_time,
                            inventory_change = -1, priority = 2),
    customers %>% transmute(row_id, station = end_station, time = end_time,
                            inventory_change = 1, priority = 3),
    removed %>% transmute(row_id, station = start_station, time = start_time,
                          inventory_change = -1, priority = 4)
  ) %>%
    arrange(time, priority, row_id)
}

#' Count the bikes at each station after every event
#'
#' @param events data frame from make_events()
#' @return the same data frame with a station_count column
track_inventory <- function(events) {
  events %>%
    group_by(station) %>%
    mutate(station_count = cumsum(inventory_change)) %>%
    ungroup()
}

# Function 1: estimate arrival rates -----------------------------------------

#' Estimate NHPP arrival rates
#'
#' @param data data frame of trips with columns row_id, start_station,
#' end_station, start_time and end_time
#' @param window_start date-time when the window starts (included)
#' @param window_end date-time when the window ends (excluded). The window has
#' to be a whole number of days
#' @return data frame with one row per origin, destination and hour (0-23), with
#' columns origin, destination, hour, observed_rate, availability and rate.
#' rate is NA when availability is 0
#'
#' @examples
#' estimate_arrival_rates(bike, ymd_hms("2022-02-16 00:00:00"),
#'                        ymd_hms("2022-02-22 00:00:00"))
estimate_arrival_rates <- function(data, window_start, window_end) {
  # Check the inputs
  stopifnot(
    is.data.frame(data),
    c("row_id", "start_station", "end_station", "start_time", "end_time") %in% names(data),
    is.POSIXct(data$start_time), is.POSIXct(data$end_time),
    is.POSIXct(window_start), length(window_start) == 1,
    is.POSIXct(window_end), length(window_end) == 1,
    window_end > window_start
  )
  n_days <- as.numeric(difftime(window_end, window_start, units = "days"))
  stopifnot(n_days == round(n_days))

  data <- data %>%
    mutate(start_station = as.character(start_station),
           end_station = as.character(end_station))
  customers <- data %>% filter(start_station != "R", end_station != "R")

  # Step 1: observed rate = departures per day for each pair and hour
  observed <- customers %>%
    filter(start_time >= window_start, start_time < window_end) %>%
    count(origin = start_station, destination = end_station,
          hour = hour(start_time)) %>%
    mutate(observed_rate = n / n_days) %>%
    select(-n)

  # Step 2: add a row with no inventory change at every hour mark, so that no
  # piece of time between two rows is ever in more than one hour
  events <- make_events(data)
  hour_marks <- c(window_start,
                  seq(ceiling_date(window_start, "hour"), window_end, by = "hour"),
                  window_end)
  mark_rows <- crossing(station = unique(events$station), time = hour_marks) %>%
    mutate(inventory_change = 0)

  # Step 3: availability = fraction of each hour with at least one bike
  availability <- bind_rows(events, mark_rows) %>%
    arrange(time, priority, row_id) %>%
    track_inventory() %>%
    group_by(station) %>%
    mutate(seconds = as.numeric(difftime(lead(time), time, units = "secs"))) %>%
    ungroup() %>%
    filter(time >= window_start, time < window_end) %>%
    group_by(origin = station, hour = hour(time)) %>%
    summarize(availability = sum(seconds[station_count > 0]) / (n_days * 3600),
              .groups = "drop")

  # Step 4: give every pair all 24 hours and calculate the rate
  customers %>%
    distinct(origin = start_station, destination = end_station) %>%
    crossing(hour = 0:23) %>%
    left_join(observed, by = c("origin", "destination", "hour")) %>%
    left_join(availability, by = c("origin", "hour")) %>%
    mutate(observed_rate = replace_na(observed_rate, 0),
           rate = if_else(availability == 0, NA, observed_rate / availability))
}

# Function 2: simulate arrivals ----------------------------------------------

#' Simulate NHPP arrivals using thinning
#'
#' @param rates data frame from estimate_arrival_rates(). NA rates are treated
#' as 0
#' @param start_time date-time when the simulation starts (included)
#' @param end_time date-time when the simulation ends (excluded)
#' @return data frame with columns origin, destination and arrival_time, sorted
#' by arrival_time (zero rows if there are no arrivals)
#'
#' @examples
#' simulate_arrivals(rates, ymd_hms("2022-02-23 00:00:00"),
#'                   ymd_hms("2022-02-24 00:00:00"))
simulate_arrivals <- function(rates, start_time, end_time) {
  # Check the inputs
  stopifnot(
    is.data.frame(rates),
    c("origin", "destination", "hour", "rate") %in% names(rates),
    is.POSIXct(start_time), length(start_time) == 1,
    is.POSIXct(end_time), length(end_time) == 1,
    end_time >= start_time
  )
  rates <- rates %>% mutate(rate = replace_na(rate, 0))
  stopifnot(rates$rate >= 0)

  # Start with an empty table so the columns are right if there are no arrivals
  arrivals <- tibble(origin = character(), destination = character(),
                     arrival_time = start_time[0])
  pairs <- rates %>% distinct(origin, destination)

  for (p in seq_len(nrow(pairs))) {
    pair_rates <- rates %>%
      filter(origin == pairs$origin[p], destination == pairs$destination[p])
    M <- max(pair_rates$rate)
    if (M == 0) next

    # Generate candidate times at the maximum rate M
    candidates <- start_time[0]
    candidate_time <- start_time + rexp(1, M) * 3600
    while (candidate_time < end_time) {
      candidates <- c(candidates, candidate_time)
      candidate_time <- candidate_time + rexp(1, M) * 3600
    }

    # Keep each candidate with probability (rate in its hour) / M
    hourly_rate <- pair_rates$rate[match(hour(candidates), pair_rates$hour)]
    keep <- runif(length(candidates)) < hourly_rate / M

    arrivals <- bind_rows(arrivals, tibble(origin = pairs$origin[p],
                                           destination = pairs$destination[p],
                                           arrival_time = candidates[keep]))
  }

  arrivals %>% arrange(arrival_time)
}
