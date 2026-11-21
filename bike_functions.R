library(tidyverse)

# Helper functions from the Week 10 lab --------------------------------------

#' Build the event table
#'
#' @description Turns trip rows into inventory events. A customer trip becomes
#' a departure (-1) and an arrival (+1), and a rebalancing row becomes one event.
#' Tied times are processed as rebalancing additions, customer departures,
#' customer arrivals, then rebalancing removals.
#'
#' @param trips data frame with row_id, start_station, end_station,
#' start_time and end_time
#' @return data frame with row_id, station, time, inventory_change and
#' event_type, sorted by time
make_events <- function(trips) {
  rebal_add <- trips %>%
    filter(start_station == "R") %>%
    transmute(row_id, station = end_station, time = end_time,
              inventory_change = 1, event_type = "R", priority = 1)
  departures <- trips %>%
    filter(start_station != "R", end_station != "R") %>%
    transmute(row_id, station = start_station, time = start_time,
              inventory_change = -1, event_type = "C", priority = 2)
  arrivals <- trips %>%
    filter(start_station != "R", end_station != "R") %>%
    transmute(row_id, station = end_station, time = end_time,
              inventory_change = 1, event_type = "C", priority = 3)
  rebal_remove <- trips %>%
    filter(end_station == "R") %>%
    transmute(row_id, station = start_station, time = start_time,
              inventory_change = -1, event_type = "R", priority = 4)

  bind_rows(rebal_add, departures, arrivals, rebal_remove) %>%
    arrange(time, priority, row_id) %>%
    select(-priority)
}

#' Track station inventory
#'
#' @description Loops through the events in order and records the new count
#' of the affected station after each event. Every station starts at zero.
#'
#' @param events data frame returned by make_events()
#' @return the event table with an extra station_count column
track_inventory <- function(events) {
  stations <- unique(events$station)
  counts <- rep(0, length(stations))
  names(counts) <- stations
  station_count <- rep(NA, nrow(events))

  for (i in 1:nrow(events)) {
    s <- events$station[i]
    counts[s] <- counts[s] + events$inventory_change[i]
    station_count[i] <- counts[s]
  }
  events$station_count <- station_count
  events
}

# Function 1: estimate arrival rates -----------------------------------------

#' Estimate NHPP arrival rates
#'
#' @description Estimates the hourly rate of customer requests for each
#' origin-destination pair. Observed departures are divided by the number of
#' observation days, then adjusted for the fraction of time the origin station
#' had at least one bike during that hour.
#'
#' @param data data frame of trips with columns row_id, start_station,
#' end_station, start_time and end_time (station labels as characters, times
#' as date-times). Rebalancing rows use "R" as a station. The data should start
#' before the window so the starting inventory is known.
#' @param window_start date-time when the observation window starts (included)
#' @param window_end date-time when the observation window ends (excluded).
#' The window has to be a whole number of days.
#' @return data frame with one row for each origin-destination pair and hour
#' (0-23) containing origin, destination, hour, observed_rate (departures per
#' day), availability (fraction of the hour's observation time with at least
#' one bike at the origin) and rate (observed_rate / availability, NA when
#' availability is zero)
#'
#' @examples
#' estimate_arrival_rates(bike, ymd_hms("2022-02-16 00:00:00"),
#'                        ymd_hms("2022-02-22 00:00:00"))
estimate_arrival_rates <- function(data, window_start, window_end) {
  # Input checks
  needed <- c("row_id", "start_station", "end_station", "start_time", "end_time")
  if (!is.data.frame(data) || !all(needed %in% names(data))) {
    stop("data must be a data frame with columns row_id, start_station, end_station, start_time and end_time")
  }
  if (!is.POSIXct(data$start_time) || !is.POSIXct(data$end_time)) {
    stop("start_time and end_time must be date-times")
  }
  if (!is.POSIXct(window_start) || !is.POSIXct(window_end) ||
      length(window_start) != 1 || length(window_end) != 1) {
    stop("window_start and window_end must each be a single date-time")
  }
  if (window_end <= window_start) {
    stop("window_end must be after window_start")
  }
  n_days <- as.numeric(difftime(window_end, window_start, units = "days"))
  if (n_days != round(n_days)) {
    stop("the window must be a whole number of days")
  }

  data <- data %>%
    mutate(start_station = as.character(start_station),
           end_station = as.character(end_station))

  # Observed departures per day for each pair and hour
  customer_trips <- data %>%
    filter(start_station != "R", end_station != "R")

  observed <- customer_trips %>%
    filter(start_time >= window_start, start_time < window_end) %>%
    mutate(hour = hour(start_time)) %>%
    count(origin = start_station, destination = end_station, hour) %>%
    mutate(observed_rate = n / n_days) %>%
    select(-n)

  # Bike availability for each station and hour
  inventory <- track_inventory(make_events(data))
  stations <- unique(customer_trips$start_station)
  available_secs <- matrix(0, nrow = length(stations), ncol = 24,
                           dimnames = list(stations, 0:23))

  for (s in stations) {
    station_events <- inventory %>% filter(station == s)
    # Each count lasts from its event until the next event at that station
    piece_start <- c(window_start, station_events$time)
    piece_end <- c(station_events$time, window_end)
    piece_count <- c(0, station_events$station_count)

    for (i in 1:length(piece_count)) {
      if (piece_count[i] >= 1) {
        # Clip the piece to the window, then split it at the hour marks
        current <- max(piece_start[i], window_start)
        stop_at <- min(piece_end[i], window_end)
        while (current < stop_at) {
          next_hour <- floor_date(current, "hour") + hours(1)
          piece_stop <- min(next_hour, stop_at)
          h <- hour(current)
          available_secs[s, h + 1] <- available_secs[s, h + 1] +
            as.numeric(difftime(piece_stop, current, units = "secs"))
          current <- piece_stop
        }
      }
    }
  }

  # Each hour of the day is observed for n_days hours in total
  availability <- as_tibble(available_secs, rownames = "origin") %>%
    pivot_longer(-origin, names_to = "hour", values_to = "secs") %>%
    mutate(hour = as.integer(hour),
           availability = secs / (n_days * 3600)) %>%
    select(-secs)

  # One row for every pair and hour, including hours with no departures
  customer_trips %>%
    distinct(origin = start_station, destination = end_station) %>%
    crossing(hour = 0:23) %>%
    left_join(observed, by = c("origin", "destination", "hour")) %>%
    left_join(availability, by = c("origin", "hour")) %>%
    mutate(observed_rate = replace_na(observed_rate, 0),
           rate = if_else(availability == 0, NA_real_,
                          observed_rate / availability)) %>%
    arrange(origin, destination, hour)
}

# Function 2: simulate arrivals ----------------------------------------------

#' Simulate NHPP arrivals
#'
#' @description Simulates customer requests for each origin-destination pair
#' independently using thinning. Candidate times come from a homogeneous
#' Poisson process with the pair's maximum hourly rate, and each candidate is
#' kept with probability (rate in that hour) / (maximum rate).
#'
#' @param rates data frame returned by estimate_arrival_rates(), with columns
#' origin, destination, hour and rate (requests per hour). NA rates are treated
#' as zero since demand could not be estimated for them.
#' @param start_time date-time when the simulation starts (included)
#' @param end_time date-time when the simulation ends (excluded)
#' @return data frame with columns origin, destination and arrival_time sorted
#' by arrival_time. If there are no arrivals it has zero rows with the same
#' columns.
#'
#' @examples
#' simulate_arrivals(rates, ymd_hms("2022-02-23 00:00:00"),
#'                   ymd_hms("2022-02-24 00:00:00"))
simulate_arrivals <- function(rates, start_time, end_time) {
  # Input checks
  needed <- c("origin", "destination", "hour", "rate")
  if (!is.data.frame(rates) || !all(needed %in% names(rates))) {
    stop("rates must be a data frame with columns origin, destination, hour and rate")
  }
  if (any(rates$rate < 0, na.rm = TRUE)) {
    stop("rates cannot be negative")
  }
  if (!is.POSIXct(start_time) || !is.POSIXct(end_time) ||
      length(start_time) != 1 || length(end_time) != 1) {
    stop("start_time and end_time must each be a single date-time")
  }
  if (end_time < start_time) {
    stop("end_time cannot be before start_time")
  }

  rates <- rates %>% mutate(rate = replace_na(rate, 0))
  pairs <- rates %>% distinct(origin, destination)
  arrivals <- list()

  for (p in 1:nrow(pairs)) {
    pair_rates <- rates %>%
      filter(origin == pairs$origin[p], destination == pairs$destination[p])
    M <- max(pair_rates$rate)
    if (M == 0) next

    # Candidate times from a homogeneous Poisson process with rate M
    candidates <- c()
    candidate_time <- start_time
    repeat {
      wait <- rexp(1, M)
      candidate_time <- candidate_time + wait * 3600
      if (candidate_time >= end_time) break
      candidates <- c(candidates, candidate_time)
    }
    if (length(candidates) == 0) next
    candidates <- as_datetime(candidates, tz = tz(start_time))

    # Keep each candidate with probability (rate in its hour) / M
    hourly_rate <- pair_rates$rate[match(hour(candidates), pair_rates$hour)]
    hourly_rate <- replace_na(hourly_rate, 0)
    keep <- runif(length(candidates)) < hourly_rate / M

    arrivals[[p]] <- tibble(origin = pairs$origin[p],
                            destination = pairs$destination[p],
                            arrival_time = candidates[keep])
  }

  empty <- tibble(origin = character(), destination = character(),
                  arrival_time = as_datetime(character(), tz = tz(start_time)))
  bind_rows(empty, arrivals) %>%
    arrange(arrival_time)
}
