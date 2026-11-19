# Week-11-Lab
Repository for the STAT 1560 Week 11 lab. Danny and Lorenzo

# Bike-Share Arrival Simulation

This repository contains `bike_functions.R`, which has two functions for modeling bike-share trip requests:

- `estimate_arrival_rates(data, window_start, window_end)` estimates the hourly request rate for each origin–destination station pair, adjusting for times when the origin station had no bikes.
- `simulate_arrivals(rates, start_time, end_time)` uses those rates to simulate customer requests over a time window.

To run an example, load `bike_functions.R` with `source("bike_functions.R")`, run `estimate_arrival_rates()` on `sample_bike.csv`, and pass the result into `simulate_arrivals()`.
