# --- OD match approach for working trips ----
# base scenario
working_od <- hh_eur5_workers %>%
  #filter(hhid %in% hh_eur5_workers$hhid) %>%
  # filter(RegioStaR7 == "very low - low") %>%
  #filter(previous_purpose == "WORK" | next_purpose == "WORK") %>% 
  #filter(start_zone %in% lez$id | end_zone %in% lez$id) %>%
  distinct(person_id, start_x, start_y, end_x, end_y, next_purpose, .keep_all = TRUE)

# scenario 1
working1_od <- legs1_week_hh %>%
  filter(hhid %in% hh_eur5_workers$hhid) %>%
  # filter(RegioStaR7 == "very low - low") %>%
  filter(previous_purpose == "WORK" | next_purpose == "WORK") %>% 
  filter(start_zone %in% lez$id | end_zone %in% lez$id) %>% 
  distinct(person_id, start_x, start_y, end_x, end_y, next_purpose, .keep_all = TRUE)

# why is there a household loss from base scenario to scenario 1?
person_available <- legs1 %>%
  filter(person_id == "3207369")
# in scenario 1, this person does not work anymore, although this person worked in the base scenario

# match ODs across scenarios, keep all variables
matched_trips <- working_od %>%
  inner_join(
    working1_od,
    by = c("person_id", "start_x", "start_y", "end_x", "end_y", "next_purpose", "day"), 
    suffix = c("_base", "_scenario1")
  ) %>%
  group_by(hhid_base) %>%
  filter(any(mode_base == "CAR_DRIVER" & emissionClass_base == "EURO_5")) %>%
  ungroup()

write.csv(matched_trips, file = "OD_working_trips_diff_RegioStaR7.csv", row.names = FALSE)

# --- Household-level metrics ---
hh_matched_metrics <- matched_trips %>%
  group_by(hhid_base) %>%
  summarise(
    # --- base scenario ---
    mean_time_min     = mean(time_min_base, na.rm = TRUE),
    mean_distance_m   = mean(distance_m_base, na.rm = TRUE),
    total_time_min    = sum(time_min_base, na.rm = TRUE),
    total_distance_m  = sum(distance_m_base, na.rm = TRUE),
    n_legs            = n(),
    
    # mode shares base
    share_CAR_DRIVER_base    = mean(mode_base == "CAR_DRIVER"),
    share_CAR_PASSENGER_base = mean(mode_base == "CAR_PASSENGER"),
    share_WALK_base          = mean(mode_base == "WALK"),
    share_BIKE_base          = mean(mode_base == "BIKE"),
    share_BUS_base           = mean(mode_base == "BUS"),
    share_TRAIN_base         = mean(mode_base == "TRAIN"),
    share_TRAM_METRO_base    = mean(mode_base == "TRAM_METRO"),
    
    # --- scenario 1 ---
    mean_time_min1    = mean(time_min_scenario1, na.rm = TRUE),
    mean_distance_m1  = mean(distance_m_scenario1, na.rm = TRUE),
    total_time_min1   = sum(time_min_scenario1, na.rm = TRUE),
    total_distance_m1 = sum(distance_m_scenario1, na.rm = TRUE),
    n_legs1           = n(),
    
    # mode shares scenario 1
    share_CAR_DRIVER_1    = mean(mode_scenario1 == "CAR_DRIVER"),
    share_CAR_PASSENGER_1 = mean(mode_scenario1 == "CAR_PASSENGER"),
    share_WALK_1          = mean(mode_scenario1 == "WALK"),
    share_BIKE_1          = mean(mode_scenario1 == "BIKE"),
    share_BUS_1           = mean(mode_scenario1 == "BUS"),
    share_TRAIN_1         = mean(mode_scenario1 == "TRAIN"),
    share_TRAM_METRO_1    = mean(mode_scenario1 == "TRAM_METRO"),
    
    # --- household attributes ---
    RegioStaR7        = first(RegioStaR7_base),
    oek_status_gr     = first(oek_status_gr_base), # kept just in case
    autos             = first(autos_base),
    
    .groups = "drop"
  )

write.csv(hh_matched_metrics, file = "OD_working_trips_stats_RegioStaR7.csv", row.names = FALSE)

# --- Household-level differences ---
hh_matched_diff <- hh_matched_metrics %>%
  mutate(
    # absolute differences
    time_diff            = total_time_min - total_time_min1,
    time_mean_diff       = mean_time_min - mean_time_min1,
    distance_diff        = total_distance_m - total_distance_m1,
    distance_mean_diff   = mean_distance_m - mean_distance_m1,
    n_legs_diff          = n_legs - n_legs1,
    
    # relative differences (safe against zero base)
    increase_time_total  = ifelse(total_time_min == 0, NA, time_diff / total_time_min),
    increase_time_mean   = ifelse(mean_time_min == 0, NA, time_mean_diff / mean_time_min),
    decrease_dist_total  = ifelse(total_distance_m == 0, NA, distance_diff / total_distance_m),
    decrease_dist_mean   = ifelse(mean_distance_m == 0, NA, distance_mean_diff / mean_distance_m),
    
    # mode share differences
    share_diff_CAR         = share_CAR_DRIVER_base    - share_CAR_DRIVER_1,
    share_diff_CAR_P       = share_CAR_PASSENGER_base - share_CAR_PASSENGER_1,
    share_diff_WALK        = share_WALK_base          - share_WALK_1,
    share_diff_BIKE        = share_BIKE_base          - share_BIKE_1,
    share_diff_BUS         = share_BUS_base           - share_BUS_1,
    share_diff_TRAIN       = share_TRAIN_base         - share_TRAIN_1,
    share_diff_TRAM_METRO  = share_TRAM_METRO_base    - share_TRAM_METRO_1
  )

# --- Grouping to RegioStaR7 ---
RegioStaR7_group_matched_summary <- hh_matched_metrics %>%
  group_by(RegioStaR7) %>%
  summarise(
    mean_time_base  = mean(mean_time_min, na.rm = TRUE),
    mean_time_scen1 = mean(mean_time_min1, na.rm = TRUE),
    mean_diff       = mean_time_base - mean_time_scen1,
    .groups = "drop"
  )

hh_matched_group_RegioStaR7 <- hh_matched_diff %>%
  group_by(RegioStaR7) %>%
  summarise(
    across(c(time_diff, time_mean_diff, distance_diff, distance_mean_diff,
             increase_time_total, increase_time_mean,
             decrease_dist_total, decrease_dist_mean,
             n_legs_diff,
             share_diff_CAR, share_diff_CAR_P, share_diff_WALK,
             share_diff_BIKE, share_diff_BUS, share_diff_TRAIN, share_diff_TRAM_METRO),
           mean, na.rm = TRUE),
    n_hhs = n(),
    .groups = "drop"
  )
