# --- during weekdays (Monday - Friday) ----
### base Scenario
### base Scenario
# hh-level metrics
legs_week_hh_nonwork <- legs_joined %>%
  filter(hhid %in% euro5_persons$hhid) %>% # take only persons who really use a EURO_5 vehicle to enter the low emission zone
  filter(day != 6) %>%
  filter(day != 7) %>%
  filter(!(
    (next_purpose == "WORK" |previous_purpose == "WORK" )
  ))
  
hh_metrics_nonwork <- legs_week_hh_nonwork %>%
  group_by(hhid, oek_status_gr) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

# hh-level mode shares
hh_mode_share_nonwork <- legs_week_hh_nonwork %>%
  group_by(hhid, oek_status_gr, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, oek_status_gr) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

# Combine hh-level metrics + mode shares
hh_all_nonwork <- hh_metrics_nonwork %>%
  left_join(hh_mode_share_nonwork, by = c("hhid", "oek_status_gr"))

# Aggregate to oek_status_gr level 
hh_oek_status_metrics_nonwork <- hh_all_nonwork %>%
  group_by(oek_status_gr) %>%
  summarise(
    mean_time_min       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs         = mean(n_legs, na.rm = TRUE),
    
    # Dispersion metrics
    cv_total_time       = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time      = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time      = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time      = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25       = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share_{.col}"),
    n_hhs = n(),
    .groups = "drop"
    
  )

### Scenario 1 
# hh-level metrics
legs1_week_hh_nonwork <- legs1_joined %>%
  filter(person_id %in% legs_week_hh_nonwork$person_id) %>%
  filter(day != 6) %>%
  filter(day != 7) %>%
  filter(!(
    (next_purpose == "WORK" |previous_purpose == "WORK" )
  ))
#consistency check
# filter base scenario so that in both scenarios are the same persons
length(unique(legs_week_hh_nonwork$hhid))
length(unique(legs1_week_hh_nonwork$hhid))
# week: there are four households from the base scenario not in scenario 1
# weekend: there are 1213-942=271 household from the base scenario not in scenario 1
legs_week_hh_nonwork <- legs_week_hh_nonwork %>%
  semi_join(legs1_week_hh_nonwork, by = "hhid")

hh1_metrics_nonworking <- legs1_week_hh_nonwork %>%
  group_by(hhid, oek_status_gr) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

# hh-level mode shares (wide format: one column per mode)
hh1_mode_share_nonworking <- legs1_week_hh_nonwork %>%
  group_by(hhid, oek_status_gr, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, oek_status_gr) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

# Combine hh-level metrics + mode shares
hh1_all_nonworking <- hh1_metrics_nonworking %>%
  left_join(hh1_mode_share_nonworking, by = c("hhid", "oek_status_gr"))

# Aggregate to oek_status_gr level 
hh_oek_status1_metrics_nonworking <- hh1_all_nonworking %>%
  group_by(oek_status_gr) %>%
  summarise(
    mean_time_min1       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m1     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time1     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance1 = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs1         = mean(n_legs, na.rm = TRUE),
    
    # Dispersion metrics
    cv_total_time       = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time      = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time      = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time      = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25       = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share1_{.col}"),
    n_hhs = n(),
    .groups = "drop"
  )

# differences during weekdays
hh_metrics_diff_nonworking <- hh_oek_status_metrics_nonwork %>%
  inner_join(hh_oek_status1_metrics_nonworking, by = "oek_status_gr") %>%
  mutate(time_diff = mean_total_time - mean_total_time1) %>%
  mutate(time_mean_diff = mean_time_min - mean_time_min1) %>%
  mutate(distance_diff = mean_total_distance - mean_total_distance1) %>%
  mutate(distance_mean_diff = mean_distance_m - mean_distance_m1) %>%
  mutate(increase_time_total = time_diff / mean_total_time) %>%
  mutate(increase_time_mean = time_mean_diff / mean_time_min) %>%
  mutate(decrease_dist_total = distance_diff / mean_total_distance) %>%
  mutate(decrease_dist_mean = distance_mean_diff / mean_distance_m) %>%
  mutate(n_legs_diff = mean_n_legs - mean_n_legs1) %>%
  mutate(share_diff_CAR = share_CAR_DRIVER - share1_CAR_DRIVER) %>%
  mutate(share_diff_CAR_P = share_CAR_PASSENGER - share1_CAR_PASSENGER) %>%
  mutate(share_diff_WALK = share_WALK - share1_WALK) %>%
  mutate(share_diff_BIKE = share_BIKE - share1_BIKE) %>%
  mutate(share_diff_BUS = share_BUS - share1_BUS) %>%
  mutate(share_diff_TRAIN = share_TRAIN - share1_TRAIN) %>%
  mutate(share_diff_TRAM_METRO = share_TRAM_METRO - share1_TRAM_METRO)

write.csv(hh_metrics_diff, file = "hh_tt_dt_diff_week_20250924.csv", row.names=FALSE)

# --- during weekends (Saturday - Sunday) ----
legs_weekend_hh <- legs_joined %>%
  filter(hhid %in% euro5_persons$hhid,
         day %in% c(6, 7)) %>%
  filter(!(
    (next_purpose == "WORK" & end_zone %in% lez$id) |
      (previous_purpose == "WORK" & start_zone %in% lez$id)
  ) |
    !(start_time_min >= 0 & start_time_min < 6*60))

legs1_weekend_hh <- legs1_joined %>%
  filter(hhid %in% legs_weekend$hhid) %>% #make sure that here are the same persons than in the base scenario
  filter(day %in% c(6, 7))

#consistency check
# filter base scenario so that in both scenarios are the same persons
length(unique(legs_weekend_hh$hhid))
length(unique(legs1_weekend_hh$hhid))
# weekend: there are 1213-942=271 household from the base scenario not in scenario 1
legs_weekend_hh <- legs_weekend_hh %>%
  semi_join(legs1_weekend_hh, by = "hhid")


### base Scenario
# hh-level metrics
hh_metrics_w <- legs_weekend_hh %>%
  group_by(hhid, oek_status_gr) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

# hh-level mode shares
hh_mode_share_w <- legs_weekend_hh %>%
  group_by(hhid, oek_status_gr, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, oek_status_gr) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

# Combine hh-level metrics + mode shares
hh_all_w <- hh_metrics_w %>%
  left_join(hh_mode_share_w, by = c("hhid", "oek_status_gr"))

# Aggregate to oek_status_gr level 
hh_oek_status_metrics_w <- hh_all_w %>%
  group_by(oek_status_gr) %>%
  summarise(
    mean_time_min       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs         = mean(n_legs, na.rm = TRUE),
    
    # Dispersion metrics
    cv_total_time       = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time      = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time      = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time      = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25       = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share_{.col}"),
    n_hhs = n(),
    .groups = "drop"
    
  )

### Scenario 1 
# hh-level metrics
hh1_metrics_w <- legs1_weekend_hh %>%
  group_by(hhid, oek_status_gr) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

# hh-level mode shares (wide format: one column per mode)
hh1_mode_share_w <- legs1_weekend_hh %>%
  group_by(hhid, oek_status_gr, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, oek_status_gr) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

# Combine hh-level metrics + mode shares
hh1_all_w <- hh1_metrics_w %>%
  left_join(hh1_mode_share_w, by = c("hhid", "oek_status_gr"))

# Aggregate to oek_status_gr level
hh_oek_status1_metrics_w <- hh1_all_w %>%
  group_by(oek_status_gr) %>%
  summarise(
    mean_time_min1       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m1     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time1     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance1 = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs1         = mean(n_legs, na.rm = TRUE),
    
    # Dispersion metrics
    cv_total_time       = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time      = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time      = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time      = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25       = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share1_{.col}"),
    n_hhs = n(),
    .groups = "drop"
  )

# differences during weekends
hh_metrics_diff_w <- hh_oek_status_metrics_w %>%
  inner_join(hh_oek_status1_metrics_w, by = "oek_status_gr") %>%
  mutate(time_diff = mean_total_time - mean_total_time1) %>%
  mutate(time_mean_diff = mean_time_min - mean_time_min1) %>%
  mutate(distance_diff = mean_total_distance - mean_total_distance1) %>%
  mutate(distance_mean_diff = mean_distance_m - mean_distance_m1) %>%
  mutate(increase_time_total = time_diff / mean_total_time) %>%
  mutate(increase_time_mean = time_mean_diff / mean_time_min) %>%
  mutate(decrease_dist_total = distance_diff / mean_total_distance) %>%
  mutate(decrease_dist_mean = distance_mean_diff / mean_distance_m) %>%
  mutate(n_legs_diff = mean_n_legs - mean_n_legs1) %>%
  mutate(share_diff_CAR = share_CAR_DRIVER - share1_CAR_DRIVER) %>%
  mutate(share_diff_CAR_P = share_CAR_PASSENGER - share1_CAR_PASSENGER) %>%
  mutate(share_diff_WALK = share_WALK - share1_WALK) %>%
  mutate(share_diff_BIKE = share_BIKE - share1_BIKE) %>%
  mutate(share_diff_BUS = share_BUS - share1_BUS) %>%
  mutate(share_diff_TRAIN = share_TRAIN - share1_TRAIN) %>%
  mutate(share_diff_TRAM_METRO = share_TRAM_METRO - share1_TRAM_METRO)

write.csv(hh_metrics_diff_w, file = "hh_tt_td_diff_weekend_20250924.csv")
