# --- get average travel time, travel distance and mode share for household per RegioStaR7 ----
# --- during weekdays (Monday - Friday) ----

### base Scenario
legs_week_hh_Regio <- legs_joined %>%
  filter(hhid %in% euro5_persons$hhid) %>%
  filter(day != 6, day != 7) %>%
  filter(!(
    (next_purpose == "WORK" & end_zone %in% lez$id) |
      (previous_purpose == "WORK" & start_zone %in% lez$id)
  ) |
    !(start_time_min >= 0 & start_time_min < 6*60))

hh_metrics_Regio <- legs_week_hh_Regio %>%
  group_by(hhid, RegioStaR7) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

hh_mode_share_Regio <- legs_week_hh_Regio %>%
  group_by(hhid, RegioStaR7, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, RegioStaR7) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

hh_all_Regio <- hh_metrics_Regio %>%
  left_join(hh_mode_share_Regio, by = c("hhid", "RegioStaR7"))

hh_Regio_metrics <- hh_all_Regio %>%
  group_by(RegioStaR7) %>%
  summarise(
    mean_time_min       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs         = mean(n_legs, na.rm = TRUE),
    cv_total_time       = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time      = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time      = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time      = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25       = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | 
             starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share_{.col}"),
    n_hhs = n(),
    .groups = "drop"
  )

### Scenario 1
legs1_week_hh_Regio <- legs1_joined %>%
  filter(person_id %in% legs_week_hh_Regio$person_id) %>%
  filter(day != 6, day != 7)

legs_week_hh_Regio <- legs_week_hh_Regio %>%
  semi_join(legs1_week_hh_Regio, by = "hhid")

hh1_metrics_Regio <- legs1_week_hh_Regio %>%
  group_by(hhid, RegioStaR7) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

hh1_mode_share_Regio <- legs1_week_hh_Regio %>%
  group_by(hhid, RegioStaR7, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, RegioStaR7) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

hh1_all_Regio <- hh1_metrics_Regio %>%
  left_join(hh1_mode_share_Regio, by = c("hhid", "RegioStaR7"))

hh1_Regio_metrics <- hh1_all_Regio %>%
  group_by(RegioStaR7) %>%
  summarise(
    mean_time_min1       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m1     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time1     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance1 = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs1         = mean(n_legs, na.rm = TRUE),
    cv_total_time        = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time       = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time       = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time       = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25        = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | 
             starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share1_{.col}"),
    n_hhs = n(),
    .groups = "drop"
  )

# Differences
hh_Regio_diff <- hh_Regio_metrics %>%
  inner_join(hh1_Regio_metrics, by = "RegioStaR7") %>%
  mutate(
    time_mean_diff       = mean_time_min - mean_time_min1,
    increase_time_mean   = time_mean_diff / mean_time_min,
    time_diff            = mean_total_time - mean_total_time1,
    increase_time_total  = time_diff / mean_total_time,
    distance_mean_diff   = mean_distance_m - mean_distance_m1,
    decrease_dist_mean   = distance_mean_diff / mean_distance_m,
    distance_diff        = mean_total_distance - mean_total_distance1,
    decrease_dist_total  = distance_diff / mean_total_distance,
    n_legs_diff          = mean_n_legs - mean_n_legs1,
    share_diff_CAR       = share_CAR_DRIVER - share1_CAR_DRIVER,
    share_diff_CAR_P     = share_CAR_PASSENGER - share1_CAR_PASSENGER,
    share_diff_WALK      = share_WALK - share1_WALK,
    share_diff_BIKE      = share_BIKE - share1_BIKE,
    share_diff_BUS       = share_BUS - share1_BUS,
    share_diff_TRAIN     = share_TRAIN - share1_TRAIN,
    share_diff_TRAM_METRO= share_TRAM_METRO - share1_TRAM_METRO
  )

write.csv(hh_Regio_diff, file = "hh_tt_dt_Regio_diff_week_20250924.csv", row.names=FALSE)

# --- during weekend (Saturday - Sunday) ----

### base Scenario
legs_weekend_hh_Regio <- legs_joined %>%
  filter(hhid %in% euro5_persons$hhid) %>%
  filter(day %in% c(6, 7)) %>%
  filter(!(
    (next_purpose == "WORK" & end_zone %in% lez$id) |
      (previous_purpose == "WORK" & start_zone %in% lez$id)
  ) |
    !(start_time_min >= 0 & start_time_min < 6*60))

hh_metrics_Regio_w <- legs_weekend_hh_Regio %>%
  group_by(hhid, RegioStaR7) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

hh_mode_share_Regio_w <- legs_weekend_hh_Regio %>%
  group_by(hhid, RegioStaR7, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, RegioStaR7) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

hh_all_Regio_w <- hh_metrics_Regio_w %>%
  left_join(hh_mode_share_Regio_w, by = c("hhid", "RegioStaR7"))

hh_Regio_metrics_w <- hh_all_Regio_w %>%
  group_by(RegioStaR7) %>%
  summarise(
    mean_time_min       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs         = mean(n_legs, na.rm = TRUE),
    cv_total_time       = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time      = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time      = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time      = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25       = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | 
             starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share_{.col}"),
    n_hhs = n(),
    .groups = "drop"
  )

### Scenario 1
legs1_weekend_hh_Regio <- legs1_joined %>%
  filter(person_id %in% legs_weekend_hh_Regio$person_id) %>%
  filter(day %in% c(6, 7))

legs_weekend_hh_Regio <- legs_weekend_hh_Regio %>%
  semi_join(legs1_weekend_hh_Regio, by = "hhid")

hh1_metrics_Regio_w <- legs1_weekend_hh_Regio %>%
  group_by(hhid, RegioStaR7) %>%
  summarise(
    mean_time_min = mean(time_min, na.rm = TRUE),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    total_time_min = sum(time_min, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    n_legs = n(),
    .groups = "drop"
  )

hh1_mode_share_Regio_w <- legs1_weekend_hh_Regio %>%
  group_by(hhid, RegioStaR7, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, RegioStaR7) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(names_from = mode, values_from = share, values_fill = 0)

hh1_all_Regio_w <- hh1_metrics_Regio_w %>%
  left_join(hh1_mode_share_Regio_w, by = c("hhid", "RegioStaR7"))

hh1_Regio_metrics_w <- hh1_all_Regio_w %>%
  group_by(RegioStaR7) %>%
  summarise(
    mean_time_min1       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m1     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time1     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance1 = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs1         = mean(n_legs, na.rm = TRUE),
    cv_total_time        = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time       = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time       = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time       = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q25        = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") | 
             starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"), 
           ~ mean(.x, na.rm = TRUE), .names = "share1_{.col}"),
    n_hhs = n(),
    .groups = "drop"
  )

# Differences
hh_Regio_diff_w <- hh_Regio_metrics_w %>%
  inner_join(hh1_Regio_metrics_w, by = "RegioStaR7") %>%
  mutate(
    time_diff            = mean_total_time - mean_total_time1,
    time_mean_diff       = mean_time_min - mean_time_min1,
    distance_diff        = mean_total_distance - mean_total_distance1,
    distance_mean_diff   = mean_distance_m - mean_distance_m1,
    increase_time_total  = time_diff / mean_total_time,
    increase_time_mean   = time_mean_diff / mean_time_min,
    decrease_dist_total  = distance_diff / mean_total_distance,
    decrease_dist_mean   = distance_mean_diff / mean_distance_m,
    n_legs_diff          = mean_n_legs - mean_n_legs1,
    share_diff_CAR       = share_CAR_DRIVER - share1_CAR_DRIVER,
    share_diff_CAR_P     = share_CAR_PASSENGER - share1_CAR_PASSENGER,
    share_diff_WALK      = share_WALK - share1_WALK,
    share_diff_BIKE      = share_BIKE - share1_BIKE,
    share_diff_BUS       = share_BUS - share1_BUS,
    share_diff_TRAIN     = share_TRAIN - share1_TRAIN,
    share_diff_TRAM_METRO= share_TRAM_METRO - share1_TRAM_METRO
  )

write.csv(hh_Regio_diff_w, file = "hh_tt_dt_Regio_diff_weekend_20250924.csv", row.names=FALSE)

write.csv(hh_Regio_diff_w,file='eq_Regio_diff_tt_dt_modeshare.csv', row.names=FALSE)