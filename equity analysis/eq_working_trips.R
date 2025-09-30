### equity analysis of working and non-working trips

# --- working trips during weekdays (Monday - Friday) ----
### Base Scenario
# Household-level metrics by WORK (only consider households whose working location is in the lez)
# get households where at least one member drives with a EUR5 car into the LEZ for work purposes
hh_eur5_workers <- legs_week_hh %>%
  # keep only HOME - WORK relations
  filter(
    (next_purpose == "WORK" & previous_purpose == "HOME") |
      (previous_purpose == "WORK" & next_purpose == "HOME")
  ) %>%
  #filter(person_id %in% hh1_metrics_work$eid) %>%
  group_by(hhid) %>%
  # keep only households where at least one leg is CAR_DRIVER + EURO_5 + entering/exiting LEZ
  filter(any(mode == "CAR_DRIVER" & emissionClass == "EURO_5" &
               (start_zone %in% lez$id | end_zone %in% lez$id))) %>%
  ungroup()


hh_metrics_work <- hh_eur5_workers %>%
  group_by(hhid, oek_status_gr) %>%
  summarise(
    mean_time_min     = mean(time_min, na.rm = TRUE),
    mean_distance_m   = mean(distance_m, na.rm = TRUE),
    total_time_min    = sum(time_min, na.rm = TRUE),
    total_distance_m  = sum(distance_m, na.rm = TRUE),
    n_legs            = n(),
    .groups = "drop"
  )

# Household-level mode shares (only households where at least one member has working trips inside the lez with a diesel EURO_5 car)
hh_mode_share_work <- hh_eur5_workers %>%
  group_by(hhid, oek_status_gr, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, oek_status_gr) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(
    names_from = mode,
    values_from = share,
    values_fill = 0
  )

# Combine household-level metrics + mode shares
hh_all_work <- hh_metrics_work %>%
  left_join(hh_mode_share_work,
            by = c("hhid", "oek_status_gr"))

# Aggregate to oek_status_gr + next_purpose
hh_oek_status_metrics_work <- hh_all_work %>%
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
    
    # Mode shares averaged across persons
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") |
             starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"),
           ~ mean(.x, na.rm = TRUE), .names = "share_{.col}"),
    
    n_hh = n(),
    .groups = "drop"
  )  


### Scenario1
# Household-level metrics by next_purpose
hh1_metrics_work <- legs1_week_hh %>%
  filter(hhid %in% hh_eur5_workers$hhid) %>%
  filter(
    (next_purpose == "WORK" & previous_purpose == "HOME") |
      (previous_purpose == "WORK" & next_purpose == "HOME")
  ) %>%
  group_by(hhid) %>%
  # keep only households where at least one leg is CAR_DRIVER + EURO_5 + entering/exiting LEZ
  filter(start_zone %in% lez$id | end_zone %in% lez$id) %>%
  ungroup() %>%
  group_by(hhid, oek_status_gr) %>%
  summarise(
    mean_time_min     = mean(time_min, na.rm = TRUE),
    mean_distance_m   = mean(distance_m, na.rm = TRUE),
    total_time_min    = sum(time_min, na.rm = TRUE),
    total_distance_m  = sum(distance_m, na.rm = TRUE),
    n_legs            = n(),
    .groups = "drop"
  )

length(unique(hh1_metrics_work$person_id))
length(unique(hh_eur5_workers$person_id))

# Household-level mode shares (wide format, by next_purpose)
hh1_mode_share_work <- legs1_week_hh %>%
  filter(hhid %in% hh_eur5_workers$hhid) %>%
  filter(
    (next_purpose == "WORK" & previous_purpose == "HOME") |
      (previous_purpose == "WORK" & next_purpose == "HOME")
  ) %>%
  group_by(hhid) %>%
  # keep only households where at least one leg is entering/exiting LEZ
  filter(start_zone %in% lez$id | end_zone %in% lez$id) %>%
  ungroup() %>%
  group_by(hhid, oek_status_gr, mode) %>%
  summarise(n_mode = n(), .groups = "drop") %>%
  group_by(hhid, oek_status_gr) %>%
  mutate(share = n_mode / sum(n_mode)) %>%
  dplyr::select(-n_mode) %>%
  tidyr::pivot_wider(
    names_from = mode,
    values_from = share,
    values_fill = 0
  )

# Combine household-level metrics + mode shares
hh1_all_work <- hh1_metrics_work %>%
  left_join(hh1_mode_share_work,
            by = c("hhid", "oek_status_gr"))

# Aggregate to oek_status_gr + next_purpose
hh_oek_status1_metrics_work <- hh1_all_work %>%
  group_by(oek_status_gr) %>%
  summarise(
    mean_time_min1       = mean(mean_time_min, na.rm = TRUE),
    mean_distance_m1     = mean(mean_distance_m, na.rm = TRUE),
    mean_total_time1     = mean(total_time_min, na.rm = TRUE),
    mean_total_distance1 = mean(total_distance_m, na.rm = TRUE),
    mean_n_legs1         = mean(n_legs, na.rm = TRUE),
    
    # Dispersion metrics
    cv_total_time1       = sd(total_time_min, na.rm = TRUE) / mean(total_time_min, na.rm = TRUE),
    q25_total_time1      = quantile(total_time_min, 0.25, na.rm = TRUE),
    q50_total_time1      = quantile(total_time_min, 0.50, na.rm = TRUE),
    q75_total_time1      = quantile(total_time_min, 0.75, na.rm = TRUE),
    ratio_q75_q251       = quantile(total_time_min, 0.75, na.rm = TRUE) /
      quantile(total_time_min, 0.25, na.rm = TRUE),
    
    # Mode shares averaged across persons
    across(starts_with("CAR") | starts_with("WALK") | starts_with("BIKE") |
             starts_with("BUS") | starts_with("TRAIN") | starts_with("TRAM_METRO"),
           ~ mean(.x, na.rm = TRUE), .names = "share1_{.col}"),
    
    n_hh = n(),
    .groups = "drop"
  )

hh_metrics_diff_work <- hh_oek_status_metrics_work %>%
  inner_join(hh_oek_status1_metrics_work, by = "oek_status_gr") %>%
  mutate(time_diff = mean_total_time - mean_total_time1) %>%
  mutate(time_mean_diff = mean_time_min - mean_time_min1) %>%
  mutate(distance_diff = mean_total_distance - mean_total_distance1) %>%
  mutate(distance_mean_diff = mean_distance_m - mean_distance_m1) %>%
  mutate(increase_time_total = time_diff / mean_total_time) %>%
  mutate(increase_time_mean = time_mean_diff / mean_time_min) %>%
  mutate(decrease_dist_total = distance_diff / mean_total_distance) %>%
  mutate(decrease_dist_mean = distance_mean_diff / mean_distance_m) %>%
  mutate(n_legs_diff = mean_n_legs - mean_n_legs1) #%>%
  #mutate(share_diff_CAR = share_CAR_DRIVER - share1_CAR_DRIVER) %>%
  #mutate(share_diff_CAR_P = share_CAR_PASSENGER - share1_CAR_PASSENGER) %>%
  #mutate(share_diff_WALK = share_WALK - share1_WALK) %>%
  #mutate(share_diff_BIKE = share_BIKE - share1_BIKE) %>%
  #mutate(share_diff_BUS = share_BUS - share1_BUS) %>%
  #mutate(share_diff_TRAIN = share_TRAIN - share1_TRAIN) %>%
  #mutate(share_diff_TRAM_METRO = share_TRAM_METRO - share1_TRAM_METRO)

EUROx_users1 <- legs1_week %>%
  filter(person_id %in% person_metrics_work$person_id) %>%
  filter(next_purpose == "WORK") %>%
  filter(in_LEZ == TRUE) %>%
  filter(mode == "CAR_DRIVER") %>%
  group_by(person_id, oek_status_gr, emissionClass) %>%
  summarise(n_EUR = n(), .groups = "drop") %>%
  group_by(person_id, oek_status_gr) %>%
  mutate(share = n_EUR / sum(n_EUR)) %>%
  select(-n_EUR) %>%
  tidyr::pivot_wider(
    names_from = emissionClass,
    values_from = share,
    values_fill = 0
  ) %>%
  group_by(oek_status_gr) %>%
  summarize(
    across(starts_with("EUROx") | starts_with("EURO_6"),
           ~ mean(.x, na.rm = TRUE), .names = "share_{.col}")
  )

# --- OD match approach for working trips ----
#base scenario
working_od <- legs_week_hh %>%
  filter(hhid %in% hh_eur5_workers$hhid) %>%
  #filter(oek_status_gr == "very low - low") %>%
  filter(previous_purpose == "WORK" | next_purpose == "WORK") %>% # 
  filter(start_zone %in% lez$id | end_zone %in% lez$id) #%>%
  #distinct(person_id, start_x, start_y, end_x, end_y, next_purpose, .keep_all = TRUE)

# scenario 1
working1_od <- legs1_week_hh %>%
  filter(hhid %in% hh_eur5_workers$hhid) %>%
  #filter(oek_status_gr == "very low - low") %>%
  filter(previous_purpose == "WORK" | next_purpose == "WORK") %>% # 
  filter(start_zone %in% lez$id | end_zone %in% lez$id) #%>% 
  #distinct(person_id, start_x, start_y, end_x, end_y, next_purpose, .keep_all = TRUE)

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

write.csv(matched_trips, file = "OD_working_trips_diff_oek.csv", row.names = FALSE)

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
    oek_status_gr     = first(oek_status_gr_base),
    RegioStaR7        = first(RegioStaR7_base),
    autos             = first(autos_base),
    
    .groups = "drop"
  )

write.csv(hh_matched_metrics, file = "OD_working_trips_stats_oek.csv", row.names = FALSE)

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

oek_group_matched_summary <- hh_matched_metrics %>%
  group_by(oek_status_gr) %>%
  summarise(
    mean_time_base  = mean(mean_time_min, na.rm = TRUE),
    mean_time_scen1 = mean(mean_time_min1, na.rm = TRUE),
    mean_diff       = mean_time_base - mean_time_scen1,
    .groups = "drop"
  )


# --- Grouping to oek_status_gr ---
hh_matched_group <- hh_matched_diff %>%
  group_by(oek_status_gr) %>%
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

# why is there a household loss from base scenario to scenario 1?
person_available <- legs1 %>%
  filter(person_id == "3207369")
# in scenario 1, this person does not work anymore, although this person worked in the base scenario

# check oek_status_gr distribution among working trips
table(matched_trips$oek_status_gr_base)
prop.table(table(matched_trips$oek_status_gr_base))

# check how many households replaced their car with another car
check_veh_change <- matched_trips %>%
  filter(mode_base == "CAR_DRIVER") %>%
  filter(mode_scenario1 == "CAR_DRIVER") %>%
  filter(emissionClass_base != emissionClass_scenario1)
length(unique(check_veh_change$hhid_base)) #23
# check how many households switched to other modes
check_mode_change <- matched_trips %>%
  filter(mode_base == "CAR_DRIVER") %>%
  filter(mode_scenario1 != "CAR_DRIVER")
length(unique(check_mode_change$hhid_base)) #182
#check how many households with EURO5 cars do not use cars to go to work
check_nonEURO5users_work <- matched_trips %>%
  filter(mode_base != "CAR_DRIVER")
length(unique(check_nonEURO5users_work$hhid_base)) #176

length(unique(matched_trips$hhid_scenario1)) #336
length(unique(legs_week$hhid)) #1361

prop.table(table(check_veh_change$oek_status_gr_base))
prop.table(table(check_veh_change$emissionClass_scenario1))
prop.table(table(check_veh_change$oek_status_gr_base))

p_1057227 <- legs_week_hh %>%
  filter(person_id == 1057227)
