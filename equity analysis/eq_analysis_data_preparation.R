library(data.table)
library(readr)
library(psych)
library(dplyr)
library(ggplot2)

# --- read in all necessary data files ----

# read in synthetic population files
pp <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//ABIT_input_20250830//pp_synpop_10pct_20250830_ST_income_adapted.csv")
hh <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//ABIT_input_20250830//hh_synpop_10pct_20250830_ST.csv")
vv <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//ABIT_input_20250830//vv_2022_20250830_ST.csv")
lez <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//lowEmissionZones_MittlererRing.csv")
hh_adapted <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//ABIT_input_20250830//hh_synpop_10pct_Cars_calibrated_20250830.csv")

# tours and legs from the base scenario
tours <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//equity_analysis//20250918_base//tours.csv")
legs <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//equity_analysis//20250918_base//legs.csv")

# tours and legs from scenario 1 (extended ban scenario)
tours1 <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//equity_analysis//20250918_Sc1//tours.csv")
legs1 <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//equity_analysis//20250918_Sc1//legs.csv")

# --- check, if sum of legs in tours is equivalent to number of legs in legs per person ----
#Count the number of legs per person in the 'legs' file
legs_count <- legs1 %>%
  summarise(legs_count = n())

#Count the sum of legs per person in the 'tours' file 
tour_legs_sum <- tours1 %>%
  summarise(tour_legs_sum = sum(num_legs))  

# --> subtours are responsible for the non_equivalent number of legs and sum of legs in tours
# --> remove subtours from legs

# --- check, if travel times and travel times are reasonable for each mode
# check speed plausibility for all modes
speed_mode <- legs1 %>%
  mutate(
    speed = case_when(
      time_min == 0 ~ (distance_m / 1000) / (1 / 60), # km/h
      TRUE             ~ (distance_m / 1000) / (time_min / 60) # km/h
    )
  ) %>%
  group_by(mode) %>%
  summarise(
    mean_speed = mean(speed, na.rm = TRUE),
    q1_speed   = quantile(speed, 0.25, na.rm = TRUE),
    q3_speed   = quantile(speed, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

print(speed_mode)

# legs from base scenario
#   mode          mean_speed q1_speed q3_speed
#   <chr>              <dbl>    <dbl>    <dbl>
#1 BIKE                62.5    42.5      71.8
#2 BUS                 21.4     7.90     21.7
#3 CAR_DRIVER          57.9    39.8      68.4
#4 CAR_PASSENGER       57.3    40.1      68.7
#5 TRAIN               18.5     7.64     25.3
#6 TRAM_METRO          15.1     9.64     19.6
#7 WALK                58.6    33.1      70.4

# legs from scenario 1
#   mode          mean_speed q1_speed q3_speed
#   <chr>              <dbl>    <dbl>    <dbl>
#1 BIKE                62.4    42.5      71.8
#2 BUS                 21.2     7.87     21.8
#3 CAR_DRIVER          57.8    39.5      68.3
#4 CAR_PASSENGER       57.3    39.8      68.7
#5 TRAIN               18.6     7.70     25.6
#6 TRAM_METRO          15.0     9.49     19.6
#7 WALK                58.4    33.0      70.3

# --- speed travel times for BIKE and WALK need to be recalculated

# --- complete legs and tours files with all necessary data for a join ----
# recalculate travel times for bike and walk
legs <- legs %>%
  filter(previous_purpose != "SUBTOUR", next_purpose != "SUBTOUR") %>%
  mutate(
    time_min = case_when(
      mode == "BIKE" ~ (distance_m / 1000) / 10 * 60,
      mode == "WALK" ~ (distance_m / 1000) / 4 * 60,
      TRUE ~ time_min   
    )
  )

legs1 <- legs1 %>%
  filter(previous_purpose != "SUBTOUR", next_purpose != "SUBTOUR") %>%
  mutate(
    time_min = case_when(
      mode == "BIKE" ~ (distance_m / 1000) / 10 * 60,
      mode == "WALK" ~ (distance_m / 1000) / 4 * 60,
      TRUE ~ time_min   
    )
  )

# add day to both leg files
legs <- legs %>%
  mutate(
    day = start_time_min %/% 1440 + 1,         # 1 = Monday
    time_of_day_min = start_time_min %% 1440   # minutes within day
  )

legs1 <- legs1 %>%
  mutate(
    day = start_time_min %/% 1440 + 1,         # 1 = Monday
    time_of_day_min = start_time_min %% 1440   # minutes within day
  )

# add household information to the leg file
legs <- legs %>%
  left_join(pp %>% dplyr::select(id, hhid, disability, homeZone),
            by = c("person_id" = "id")) %>%
  left_join(hh %>% dplyr::select(id, autos, hhSize),
            by = c("hhid" = "id")) %>%
  left_join(
    hh_adapted %>%
      dplyr::select(hhid = id, oek_status_gr, RegioStaR2, RegioStaR4, RegioStaR7) %>%
      distinct(hhid, .keep_all = TRUE),
    by = "hhid"
  ) %>%
  mutate(
    in_LEZ = end_zone %in% lez$id,
    home_in_LEZ = homeZone %in% lez$id
  )

legs1 <- legs1 %>%
  left_join(pp %>% dplyr::select(id, hhid, disability, homeZone),
            by = c("person_id" = "id")) %>%
  left_join(hh %>% dplyr::select(id, autos, hhSize),
            by = c("hhid" = "id")) %>%
  left_join(
    hh_adapted %>%
      dplyr::select(hhid = id, oek_status_gr, RegioStaR2, RegioStaR4, RegioStaR7) %>%
      distinct(hhid, .keep_all = TRUE),
    by = "hhid"
  ) %>%
  mutate(
    in_LEZ = end_zone %in% lez$id,
    home_in_LEZ = homeZone %in% lez$id
  )

# add vehicle information to the tours
tours_veh <- tours %>%
  left_join(vv %>% dplyr::select(index, emissionClass),
            by = c("vehicle_id" = "index")) 

tours_veh1 <- tours1 %>%
  left_join(vv %>% dplyr::select(index, emissionClass),
            by = c("vehicle_id" = "index"))

# --- filter all targeted households and persons ----

# filter all households with at least one EURO5 diesel vehicle
hh_with_euro5 <- vv %>%
  filter(emissionClass %in% c("EURO_5")) %>%
  distinct(id)

# filter all persons having access to a EURO 5 vehicle in their household
# filter all persons having a drivers license from these households
# filter if their destination is at least once in the LEZ and their home outside the LEZ
targeted_persons <- pp %>%
  filter(hhid %in% hh_with_euro5$id,
         driversLicense == TRUE) %>%
  semi_join(
    legs %>% filter(in_LEZ == TRUE,
                    home_in_LEZ == FALSE),
    by = c("id" = "person_id")
  )

# --- join the targeted legs and tours for the analysis ----

# --- join legs and tours from the base scenario ----
# filter legs belonging to the targeted persons
legs_filt <- legs %>%
  filter(person_id %in% targeted_persons$id)

# convert legs and tour files to data.tables for faster calculation
legs_dt  <- as.data.table(legs_filt)
tours_dt <- as.data.table(tours_veh)

# add tour_id to legs based on tour time windows (start and end time)
setDT(legs_dt)
setDT(tours_dt)

# Prepare legs with interval
legs_dt[, leg_id := .I]
legs_int <- legs_dt[, .(leg_id, person_id, mode,
                        start = start_time_min,
                        end   = end_time)]

# Prepare tours with interval
tours_int <- tours_dt[, .(person_id, mode = tour_mode, tour_id, num_legs,
                          start = tour_start_time_min,
                          end   = tour_end_time_min)]

# Keys: person_id + mode + interval (start, end)
setkey(legs_int, person_id, mode, start, end)
setkey(tours_int, person_id, mode, start, end)

# Overlap join
candidates <- foverlaps(legs_int, tours_int, nomatch = 0L, type = "any")

# Keep exactly n_legs legs per tour
candidates <- candidates[order(person_id, tour_id, i.start)]
candidates <- candidates[, head(.SD, num_legs[1]), by = .(person_id, tour_id)]

# Bring back to legs_dt
legs_dt[candidates, on = .(leg_id), tour_id := i.tour_id]

legs_joined <- legs_dt %>%
  left_join(
    tours_veh,
    by = c("person_id", "day", "tour_id"),
    suffix = c("_leg", "_tour")
  )

# --- join legs and tours from scenario 1 ----
# filter legs1 belonging to the targeted persons
legs1_filt <- legs1 %>%
  filter(person_id %in% legs_filt$person_id)

legs1_dt  <- as.data.table(legs1_filt)
tours1_dt <- as.data.table(tours_veh1)

# add tour_id to legs based on tour time windows (start and end time)
setDT(legs1_dt)
setDT(tours1_dt)

# Prepare legs with interval
legs1_dt[, leg_id := .I]
legs1_int <- legs1_dt[, .(leg_id, person_id, mode,
                        start = start_time_min,
                        end   = end_time)]

# Prepare tours with interval
tours1_int <- tours1_dt[, .(person_id, mode = tour_mode, tour_id, num_legs,
                          start = tour_start_time_min,
                          end   = tour_end_time_min)]

# Keys: person_id + mode + interval (start, end)
setkey(legs1_int, person_id, mode, start, end)
setkey(tours1_int, person_id, mode, start, end)

# Overlap join
candidates1 <- foverlaps(legs1_int, tours1_int, nomatch = 0L, type = "any")

# Keep exactly n_legs legs per tour
candidates1 <- candidates1[order(person_id, tour_id, i.start)]
candidates1 <- candidates1[, head(.SD, num_legs[1]), by = .(person_id, tour_id)]

# Bring back to legs_dt
legs1_dt[candidates1, on = .(leg_id), tour_id := i.tour_id]

legs1_joined <- legs1_dt %>%
  left_join(
    tours_veh1,
    by = c("person_id", "day", "tour_id"),
    suffix = c("_leg", "_tour")
  )

# --- create week-day and week-end data sets for both scenarios ----

# split into week-day data set and week-end data set
euro5_persons <- legs_joined %>%
  filter(mode == "CAR_DRIVER") %>% # take only persons who really use a EURO_5 vehicle to enter the low emission zone
  filter(emissionClass == "EURO_5") %>%
  filter(disability != "PHYSICAL") %>% # persons with physical abilities are allowed to enter the LEZ with a non-compliant car --> exclude them from the analysis
  filter(in_LEZ == TRUE) %>%
  filter(home_in_LEZ == FALSE) 

legs_week <- legs_joined %>%
  filter(person_id %in% euro5_persons$person_id) %>% # take only persons who really use a EURO_5 vehicle to enter the low emission zone
  filter(day != 6) %>%
  filter(day != 7) %>%
  filter(!(
    (next_purpose == "WORK" & end_zone %in% lez$id) |
      (previous_purpose == "WORK" & start_zone %in% lez$id)
  ) |
    !(start_time_min >= 0 & start_time_min < 6*60)) # remove all night-shift working trips related to the LEZ

legs1_week <- legs1_joined %>%
  filter(person_id %in% euro5_persons$person_id) %>% # take only persons who really use a EURO_5 vehicle to enter the low emission zone
  filter(day != 6) %>%
  filter(day != 7) %>%
  filter(!(
    (next_purpose == "WORK" & end_zone %in% lez$id) |
      (previous_purpose == "WORK" & start_zone %in% lez$id)
  ) |
    !(start_time_min >= 0 & start_time_min < 6*60)) # remove all night-shift working trips related to the LEZ

legs_weekend <- legs_joined %>%
  filter(person_id %in% euro5_persons$person_id,
         day %in% c(6, 7)) %>%
  filter(!(
    (next_purpose == "WORK" & end_zone %in% lez$id) |
      (previous_purpose == "WORK" & start_zone %in% lez$id)
  ) |
    !(start_time_min >= 0 & start_time_min < 6*60))

legs1_weekend <- legs1_joined %>%
  filter(person_id %in% legs_weekend$person_id) %>% #make sure that here are the same persons than in the base scenario
  filter(day %in% c(6, 7))

length(unique(legs_week$person_id))
length(unique(legs1_week$person_id))

# check if filtered persons are consistent between the two scnearios
p_base <- legs_week %>%
  distinct(person_id)
p_1 <- legs1_week %>%
  distinct(person_id)
common_persons <- p_base %>%
  filter(person_id %in% p_1$person_id)
p_base <- legs_weekend %>%
  distinct(person_id)
p_1 <- legs1_weekend %>%
  distinct(person_id)
common_persons <- p_base %>%
  filter(person_id %in% p_1$person_id)
# there are 9 persons more in the base scenario comapred to the scenario 1 for weekday trips
# there are 14 persons more in the base scenario compared to the scenario 1 for weekend trips

# filter base scenario so that in both scenarios are the same persons
legs_week <- legs_week %>%
  semi_join(legs1_week, by = "person_id")
legs_weekend <- legs_weekend %>%
  semi_join(legs1_weekend, by = "person_id")

# --- consistency check ----
# check, if there are inconsistencies (non-car useres have an emission-class, leg_mode != tour_mode)
check <- legs_joined %>%
  filter(mode != "CAR_DRIVER")
table(check$emissionClass)

inconsistencies <- legs_joined %>%
  filter(mode != tour_mode)


# --- general descriptive statistics of the simulation output ----
# read in Munich's zones
zonesMunich <- read.csv("Z://Masterarbeit//Data_Research//diesel_euro_distribution_muc_zones_districts.csv", sep=";")

#Base Scenario
modeshare_muc_base <- legs %>%
  filter(homeZone %in% zonesMunich$id)

prop.table(table(modeshare_muc_base$mode))
#      BIKE           BUS    CAR_DRIVER CAR_PASSENGER         TRAIN    TRAM_METRO WALK
#0.16038554    0.05270989    0.24531137    0.08859693    0.07788184    0.20482740 0.17028703

modeshare_nonmuc_base <- legs %>%
  dplyr::filter(!homeZone %in% zonesMunich$id)

prop.table(table(modeshare_nonmuc_base$mode))
#      BIKE           BUS    CAR_DRIVER CAR_PASSENGER         TRAIN    TRAM_METRO        WALK 
#0.11082788    0.03657730    0.49433050    0.16337572    0.06880712    0.02001524  0.10606625 

#Scenario 1
modeshare_muc_sc1 <- legs1 %>%
  filter(homeZone %in% zonesMunich$id)

prop.table(table(modeshare_muc_sc1$mode))
#      BIKE           BUS    CAR_DRIVER CAR_PASSENGER         TRAIN    TRAM_METRO         WALK 
#0.15803125    0.05387653    0.25203809    0.08936763    0.07901719    0.20291607   0.16475323

modeshare_nonmuc_sc1 <- legs1 %>%
  dplyr::filter(!homeZone %in% zonesMunich$id)

prop.table(table(modeshare_nonmuc_sc1$mode))
#      BIKE           BUS    CAR_DRIVER CAR_PASSENGER         TRAIN    TRAM_METRO       WALK
#0.10943726    0.03628866    0.50064322    0.16163505    0.06905076    0.02030690 0.10263815 

# calculate number and share of targeted people and households
#total people: 195842
length(unique(legs$person_id))
#euro5_targeted persons: 2782
length(unique(euro5_persons$person_id))
#share 1.4%

#total: EUR_5 holders
length(unique(hh_with_euro5$id))

#total households: 93024
length(unique(legs$hhid))
#euro5_targeted households by the LEZ: 1575
length(unique(euro5_persons$hhid))
#share 1.69%

# calculate number and share of targeted trips
targeted_legs <- legs_joined %>%
  filter(mode=="CAR_DRIVER") %>%
  filter(emissionClass=="EURO_5") %>%
  filter(start_zone %in% lez$id | end_zone %in% lez$id) %>%
  filter(home_in_LEZ == FALSE)
#5509 targeted trips

# calculate how many origin destination trips were kept in scenario 1 with the same persons
# keep OD + extra info in both datasets
targeted_od <- targeted_legs %>%
  distinct(person_id, start_x, start_y, end_x, end_y, next_purpose, mode, emissionClass, oek_status_gr, RegioStaR7, autos)

legs1_od <- legs1_joined %>%
  distinct(person_id, start_x, start_y, end_x, end_y, next_purpose, mode, emissionClass, oek_status_gr, RegioStaR7, autos)

# only by OD + person_id, but keep all columns
matched_od <- targeted_od %>%
  inner_join(
    legs1_od,
    by = c("person_id", "start_x", "start_y", "end_x", "end_y"),
    suffix = c("_base", "_scenario1")   # distinguish columns
  )

# counts
n_matches <- nrow(matched_od)
n_total   <- nrow(targeted_od)
prop_matched <- n_matches / n_total

n_matches
n_total
prop_matched # 15%

#instead of definging od trips only by exact coordinates, use zones
targeted_od <- targeted_legs %>%
  distinct(person_id, start_zone, end_zone, next_purpose, mode, emissionClass)

legs1_od <- legs1_joined %>%
  distinct(person_id, start_zone, end_zone, next_purpose, mode, emissionClass)

# match only by OD + person_id, but keep all columns
matched_od <- targeted_od %>%
  inner_join(
    legs1_od,
    by = c("person_id", "start_zone", "end_zone", "next_purpose"),
    suffix = c("_base", "_scenario1")   
  )

# counts
n_matches <- nrow(matched_od)
n_total   <- nrow(targeted_od)
prop_matched <- n_matches / n_total

n_matches
n_total
prop_matched #16%

#get distribtution of trip purpose of targeted trips
prop.table(table(matched_od$next_purpose))
#ACCOMPANY   EDUCATION        HOME       OTHER    SHOPPING        WORK 
#0.011111111 0.003703704 0.472839506 0.002469136 0.009876543 0.500000000 

#get mode share of targeted trips in scenario 1
prop.table(table(matched_od$mode_scenario1))
#       BIKE           BUS    CAR_DRIVER CAR_PASSENGER         TRAIN    TRAM_METRO WALK 
#0.156790123   0.019753086   0.169135802   0.011111111   0.325925926   0.309876543 0.007407407

#get economic and residential location of the targeted households
# make sure each person appears only once with their attributes
unique_households <- targeted_legs %>%
  distinct(household_id, oek_status_gr, RegioStaR7)

# distribution of economic status
dist_oek <- unique_households %>%
  count(oek_status_gr) %>%
  mutate(share = n / sum(n))

# distribution of regional type
dist_region <- unique_households %>%
  count(RegioStaR7) %>%
  mutate(share = n / sum(n))

dist_oek
dist_region

#person level
#> dist_oek
#    oek_status_gr     n     share
#           <char> <int>     <num>
#1:           high   725 0.4349130
#2:         medium   538 0.3227355
#3:      very high   364 0.2183563
#4: very low - low    40 0.0239952
#> dist_region
#   RegioStaR7     n      share
#        <num> <int>      <num>
#1:         71   752 0.45110978
#2:         72     2 0.00119976
#3:         73   601 0.36052789
#4:         74   189 0.11337732
#5:         75     4 0.00239952
#6:         76    41 0.02459508
#7:         77    78 0.04679064


#household level
#> dist_oek
#   oek_status_gr     n      share
#           <char> <int>      <num>
#1:           high   631 0.43577348
#2:         medium   465 0.32113260
#3:      very high   312 0.21546961
#4: very low - low    40 0.02762431
#> dist_region
#   RegioStaR7     n       share
#        <num> <int>       <num>
#1:         71   606 0.418508287
#2:         72     2 0.001381215
#3:         73   539 0.372237569
#4:         74   179 0.123618785
#5:         75     4 0.002762431
#6:         76    41 0.028314917
#7:         77    77 0.053176796


# get distribution of oek_status from persons replacing the trip with a newer vehicle
veh_replacer <- matched_od %>%
  filter(mode_scenario1 == "CAR_DRIVER")
prop.table(table(veh_replacer$oek_status_gr_scenario1))
#high         medium      very high very low - low 
#  67             13             39              1 
#       high         medium      very high very low - low 
#0.558333333    0.108333333    0.325000000    0.008333333
prop.table(table(veh_replacer$RegioStaR7_scenario1))
#71 73 74 76 77 
#57 52  7  2  2 
#        71         73         74         76         77 
#0.47500000 0.43333333 0.05833333 0.01666667 0.01666667 

non_veh_replacer <- matched_od %>%
  filter(mode_scenario1 != "CAR_DRIVER")
table(non_veh_replacer$oek_status_gr_scenario1)
prop.table(table(non_veh_replacer$oek_status_gr_scenario1))
#high         medium      very high very low - low 
# 322            121            173             11 
#      high         medium      very high very low - low 
#0.51355662     0.19298246     0.27591707     0.01754386

#print all targeted households to visualize their home location
write.csv(euro5_persons, file = "targeted_persons_households.csv", row.names = FALSE)

# --- targeted trips where destination change occurred ----
# OD pairs in targeted but NOT in legs1
non_matching_targeted <- targeted_od %>%
  anti_join(
    legs1_od,
    by = c("person_id", "start_x", "start_y", "end_x", "end_y", "next_purpose")
    #by = c("person_id", "start_zone", "end_zone", "next_purpose")
  )

# OD pairs in legs1 but NOT in targeted
non_matching_legs1 <- legs1_od %>%
  anti_join(
    targeted_od,
    by = c("person_id", "start_x", "start_y", "end_x", "end_y", "next_purpose")
    #by = c("person_id", "start_zone", "end_zone", "next_purpose")
  )
