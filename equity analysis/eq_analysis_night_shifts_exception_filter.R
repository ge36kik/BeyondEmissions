# --- get all people using their EURO_5 car only for work at night in the LEZ ----

# Filter legs for EURO_5 trips in LEZ
euro5_legs <- legs_joined %>%
  filter(mode == "CAR_DRIVER",
         emissionClass == "EURO_5",
         home_in_LEZ == FALSE)

# Define "night work trips"
night_work_legs <- euro5_legs %>%
  filter(
    (next_purpose == "WORK" & end_zone %in% lez$id) |
      (previous_purpose == "WORK" & start_zone %in% lez$id)
  ) %>%
  filter(start_time_min >= 0 & start_time_min < 6*60)  # between 00:00–06:00

# Persons who use EURO_5 at night for work at least once a week
persons_night_workers <- night_work_legs %>%
  group_by(person_id, day) %>%              
  summarise(n_trips = n(), .groups = "drop") %>%
  filter(n_trips >= 1) %>%
  distinct(person_id)

# Check if these persons ONLY used EURO_5 for night work
# Compare their total EURO_5 trips vs their night_work trips
person_usage <- euro5_legs %>%
  filter(person_id %in% persons_night_workers$person_id) %>%
  group_by(person_id) %>%
  summarise(
    total_euro5_trips = n(),
    night_work_trips  = sum((next_purpose == "WORK" | previous_purpose == "WORK") &
                              (start_time_min >= 0*60 | start_time_min < 6*60)),
    .groups = "drop"
  ) %>%
  mutate(only_night_work = (total_euro5_trips == night_work_trips))

# filter persons who only use their EURO_5 to access or egress WORK in the LEZ at night
person_usage_night <- person_usage %>%
  filter(only_night_work == TRUE)
