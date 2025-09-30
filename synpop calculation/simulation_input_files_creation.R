library(readr)
library(dplyr)

## --- create input files for the simulation -----------------------------------
# --- combine the synpop of the single and multiple car households--------------
df0 <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//pp_synpop_10pct_20250815_ST_nonCarOwners.csv")
df1 <- read.csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//calibration_output_1CHH_20250830//hh_synpop_10pct_1CHH_20250830.csv")
df2 <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//calibration_output_2CHH_20250830//hh_synpop_10pct_2CHH_20250830.csv")
common_cols <- intersect(names(df1), names(df2))
merged_df <- merge(df1, df2, by = common_cols, all = TRUE)
write.csv(merged_df, file = "hh_synpop_10pct_Cars_calibrated_20250830.csv", row.names = FALSE)


### prepare the car_data_file in the right format for the simulation
# Add car id per household starting from 0
df3 <- merged_df %>%
  group_by(id) %>%
  mutate(car_id_within_household = row_number() - 1) %>%
  ungroup()

vehicles <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//vv_2022_20231229.csv")

merged_veh <- vehicles %>%
  inner_join(df3 %>%
              select(id, car_id_within_household, emissionClass, diesel_choice),
            by = c("id", "vehId" = "car_id_within_household"))

merged_veh <- na.omit(merged_veh)
merged_veh <- merged_veh[ , !(names(merged_veh) == "type")]
merged_veh <- merged_veh %>% rename(type = diesel_choice)

write.csv(merged_veh, file = "vv_2022_20250830_ST.csv", row.names = FALSE)

emission_shares <- merged_df %>%
  filter(emissionClass != "EUROx") %>%
  group_by(JUR_NAME, emissionClass) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

emission_shares_wide <- merged_df %>%
  filter(emissionClass != "EUROx") %>%
  group_by(JUR_NAME, emissionClass) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(share = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(
    names_from = emissionClass,
    values_from = share,
    names_prefix = "share_"
  ) %>%
  ungroup()

emission_shares_wide <- emission_shares_wide %>%
  left_join(Diesel_MetroMUC %>% select(geo_name, EURO_4D, EURO_5D, EURO_6D), by = c("JUR_NAME" = "geo_name"))

emission_shares_wide <- emission_shares_wide %>%
  mutate(diff_EUR4 = share_EURO_4 - EURO_4D) %>%
  mutate(diff_EUR5 = share_EURO_5 - EURO_5D) %>%
  mutate(diff_EUR6 = share_EURO_6 - EURO_6D) 

write.csv(emission_shares_wide, file = "EURO_shares_calibdiff_20250830_ST.csv", row.names = FALSE)

### prepare the hh and pp data_file in the right format for the simulation
hh <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//hh_2022_20231229.csv")
pp <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//pp_2022_20231229.csv")
#vv <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//Calibrated_Simulation_Data_from_R//vv_2022_20250830_ST.csv")
vv <- merged_veh

hh <- read_csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//hh_synpop_10pct_20250816_ST.csv")
pp <- read_csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//pp_synpop_10pct_20250816_ST.csv")

hh <- hh %>%
  filter(id %in% df0$hhid | id %in% vv$id)

pp <- pp %>%
  filter(hhid %in% df0$hhid | hhid %in% vv$id)

# Ensure no household with autos > 0 is missing in vv:
#hh <- hh %>% filter(!(autos > 0 & !(id %in% vv$id)))
#pp <- pp %>% filter(!(hhid %in% hh$id))

#check filtering
hh_v <- hh %>%
  filter(autos > 0)
sum(unique(hh_v$id) %in% vv$id)
sum(unique(vv$id) %in% hh_v$id)
sum(unique(pp$hhid) %in% vv$id)
unique(vv$id)

#check number of cars per household distribution full synpop and calibrated synpop
#0          1          2          3 
#0.19364088 0.55247524 0.20488622 0.04899766 
#0.22077859 0.49514183 0.22915974 0.05491983

write.csv(pp, file = "pp_synpop_10pct_20250830_ST.csv", row.names = FALSE)
write.csv(hh, file = "hh_synpop_10pct_20250830_ST.csv", row.names = FALSE)

# --- create extra adapted income column for pp
## adapt income distribution because currently it does not represent the monthly net income
# https://stadt.muenchen.de/dam/jcr:52aeb1bd-840e-45a3-91dd-62503582dfef/Expertise_Reichtum_und_Verteilung_Muenchen_2022_final.pdf
pp_synpop <- pp %>%
  filter(income != 0)

# define target values
median_income <- 2700
target_props <- c(0.19, 0.43, 0.26, 0.12)
bounds <- c(0, 0.6, 1.2, 2.0, Inf)

# optimize sigma
loss_fun <- function(sigma) {
  mu <- log(median_income)
  x <- rlnorm(5e5, meanlog = mu, sdlog = sigma)
  breaks_abs <- bounds * median_income
  props <- as.numeric(prop.table(table(cut(x, breaks = breaks_abs, include.lowest = TRUE))))
  sum((props - target_props)^2)
}

opt <- optim(par = 0.5, fn = loss_fun, method = "Brent", lower = 0.05, upper = 2)
opt_sigma <- opt$par

# map pp_synpop
mu <- log(median_income)
n <- length(pp_synpop$income)
u <- rank(pp_synpop$income, ties.method = "random") / (n + 1)

# map ranks on the optimized target distribution
income_mapped <- qlnorm(u, meanlog = mu, sdlog = opt_sigma)

# check mapping
hist(income_mapped, breaks = 50, main = "Quantile Mapped", xlab = "Income")
breaks_abs <- bounds * median_income
group <- cut(income_mapped, breaks = breaks_abs, include.lowest = TRUE)
round(prop.table(table(group)), 4)

pp_synpop <- pp_synpop %>%
  mutate(income_adapted = income_mapped)

pp_income_adapted <- pp %>%
  left_join(pp_synpop %>% select(id, income_adapted), by = "id")

pp_income_adapted <- pp %>%
  left_join(pp_synpop %>% select(id, income_adapted), by = "id") %>%
  mutate(income_adapted = round(income_adapted, 0)) %>%
  mutate(income_adapted = replace_na(income_adapted, 0))

write.csv(pp_income_adapted, file = "pp_synpop_10pct_20250830_ST_income_adapted.csv", row.names = FALSE)

# --- load created files for simulation
pp <- read_csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//pp_synpop_10pct_20250816_ST.csv")
hh <- read_csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//hh_synpop_10pct_20250816_ST.csv")
vv <- read_csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//vv_2022_20250816_ST.csv")

# --- calculate average person statistics for the logsum calculation -----------
ppE4 <- merged_veh %>%
  filter(emissionClass == "EURO_4")

ppE5 <- merged_veh %>%
  filter(emissionClass == "EURO_5")

ppE6 <- merged_veh %>%
  filter(emissionClass == "EURO_6")

ppEx <- merged_veh %>%
  filter(emissionClass == "EUROx")

pE4 <- pp_income_adapted %>%
  filter(hhid %in% ppE4$id) %>%
  filter(driversLicense == TRUE)

pE5 <- pp_income_adapted %>%
  filter(hhid %in% ppE5$id) %>%
  filter(driversLicense == TRUE)

pE6 <- pp_income_adapted %>%
  filter(hhid %in% ppE6$id) %>%
  filter(driversLicense == TRUE)

pEx <- pp_income_adapted %>%
  filter(hhid %in% ppEx$id) %>%
  filter(driversLicense == TRUE)

library(dplyr)
library(purrr)

calculate_shares <- function(df) {
  # First, compute household-level metrics
  hh_stats <- df %>%
    group_by(hhid) %>%
    summarise(
      has_children = any(relationShip == "child"),
      hh_size = n(),
      .groups = "drop"
    )
  
  # Then, compute individual-level metrics
  individual_stats <- df %>%
    mutate(
      age_group = case_when(
        age <= 18 ~ "age_0_18",
        age <= 29 ~ "age_19_29",
        age <= 49 ~ "age_30_49",
        age <= 59 ~ "age_50_59",
        age <= 69 ~ "age_60_69",
        TRUE ~ "age_70"
      ),
      income_group = case_when(
        income_adapted <= 1500 ~ "income_0_1500",
        income_adapted <= 5600 ~ "income_1501_5600",
        TRUE ~ "income_5601"
      ),
      gender = case_when(
        gender == 1 ~ "male",
        gender == 2 ~ "female"
      ),
      mobilityRestricted = case_when(
        disability == "WITHOUT" ~ 0,
        disability %in% c("PHYSICAL", "MENTAL") ~ 1
      )
    ) %>%
    summarise(
      female_share = mean(gender == "female"),
      male_share = mean(gender == "male"),
      mobilityRestricted_share = mean(mobilityRestricted),
      age_0_18 = mean(age_group == "age_0_18"),
      age_19_29 = mean(age_group == "age_19_29"),
      age_30_49 = mean(age_group == "age_30_49"),
      age_50_59 = mean(age_group == "age_50_59"),
      age_60_69 = mean(age_group == "age_60_69"),
      age_70 = mean(age_group == "age_70"),
      income_0_1500 = mean(income_group == "income_0_1500"),
      income_1501_5600 = mean(income_group == "income_1501_5600"),
      income_5601 = mean(income_group == "income_5601")
    )
  
  # Combine with household-level metrics
  hh_summary <- hh_stats %>%
    summarise(
      share_with_children = mean(has_children),
      average_household_size = mean(hh_size)
    )
  
  cbind(individual_stats, hh_summary)
}

# Apply to a list of datasets
datasets <- list(pE4, pE5, pE6, pEx)
shares_list <- map(datasets, calculate_shares)
shares_combined <- bind_rows(shares_list, .id = "dataset")
shares_combined

write.csv(shares_combined, file = "logsum_average_person_age_incomeADAPTED_gender_mobrestric_20250830_ST.csv", row.names = FALSE)
