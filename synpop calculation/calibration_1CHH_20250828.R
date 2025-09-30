library(readr)
library(dplyr)
library(tidyr)
library(mlogit)
library(purrr)
library(ggplot2)

# read in file with Munich's target shares and the regional target share of Munich's metropolitan area from 2022
Diesel_MUC_zones <- read.csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//Munich//diesel_euro_distribution_muc_zones_districts.csv", sep=";")
Diesel_MetroMUC <- read.csv("C://Users//tumme//Desktop//Diesel_2022_MetroMUC.csv", sep=";")
Diesel_MUC <- read.csv("C://Users//tumme//Desktop//Diesel_2022_MUC.csv", sep=";")
hh_synpop_10pct <- read.csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//hh_synpop_10pct_20250815_ST.csv", sep=";")

#combine Diesel_MUC and Diesel_MetroMuc to have all target values in one datafile
common_cols <- intersect(names(Diesel_MetroMUC), names(Diesel_MUC))
Diesel_MetroMUC <- merge(Diesel_MetroMUC, Diesel_MUC, by = common_cols, all = TRUE)
Diesel_MetroMUC$diesel_share <- as.numeric(gsub(",", ".", Diesel_MetroMUC$diesel_share))
Diesel_MetroMUC$EURO_4D <- as.numeric(gsub(",", ".", Diesel_MetroMUC$EURO_4D))
Diesel_MetroMUC$EURO_5D <- as.numeric(gsub(",", ".", Diesel_MetroMUC$EURO_5D))
Diesel_MetroMUC$EURO_6D <- as.numeric(gsub(",", ".", Diesel_MetroMUC$EURO_6D))

# ---------- Diesel choice calibration for one car households ------------------

# filter households only owning one car outside Munich
hh_synpop_10pct_1CHH <- hh_synpop_10pct %>%
  filter(JUR_NAME != 'Muenchen Landeshauptstadt') %>%
  filter(H_ANZAUTO == 1)
# distribution 1CHH-MCHH: 70%-30%
# distribution 2CHH-3CHH: 80%-20%

# filter households only owning multiple cars in Munich
hh_synpop_10pct_1CHH_MUC <- hh_synpop_10pct %>%
  filter(JUR_NAME == 'Muenchen Landeshauptstadt') %>%
  filter(H_ANZAUTO == 1)

# when predicting with the model, NAs were generated --> eliminate them from the input data
hh_synpop_10pct_1CHH <- na.omit(hh_synpop_10pct_1CHH)
hh_synpop_10pct_1CHH_MUC <- na.omit(hh_synpop_10pct_1CHH_MUC)

# merge diesel target values with the input population
hh_synpop_10pct_1CHH <- hh_synpop_10pct_1CHH %>%
  left_join(
    Diesel_MetroMUC %>% dplyr::select(geo_name, diesel_share),
    by = c("JUR_NAME" = "geo_name")
  )

hh_synpop_10pct_1CHH_MUC <- hh_synpop_10pct_1CHH_MUC %>%
  left_join(
    Diesel_MUC_zones %>% dplyr::select(id, diesel_share, geo_name),
    by = c("zone" = "id")
  )

# attention! zones sometimes belong so multiple districts! -> decide for one!
# only select one district per zone and household
hh_synpop_10pct_1CHH_MUC <- hh_synpop_10pct_1CHH_MUC %>%
  distinct(id, .keep_all = TRUE)

# adapt variables' names to merge the data sets
hh_synpop_10pct_1CHH_MUC <- hh_synpop_10pct_1CHH_MUC %>% rename(JUR_NAME_MUC = JUR_NAME)
hh_synpop_10pct_1CHH_MUC <- hh_synpop_10pct_1CHH_MUC %>% rename(JUR_NAME = geo_name)

# combine both data sets to have all Landkreise and Munichs districts in one data file
common_cols <- intersect(names(hh_synpop_10pct_1CHH), names(hh_synpop_10pct_1CHH_MUC))
merged_df <- merge(hh_synpop_10pct_1CHH, hh_synpop_10pct_1CHH_MUC, by = common_cols, all = TRUE)
test <- hh_synpop_10pct %>%
  filter(H_ANZAUTO == 1)
#check is passed: sum of observations in the merged data file equals the original data file
hh_synpop_10pct_1CHH <- merged_df

# create a row per car of each household
#hh_synpop_10pct_1CHH <- hh_synpop_10pct_1CHH %>%
#  uncount(H_ANZAUTO, .id = "car_rank")

# add H_ANZAUTO to the file again
hh_synpop_10pct_1CHH <- hh_synpop_10pct_1CHH %>%
  left_join(hh_synpop_10pct %>% dplyr::select(H_ANZAUTO, id),
            by = c("id")
  )

hh_synpop_10pct_1CHH$JUR_NAME_MUC %>% replace_na("nonMUC")

# --- Step 1: Compute base utility for diesel ----------------------------------
hh_synpop_10pct_1CHH <- hh_synpop_10pct_1CHH %>%
  mutate(
    base_util = predict(glm_Diesel_1CHH_implementation_final_gender_withoutmileage, newdata = ., type = "link")  )

# --- Step 2: Probabilistic diesel assignment for 10 seeds ---------------------
seeds <- 1:10
decision_list <- list()

for (s in seeds) {
  set.seed(123 + s)
  
  prob_mat <- matrix(rep(hh_synpop_10pct_1CHH$base_util, each = 1), ncol = 1)
  # Convert utility to probability
  prob_vec <- 1 / (1 + exp(-hh_synpop_10pct_1CHH$base_util))
  
  # Probabilistic diesel choices
  diesel_choice <- rbinom(nrow(hh_synpop_10pct_1CHH), 1, prob_vec)
  
  decision_list[[s]] <- hh_synpop_10pct_1CHH %>%
    mutate(diesel_choice = diesel_choice)
}

# --- Step 3: Calibration loop per seed ----------------------------------------
# Calibration parameters
tolerance <- 0.008
step <- 0.05
max_iter <- 300

calibrated_list <- list()

for (s in seeds) {
  hh_data <- decision_list[[s]]
  
  # Initialize constants per iteration
  lkr_constants_diesel <- hh_data %>%
    distinct(JUR_NAME) %>%
    mutate(constant = 0)
  
  for (i in 1:max_iter) {
    # Join constants
    hh_cal <- hh_data %>%
      left_join(lkr_constants_diesel, by = "JUR_NAME") %>%
      mutate(constant = ifelse(is.na(constant), 0, constant),
             util_adj = base_util + constant,
             prob_diesel = 1 / (1 + exp(-util_adj)))
    
    # Compute current regional shares and and the difference to the target share over the mean of all seeds
    regional_diesel <- hh_cal %>%
      group_by(JUR_NAME) %>%
      summarise(
        current = mean(prob_diesel, na.rm = TRUE),
        diesel_share = first(diesel_share),
        diff = diesel_share - current,
        .groups = "drop"
      )
    
    max_reg_diff <- max(abs(regional_diesel$diff))
    cat(sprintf("Seed %d | Iteration %d | Max regional diff: %.4f\n", s, i, max_reg_diff))
    
    if (max_reg_diff < tolerance) {
      cat(sprintf("Seed %d: Calibration complete after %d iterations\n", s, i))
      break
    }
    
    # Update constants
    lkr_constants_diesel <- lkr_constants_diesel %>%
      left_join(regional_diesel %>% select(JUR_NAME, diff), by = "JUR_NAME") %>%
      mutate(constant = constant + step * diff) %>%
      select(JUR_NAME, constant)
  }
  
  hh_cal <- hh_cal %>%
    rowwise() %>%
    mutate(
      diesel_choice_final = rbinom(1, 1, prob_diesel)
    ) %>%
    ungroup()
  
  calibrated_list[[s]] <- hh_cal
}

# --- Step 4: Decide for one dataset (seed) ------------------------------------

## pick the seed where regional targets are met best
## check if economic household status and household type align with the target distribution

# Combine all seeds into one big dataframe
calib_long <- map2_df(calibrated_list, 1:length(calibrated_list), 
                      ~mutate(.x, seed = .y))

# Regional fit per seed
region_scores <- calib_long %>%
  group_by(seed, JUR_NAME) %>%
  summarise(
    current_share = mean(diesel_choice_final, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    Diesel_MetroMUC, 
    by = c("JUR_NAME" = "geo_name")
  ) %>%
  mutate(diff = abs(current_share - diesel_share))

# Aggregate deviations per seed
seed_scores <- region_scores %>%
  group_by(seed) %>%
  summarise(
    mean_regional_diff = mean(diff, na.rm = TRUE),
    max_regional_diff  = max(diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add stability measures
  left_join(
    calib_long %>%
      group_by(seed) %>%
      summarise(
        sd_constant = sd(constant, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "seed"
  ) %>%
  # Rank based on fit and stability
  mutate(
    diff_z = scale(mean_regional_diff)) %>%
  arrange(max_regional_diff)

best_seed <- seed_scores$seed[1]
cat("Best seed by regional fit & stability:", best_seed, "\n")
print(seed_scores)

# update the dataframe to continue with the emission class assignment
hh_synpop_10pct_1CHH1 <- as.data.frame(calibrated_list[5])
# max regional difference is 0.00838 

# ---- calibration statistics: diesel share variance across 10 seeds
# Summarize calibration stability across seeds
calib_summary <- calib_long %>%
  group_by(JUR_NAME, seed) %>%
  summarise(
    mean_constant = mean(constant, na.rm = TRUE),
    diesel_share = mean(diesel_choice_final, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(JUR_NAME) %>%
  summarise(
    sd_diesel_share = sd(diesel_share, na.rm = TRUE),
    range_diesel_share = max(diesel_share, na.rm = TRUE) - min(diesel_share, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(calib_summary, aes(y = range_diesel_share)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of diesel share ranges across regions and seeds",
       y = "range") +
  theme_minimal()

ggplot(calib_summary, aes(y = sd_diesel_share)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of diesel's share standard deviation across regions and seeds",
       y = "standard deviation") +
  theme_minimal()

# ---------- EUR class calibration for diesel vehicles with 10 seeds -----------
hh_synpop_10pct_1CHH1 <- read.csv("C://Users//tumme//OneDrive - TUM//Dokumente//calibrated_Simulation_Data_from_R//calibration_output_1CHH_20250816//hh_synpop_10pct_1CHH_diesel_all_seeds_calibrated_20250816.csv")

# get all diesel cars
hh_synpop_10pct_1CHHD <- hh_synpop_10pct_1CHH1 %>%
  filter(seed == 5) %>%
  filter(diesel_choice_final == 1) %>%
  mutate(row_xd = row_number())

# Calibration parameters
seeds <- 1:10
tolerance <- 0.01
step <- 0.1
max_iter <- 200

# Alternatives
alternatives <- c("EUR4", "EUR5", "EUR6")

# --- Prepare data for prediction in mlogit format ---

hh_synpop_long <- hh_synpop_10pct_1CHHD %>%
  select(id, JUR_NAME, everything()) %>%
  crossing(EUR = alternatives) %>%
  arrange(id) %>%
  mutate(
    choice = NA,  
    EUR_5 = if_else(EUR == "EUR5", 1, 0),
    EUR_6 = if_else(EUR == "EUR6", 1, 0),
    A_GEW = 1
  )

# Convert matrix to data frame with ids
base_probs_df <- res %>%
  select(row_xd, EUR, utility, probability) %>% 
  pivot_wider(
    names_from = EUR,
    values_from = c(utility, probability),
    names_glue = "{.value}_{EUR}"
  )

# --- Join probabilities to household data ---
hh_synpop_10pct_1CHHD <- hh_synpop_10pct_1CHHD %>%
  left_join(base_probs_df, by = "row_xd")

hh_synpop_10pct_1CHHD <- hh_synpop_10pct_1CHHD %>% rename(base_EUR4 = utility_EUR4) 
hh_synpop_10pct_1CHHD <- hh_synpop_10pct_1CHHD %>% rename(base_EUR5 = utility_EUR5) 
hh_synpop_10pct_1CHHD <- hh_synpop_10pct_1CHHD %>% rename(base_EUR6 = utility_EUR6)

# --- Step 1: Create 10 seed-specific datasets and compute utilities ---
eur_datasets <- vector("list", length(seeds))

for (s in seq_along(seeds)) {
  set.seed(1000 + s)
  
  eur_datasets[[s]] <- hh_synpop_10pct_1CHHD %>%
    mutate(
      # calculate utilities out of probabilities
      util_EUR4 = base_EUR4,
      util_EUR5 = base_EUR5,
      util_EUR6 = base_EUR6
    )
}

names(eur_datasets) <- paste0("seed_", seeds)

# --- Step 2: Calibrate each seed-specific dataset ---
calib_results <- vector("list", length(seeds))

for (s in seq_along(seeds)) {
  cat("\n---- Calibrating seed", seeds[s], "----\n")
  
  hh_seed <- eur_datasets[[s]]
  
  # Initialize regional constants
  lkr_constants_eur <- hh_seed %>%
    distinct(JUR_NAME) %>%
    mutate(EUR4 = 0, EUR5 = 0, EUR6 = 0)
  
  for (i in 1:max_iter) {
    # Join constants
    hh_seed <- hh_seed %>%
      select(-any_of(c("EUR4", "EUR5", "EUR6",
                       "exp_EUR4", "exp_EUR5", "exp_EUR6",
                       "total_exp", "p_EUR4", "p_EUR5", "p_EUR6"))) %>%
      left_join(lkr_constants_eur, by = "JUR_NAME") %>%
      mutate(
        # Adjust utilities by constants
        util4_adj = util_EUR4 + EUR4,
        util5_adj = util_EUR5 + EUR5,
        util6_adj = util_EUR6 + EUR6,
        
        # probabilities
        exp_EUR4 = exp(util4_adj),
        exp_EUR5 = exp(util5_adj),
        exp_EUR6 = exp(util6_adj),
        total_exp = exp_EUR4 + exp_EUR5 + exp_EUR6,
        p_EUR4 = exp_EUR4 / total_exp,
        p_EUR5 = exp_EUR5 / total_exp,
        p_EUR6 = exp_EUR6 / total_exp
      )
    
    # Regional observed vs target
    regional_eur <- hh_seed %>%
      group_by(JUR_NAME) %>%
      summarise(
        obs_EUR4 = mean(p_EUR4),
        obs_EUR5 = mean(p_EUR5),
        obs_EUR6 = mean(p_EUR6),
        .groups = "drop"
      ) %>%
      left_join(
        Diesel_MetroMUC %>%
          select(geo_name, target_EUR4 = EURO_4D,
                 target_EUR5 = EURO_5D, target_EUR6 = EURO_6D),
        by = c("JUR_NAME" = "geo_name")
      ) %>%
      mutate(
        diff_EUR4 = target_EUR4 - obs_EUR4,
        diff_EUR5 = target_EUR5 - obs_EUR5,
        diff_EUR6 = target_EUR6 - obs_EUR6
      )
    
    # Convergence check
    max_reg_diff <- max(abs(c(regional_eur$diff_EUR4,
                              regional_eur$diff_EUR5,
                              regional_eur$diff_EUR6)))
    cat(sprintf("Iteration %d | Max regional diff: %.6f\n", i, max_reg_diff))
    
    if (max_reg_diff < tolerance) {
      cat("Calibration complete after", i, "iterations\n")
      break
    }
    
    # Update constants
    lkr_constants_eur <- lkr_constants_eur %>%
      left_join(regional_eur %>% select(JUR_NAME, diff_EUR4, diff_EUR5, diff_EUR6),
                by = "JUR_NAME") %>%
      mutate(
        EUR4 = EUR4 + step * diff_EUR4,
        EUR5 = EUR5 + step * diff_EUR5,
        EUR6 = EUR6 + step * diff_EUR6
      ) %>%
      select(JUR_NAME, EUR4, EUR5, EUR6)
  }
  
  # After convergence: make probabilistic assignment
  hh_seed <- hh_seed %>%
    rowwise() %>%
    mutate(
      EUR_choice_updated = sample(
        c("EUR4", "EUR5", "EUR6"),
        size = 1,
        prob = c(p_EUR4, p_EUR5, p_EUR6)
      )
    ) %>%
    ungroup()
  
  # Store calibrated dataset
  calib_results[[s]] <- hh_seed
}

names(calib_results) <- paste0("seed_", seeds)


# --- Step 3: Evaluate seeds ---
eur_eval <- map2_df(calib_results, seeds, ~{
  .x %>%
    group_by(JUR_NAME) %>%
    summarise(
      EUR4_share = mean(EUR_choice_updated == "EUR4"),
      EUR5_share = mean(EUR_choice_updated == "EUR5"),
      EUR6_share = mean(EUR_choice_updated == "EUR6"),
      .groups = "drop"
    ) %>%
    mutate(seed = .y)
})

eur_eval <- eur_eval %>%
  left_join(
    Diesel_MetroMUC %>%
      select(JUR_NAME = geo_name,
             target_EUR4 = EURO_4D,
             target_EUR5 = EURO_5D,
             target_EUR6 = EURO_6D),
    by = "JUR_NAME"
  ) %>%
  mutate(
    diff_EUR4 = EUR4_share - target_EUR4,
    diff_EUR5 = EUR5_share - target_EUR5,
    diff_EUR6 = EUR6_share - target_EUR6,
    max_diff = pmax(abs(diff_EUR4), abs(diff_EUR5), abs(diff_EUR6))
  )


# Aggregate per seed: worst-case deviation across regions
seed_scores <- eur_eval %>%
  group_by(seed) %>%
  summarise(
    max_diff_overall = max(max_diff),
    mean_diff_overall = mean(max_diff),
    .groups = "drop"
  ) %>%
  arrange(max_diff_overall)

seed_scores
# best seed is 4 with max 0.070 difference

hh_synpop_10pct_1CHHD1 <- as.data.frame(calib_results[4])
names(hh_synpop_10pct_1CHHD1) <- sub('seed_4.', '', names(hh_synpop_10pct_1CHHD1))

# --- prepare evaluation statistics to compare the seeds -----------------------
eur_calib <- hh_synpop_10pct_1CHHD1 %>%
  group_by(JUR_NAME) %>%
  summarise(
    EUR4_share = mean(EUR_choice_updated == "EUR4", na.rm = TRUE),
    EUR5_share = mean(EUR_choice_updated == "EUR5", na.rm = TRUE),
    EUR6_share = mean(EUR_choice_updated == "EUR6", na.rm = TRUE),
    .groups = "drop"
  )

# --- compare the deviation / variance across the seeds ------------------------
# Summarize EUR shares per region & seed
calib_long2 <- map2_df(calib_results, 1:length(calib_results), 
                       ~mutate(.x, seed = .y))

eur_summary <- calib_long2 %>%
  group_by(seed, JUR_NAME) %>%
  summarise(
    EUR4_share = mean(EUR_choice_updated == "EUR4", na.rm = TRUE),
    EUR5_share = mean(EUR_choice_updated == "EUR5", na.rm = TRUE),
    EUR6_share = mean(EUR_choice_updated == "EUR6", na.rm = TRUE),
    .groups = "drop"
  )

eur_variance <- eur_summary %>%
  group_by(JUR_NAME) %>%
  summarise(
    mean_EUR4 = mean(EUR4_share),
    sd_EUR4   = sd(EUR4_share),
    range_EUR4= max(EUR4_share) - min(EUR4_share),
    mean_EUR5 = mean(EUR5_share),
    sd_EUR5   = sd(EUR5_share),
    range_EUR5= max(EUR5_share) - min(EUR5_share),
    mean_EUR6 = mean(EUR6_share),
    sd_EUR6   = sd(EUR6_share),
    range_EUR6= max(EUR6_share) - min(EUR6_share),
    .groups = "drop"
  )

ggplot(eur_variance, aes(y = range_EUR4)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of EURO_4 share ranges across regions and seeds",
       y = "range") +
  theme_minimal()

ggplot(eur_variance, aes(y = range_EUR5)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of EURO_5 share ranges across regions and seeds",
       y = "range") +
  theme_minimal()

ggplot(eur_variance, aes(y = range_EUR6)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of EURO
       _6 share ranges across regions and seeds",
       y = "range") +
  theme_minimal()

ggplot(eur_variance, aes(y = sd_EUR4)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of EURO_4's share standard deviation across regions and seeds",
       y = "standard deviation") +
  theme_minimal()

ggplot(eur_variance, aes(y = sd_EUR5)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of EURO_5's share standard deviation across regions and seeds",
       y = "standard deviation") +
  theme_minimal()

ggplot(eur_variance, aes(y = sd_EUR6)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  labs(title = "Distribution of EURO_6's share standard deviation across regions and seeds",
       y = "standard deviation") +
  theme_minimal()


# --- create outputfiles -------------------------------------------------------

# create data file with calibrated nonDiesel and Diesel cars
hh_synpop_10pct_1CHHnD <- hh_synpop_10pct_1CHH1 %>%
  filter(seed == 5) %>%
  filter(diesel_choice == 0)

common_cols <- intersect(names(hh_synpop_10pct_1CHHD1), names(hh_synpop_10pct_1CHHnD))
merged_1CHHDnD <- merge(hh_synpop_10pct_1CHHD1, hh_synpop_10pct_1CHHnD, by = common_cols, all = TRUE)
merged_1CHHDnD <- merged_1CHHDnD %>% rename(emissionClass = EUR_choice_updated)
merged_1CHHDnD <- merged_1CHHDnD %>%
  mutate(emissionClass = replace_na(emissionClass, "EUROx"))
merged_1CHHDnD$emissionClass <- factor(merged_1CHHDnD$emissionClass, 
                                       levels = c("EUR4", "EUR5", "EUR6", "EUROx"),
                                       labels = c("EURO_4", "EURO_5", "EURO_6", "EUROx"))
merged_1CHHDnD$diesel_choice <- factor(merged_1CHHDnD$diesel_choice_final, 
                                       levels = c(0,1),
                                       labels = c("nonDIESEL", "DIESEL"))

# write calibrated population and calibration parameters to csv files
write.csv(merged_1CHHDnD, file = "hh_synpop_10pct_1CHH_20250830.csv", row.names = FALSE)

write.csv(calib_long, file = "hh_synpop_10pct_1CHH_diesel_all_seeds_calibrated_20250816.csv", row.names = FALSE)
write.csv(lkr_constants_diesel, file = "Diesel_constants_1CHH_20250816.csv", row.names = FALSE)
write.csv(regional_diesel, file = "Diesel_1CHH_target_calibrated_distribution_probs_20250816.csv", row.names = FALSE)
write.csv(region_scores, file = "Diesel_1CHH_target_calibrated_distribution_selectedseed5_20250816.csv", row.names = FALSE)

write.csv(calib_long2, file = "hh_synpop_10pct_1CHH_EUR_all_seeds_calibrated_20250816.csv", row.names = FALSE)
write.csv(lkr_constants_eur, file = "EUR_constants_1CHH_20250828.csv", row.names = FALSE)
write.csv(regional_eur, file = "EUR_1CHH_target_calibrated_distribution_probs_20250828.csv", row.names = FALSE)
write.csv(eur_eval, file = "EUR_1CHH_target_calibrated_distribution_selectedseed8_20250816.csv", row.names = FALSE)

write.csv(calib_summary, file = "Diesel_1CHH_calibration_stats_20250816.csv", row.names = FALSE)
write.csv(eur_variance, file = "EUR_1CHH_calibration_stats__20250816.csv", row.names = FALSE)
