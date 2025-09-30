# Coefficients for EUR5 and EUR6
coef_EUR5 <- c(
  "(Intercept):EUR5" = -0.612592,
  "I(EUR_5 * bus_500)" = -0.056062,
  "I(EUR_5 * higher_education)" = 0.123730,
  "oek_status_grmedium:EUR5" = 0.277674,
  "oek_status_grhigh:EUR5" = 0.539983,
  "oek_status_grvery_high:EUR5" = 0.680740,
  "hhgr_gr33_or_more_persons:EUR5" = -0.093464,
  "RegioStaR2urban_region:EUR5" = 0.124666,
  "non_working:EUR5" = -0.209545
)

coef_EUR6 <- c(
  "(Intercept):EUR6" = -1.812197,
  "I(EUR_6 * family)" = 0.287679,
  "oek_status_grmedium:EUR6" = 0.606863,
  "oek_status_grhigh:EUR6" = 1.251568,
  "oek_status_grvery_high:EUR6" = 1.732423,
  "hhgr_gr33_or_more_persons:EUR6" = -0.239028,
  "RegioStaR2urban_region:EUR6" = 0.185217,
  "non_working:EUR6" = -0.369910
)


compute_utilities_1st <- function(long_df) {
  
  # Initialize utility
  long_df$utility <- 0
  
  # ---- EUR4 ---- (reference alternative: utility = 0)
  # already set to 0 above
  
  # ---- EUR5 ----
  idx5 <- long_df$EUR == "EUR5"
  long_df$utility[idx5] <- 
    coef_EUR5["(Intercept):EUR5"] +
    ifelse(long_df$bus_500[idx5] == 1, coef_EUR5["I(EUR_5 * bus_500)"], 0) +
    ifelse(long_df$higher_education[idx5] == 1, coef_EUR5["I(EUR_5 * higher_education)"], 0) +
    ifelse(long_df$oek_status_gr[idx5] == "medium", coef_EUR5["oek_status_grmedium:EUR5"], 0) +
    ifelse(long_df$oek_status_gr[idx5] == "high", coef_EUR5["oek_status_grhigh:EUR5"], 0) +
    ifelse(long_df$oek_status_gr[idx5] == "very high", coef_EUR5["oek_status_grvery_high:EUR5"], 0) +
    ifelse(long_df$hhgr_gr3[idx5] == "3 or more persons", coef_EUR5["hhgr_gr33_or_more_persons:EUR5"], 0) +
    ifelse(long_df$RegioStaR2[idx5] == "urban region", coef_EUR5["RegioStaR2urban_region:EUR5"], 0) +
    ifelse(long_df$non_working[idx5] == 1, coef_EUR5["non_working:EUR5"], 0)
  
  # ---- EUR6 ----
  idx6 <- long_df$EUR == "EUR6"
  long_df$utility[idx6] <- 
    coef_EUR6["(Intercept):EUR6"] +
    ifelse(long_df$family[idx6] == 1, coef_EUR6["I(EUR_6 * family)"], 0) +
    ifelse(long_df$oek_status_gr[idx6] == "medium", coef_EUR6["oek_status_grmedium:EUR6"], 0) +
    ifelse(long_df$oek_status_gr[idx6] == "high", coef_EUR6["oek_status_grhigh:EUR6"], 0) +
    ifelse(long_df$oek_status_gr[idx6] == "very high", coef_EUR6["oek_status_grvery_high:EUR6"], 0) +
    ifelse(long_df$hhgr_gr3[idx6] == "3 or more persons", coef_EUR6["hhgr_gr33_or_more_persons:EUR6"], 0) +
    ifelse(long_df$RegioStaR2[idx6] == "urban region", coef_EUR6["RegioStaR2urban_region:EUR6"], 0) +
    ifelse(long_df$non_working[idx6] == 1, coef_EUR6["non_working:EUR6"], 0)
  
  # ---- Probabilities ----
  long_df <- long_df %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(probability = exp(utility) / sum(exp(utility))) %>%
    dplyr::ungroup()
  
  return(long_df)
}

long_first <- hh_synpop_long %>%
  filter(car_rank==1)

res <- compute_utilities_1st(long_first)

write.csv(res, file = "mlogitfirst_20250829.csv", row.names = FALSE)
