# Coefficients for EUR5 and EUR6
coef_EUR5 <- c(
  "(Intercept):EUR 5"	=	-1.073589,
  "I(EUR_5 * single_HH)"	=	-0.276063,
  "I(EUR_5 * young_household)"	=	-0.229813,
  "oek_status_grmedium:EUR 5"	=	0.656111,
  "oek_status_grhigh:EUR 5"	=	0.921487,
  "oek_status_grvery high:EUR 5"	=	0.936593,
  "family:EUR 5"	=	-0.155751,
  "bahn28_impgr2more than 500 m:EUR 5"	=	0.152639,
  "full_time_working:EUR 5"	=	0.167079
)

coef_EUR6 <- c(
  "(Intercept):EUR 6" = -2.763431	,
  "oek_status_grmedium:EUR 6" = 0.987691,
  "oek_status_grhigh:EUR 6" = 1.503567,
  "oek_status_grvery high:EUR 6" = 1.791743,
  "family:EUR 6" = -0.121493,
  "bahn28_impgr2more than 500 m:EUR 6" = 0.425931,
  "full_time_working:EUR 6" = 0.430819
)


compute_utilities <- function(long_df) {
  
  # Initialize utility
  long_df$utility <- 0
  
  # ---- EUR4 ---- (reference alternative: utility = 0)
  # already set to 0 above
  
  # ---- EUR5 ----
  idx5 <- long_df$EUR == "EUR5"
  long_df$utility[idx5] <- 
    coef_EUR5["(Intercept):EUR 5"] +
    ifelse(long_df$single_HH[idx5] == 1, coef_EUR5["I(EUR_5 * single_HH)"], 0) +
    ifelse(long_df$young_household[idx5] == 1, coef_EUR5["I(EUR_5 * young_household)"], 0) +
    ifelse(long_df$oek_status_gr[idx5] == "medium", coef_EUR5["oek_status_grmedium:EUR 5"], 0) +
    ifelse(long_df$oek_status_gr[idx5] == "high", coef_EUR5["oek_status_grhigh:EUR 5"], 0) +
    ifelse(long_df$oek_status_gr[idx5] == "very high", coef_EUR5["oek_status_grvery high:EUR 5"], 0) +
    ifelse(long_df$family[idx5] == 1, coef_EUR5["family:EUR 5"], 0) +
    ifelse(long_df$bahn28_impgr2[idx5] == "more than 2500 m", coef_EUR5["bahn28_impgr2more than 500 m:EUR 5"], 0) +
    ifelse(long_df$full_time_working[idx5] == 1, coef_EUR5["full_time_working:EUR 5"], 0)
  
  # ---- EUR6 ----
  idx6 <- long_df$EUR == "EUR6"
  long_df$utility[idx6] <- 
    coef_EUR6["(Intercept):EUR 6"] +
    ifelse(long_df$oek_status_gr[idx6] == "medium", coef_EUR6["oek_status_grmedium:EUR 6"], 0) +
    ifelse(long_df$oek_status_gr[idx6] == "high", coef_EUR6["oek_status_grhigh:EUR 6"], 0) +
    ifelse(long_df$oek_status_gr[idx6] == "very high", coef_EUR6["oek_status_grvery high:EUR 6"], 0) +
    ifelse(long_df$family[idx6] == 1, coef_EUR6["family:EUR 6"], 0) +
    ifelse(long_df$bahn28_impgr2[idx6] == "more than 2500 m", coef_EUR6["bahn28_impgr2more than 500 m:EUR 6"], 0) +
    ifelse(long_df$full_time_working[idx6] == 1, coef_EUR6["full_time_working:EUR 6"], 0)
  
  # ---- Probabilities ----
  long_df <- long_df %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(probability = exp(utility) / sum(exp(utility))) %>%
    dplyr::ungroup()
  
  return(long_df)
}

res <- compute_utilities(hh_synpop_long)
setwd("C://Users//tumme//OneDrive - TUM//Dokumente//R_Skripte")
write.csv(res, file = "mlogit1CHH_20250829.csv", row.names = FALSE)
