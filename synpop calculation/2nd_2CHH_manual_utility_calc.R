
coef_EUR5_2nd <- c(
  "(Intercept):EUR 5"	=	-0.946137,
  "I(EUR_5 * bus_500)"	=	-0.132211,
  "oek_status_grmedium:EUR 5"	=	0.218904,
  "oek_status_grhigh:EUR 5"	=	0.502404,
  "oek_status_grvery high:EUR 5" =	0.744775
)


coef_EUR6_2nd <- c(
  "(Intercept):EUR 6"	=	-2.311079	,
  "I(EUR_6 * only_adult_household)"	=	-0.18801,
  "I(EUR_6 * higher_education)"	=	0.166813,
  "I(EUR_6 * part_time_working)"	=	-0.31384,
  "oek_status_grmedium:EUR 6"	=	0.797083,
  "oek_status_grhigh:EUR 6"	=	1.1886,
  "oek_status_grvery high:EUR 6"	=	1.535892
)


# function to compute utilities & probabilities
compute_utilities_2nd <- function(long_df) {
  
  # initialize utility column
  long_df$utility <- 0
  
  # ---- EUR4 ---- (reference alternative: utility = 0)
  # already set to 0 above
  
  # ---- EUR5 ----
  idx5 <- long_df$EUR == "EUR5"
  long_df$utility[idx5] <- 
    coef_EUR5_2nd[["(Intercept):EUR 5"]] +
    ifelse(long_df[["bus_500"]][idx5] == 1, coef_EUR5_2nd["I(EUR_5 * bus_500)"], 0) +
    ifelse(long_df[["oek_status_gr"]][idx5] == "medium",    coef_EUR5_2nd["oek_status_grmedium:EUR 5"], 0) +
    ifelse(long_df[["oek_status_gr"]][idx5] == "high",      coef_EUR5_2nd["oek_status_grhigh:EUR 5"], 0) +
    ifelse(long_df[["oek_status_gr"]][idx5] == "very high", coef_EUR5_2nd["oek_status_grvery high:EUR 5"], 0)
  
  # ---- EUR6 ----
  idx6 <- long_df$EUR == "EUR6"
  long_df$utility[idx6] <- 
    coef_EUR6_2nd[["(Intercept):EUR 6"]] +
    ifelse(long_df[["only_adult_household"]][idx6] == 1, coef_EUR6_2nd["I(EUR_6 * only_adult_household)"], 0) +
    ifelse(long_df[["higher_education"]][idx6] == 1, coef_EUR6_2nd["I(EUR_6 * higher_education)"], 0) +
    ifelse(long_df[["part_time_working"]][idx6] == 1, coef_EUR6_2nd["I(EUR_6 * part_time_working)"], 0) +
    ifelse(long_df[["oek_status_gr"]][idx6] == "medium", coef_EUR6_2nd["oek_status_grmedium:EUR 6"], 0) +
    ifelse(long_df[["oek_status_gr"]][idx6] == "high",      coef_EUR6_2nd["oek_status_grhigh:EUR 6"], 0) +
    ifelse(long_df[["oek_status_gr"]][idx6] == "very high", coef_EUR6_2nd["oek_status_grvery high:EUR 6"], 0)
  
  # ---- Probabilities ----
  long_df <- long_df %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(probability = exp(utility) / sum(exp(utility))) %>%
    dplyr::ungroup()
  
  return(long_df)
}

long_other <- hh_synpop_long %>%
  filter(car_rank!=1)

res2 <- compute_utilities_2nd(long_other)

write.csv(res2, file = "mlogitsecond_20250829.csv", row.names = FALSE)
