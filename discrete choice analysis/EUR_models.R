install.packages(c("readr", "dplyr", "tidyr", "factoextra", "mlogit", "caret", "car", "DescTools", "ggplot2"))

library(readr)
library(dplyr)
library(tidyr)
library(factoextra)
library(DescTools)
library(mlogit)
library(caret)
library(car)
library(ggplot2)

plot_data <- read_csv("Z://Masterarbeit//plot_data.csv")
plot_data <- read_csv("C://Users//tumme//Documents//plot_data.csv")
  
  
################################################################################
###modeling data preparation
##create model data depending on the number of cars per household
##differentiate between
#- households owning only one car
#- the most driven car of households owning 2-5 cars
#- the lesser driven cars of households owning 2-5 cars

model_data <- plot_data %>%
  filter(Diesel == 'Diesel') %>% # filter only diesel car, since the predictors show stronger tendencies for diesel cars
  filter(A_BAUJ <= 2017) %>% # filter out cars without any information about the manufacturing year
  #filter(hhtyp2 != "other") %>%
  
  # filtering for one car households (comment all filtering lines for 2-5CHHs)
  filter(H_ANZAUTO <= 1) %>% 
  filter(H_GR <= 7) #filter since from H_GR > 7, not all choices are represented for one car households
  
  # filtering for 2-5 car households (comment the filtering line for 1CHHs)
  #filter(H_ANZAUTO <= 5 & H_ANZAUTO > 1) %>%
  #filter(H_GR <= 9) %>% #filter since from H_GR > 8, not all choices are represented for multiple car households
  #group_by(H_ID) %>%
  
  # from these households filter the most driven car
  #filter(A_JAHRESFL_imp == max(A_JAHRESFL_imp)) %>%
  
  # from these households filter the lesser driven cars
  #arrange(desc(A_JAHRESFL_imp)) %>%
  #mutate(rank = row_number()) %>%
  #filter(rank != 1) %>%
  #select(-rank) %>%
  #ungroup()

# create a dummy variable for company car, since it is relevant, 
# >90% of the data in A_HALTER contains no information --> the share of company car is too small to impute it
# filtering leads to a too high data loss
model_data$company_car <- ifelse(model_data$A_HALTER =="company car",1,0)

# create dummy variable for diesel
model_data$diesel_bin <- ifelse(model_data$Diesel =="Diesel",1,0)

# create dummy variables for HH_occupation
model_data$full_time_working <- ifelse(model_data$HH_occupation == "full time working",1,0)
model_data$part_time_working <- ifelse(model_data$HH_occupation == "part time working",1,0)
model_data$non_working <- ifelse(model_data$HH_occupation == "no one is working",1,0)

# create dummy variables for different HH types
model_data$single_from_18_to_29 <- ifelse(model_data$hhtyp == "1P/18-29y",1,0)
model_data$single_older_60 <- ifelse(model_data$hhtyp == "1P/+60y",1,0)
model_data$single_parent <- ifelse(model_data$hhtyp == "Single Parent",1,0)
model_data$young_household <- ifelse(model_data$hhtyp2 == "young:<35",1,0)
model_data$family <- ifelse(model_data$hhtyp2 == "family",1,0)
model_data$only_adult_household <- ifelse(model_data$hhtyp2 == "only adults",1,0)
model_data$elderly_people_household <- ifelse(model_data$hhtyp2 == "old:+65",1,0)

# aggregate levels for distance to train station
model_data$bahn28_imp <- factor(model_data$bahn28_imp, 
                                  levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'))
                                  
model_data$bahn28_impgr <- factor(model_data$bahn28_imp, 
                                        levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                        labels = c('< 250 m', '250 - <1000 m', '250 - <1000 m', '1000 m - <2500 m', 'more than 2500 m', 'more than 2500 m'))

model_data$bahn28_impgr2 <- factor(model_data$bahn28_imp, 
                                  levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                  labels = c('up to 500 m', 'up to 500 m', 'more than 500 m', 'more than 500 m', 'more than 500 m', 'more than 500 m'))

model_data$bahn28_impgr3 <- factor(model_data$bahn28_imp, 
                                   levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                   labels = c('less than 1000 m', 'less than 1000 m', 'less than 1000 m', 'more than 1000 m', 'more than 1000 m', 'more than 1000 m'))

model_data$bahn28_impgr_more_5000 <- ifelse(model_data$bahn28_imp==">= 5000 m",1,0)

# aggregate levels for distance to bus station
model_data$bus28_imp <- factor(model_data$bus28_imp, 
                                 levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'))

model_data$bus28_impgr <- factor(model_data$bus28_imp, 
                                       levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                       labels = c('up to 500 m', 'up to 500 m', 'more than 500 m', 'more than 500 m', 'more than 500 m', 'more than 500 m'))

model_data$bus28_impgr_more_1000 <- ifelse(model_data$bus28_impgr=="more than 1000 m",1,0)

# aggregate levels of or dummy code RegioStaR7 and level it correctly
model_data$RegioStaR7 <- factor(model_data$RegioStaR7, 
                                levels = c("urban: metropolis", "urban: regiopolis", "urban: medium-sized town", "urban: village area", 
                                           "rural: central city", "rural: medium-sized city", "rural: village area"))
                                
model_data$RegioStaR7_gr <- factor(model_data$RegioStaR7, 
                                      levels = c("urban: metropolis", "urban: regiopolis", "urban: medium-sized town", "urban: village area", 
                                                 "rural: central city", "rural: medium-sized city", "rural: village area"),
                                      labels = c("urban: metropolis-regiopolis", "urban: metropolis-regiopolis", "urban: medium-sized town", "urban: village area", 
                                                 "rural: central city", "rural: medium-sized city", "rural: village area"))

model_data$RegioStaR7_gr2 <- factor(model_data$RegioStaR7, 
                                   levels = c("urban: metropolis", "urban: regiopolis", "urban: medium-sized town", "urban: village area", 
                                              "rural: central city", "rural: medium-sized city", "rural: village area"),
                                   labels = c("urban: metropolis-regiopolis", "urban: metropolis-regiopolis", "urban: medium-sized city to small town area", "urban: medium-sized city to small town area", 
                                              "rural area", "rural area", "rural area"))

model_data$metropolis_regiopolis <- ifelse(model_data$RegioStaR7_gr=="urban: metropolis-regiopolis",1,0)
model_data$urb_medium_small_city <- ifelse(model_data$RegioStaR7_gr=="urban: medium-sized city to small town area",1,0)

# dummy code RegioStaR4 and level it correctly
model_data$RegioStaR4 <- factor(model_data$RegioStaR4, 
                                levels = c("metropolitan urban region", "regiopolitan urban region", "rural region close to an urban region", "peripheral rural region"))

model_data$metropolitan_urban_region <- ifelse(model_data$RegioStaR4=="metropolitan urban region",1,0)
model_data$peripheral_rural_region <- ifelse(model_data$RegioStaR4=="peripheral rural region",1,0)

# aggregate hhgr_gr
model_data$hhgr_gr2 <- factor(model_data$hhgr_gr, 
                            levels = c("1 person", "2 persons", "3 persons", "4 persons", "5 or more persons"),
                            labels = c("1 person", "2 persons", "3 or more persons", "3 or more persons", "3 or more persons"))

model_data$hhgr_gr3 <- factor(model_data$hhgr_gr, 
                              levels = c("1 person", "2 persons", "3 persons", "4 persons", "5 or more persons"),
                              labels = c("1-2 persons", "1-2 persons", "3 or more persons", "3 or more persons", "3 or more persons"))

model_data$single_HH <- ifelse(model_data$H_GR == 1,1,0)

# order the levels of some categorical variables again to represent them in the correct order in the model
model_data$oek_status <- factor(model_data$oek_status, 
                                levels = c("very low", "low", "medium", "high", "very high"))

model_data$jahresfl_gr <- factor(model_data$jahresfl_gr, 
                                levels = c("<5000 km", "5000 - <10000 km", "10000 - <15000 km", "15000 - <20000 km", "20000 - <25000 km", "25000 - <50000 km", "50000 km or more", "unplausible", "not specified"))

model_data$seg_kba_imp <- factor(model_data$seg_kba_imp, 
                            levels = c('Mini', 'Kleinwagen', 'Kompaktklasse', 'Mittelklasse', 'obere Mittelklasse', 'Oberklasse', 'Sportgeländewagen', 'Geländewagen', 'Sportwagen', 'Mini-Vans', 'Großraum-Vans', 'Utilities'))

model_data$seg_kba_gr <- factor(model_data$seg_kba_gr, 
                                 levels = c('small', 'compact', 'medium', 'big', 'not assignable'))
                                 
#model_data$hhtyp2 <- factor(model_data$hhtyp2, 
#                                 levels = c('young:<35', 'family', 'only adults', 'old:+65'))

model_data$oek_status_gr <- factor(model_data$oek_status, 
                                   levels = c("very low", "low", "medium", "high", "very high"),
                                   labels = c("very low - low", "very low - low", "medium", "high", "very high"))

# modify annual kilometers traveled, since it has too extreme values
model_data$A_JAHRESFL_wins <- Winsorize(model_data$A_JAHRESFL_imp, val = quantile(model_data$A_JAHRESFL_imp, probs = c(0.001, 0.99), na.rm = FALSE))
summary(model_data$A_JAHRESFL_wins)
model_data$A_JAHRESFL_wins_std <- scale(model_data$A_JAHRESFL_wins)
summary(model_data$A_JAHRESFL_wins_std)

# zweck
library(forcats)
model_data$zweck <- factor(model_data$zweck,
                               levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 99),
                               labels = c('access to work', 'business', 'education', 'shopping', 'privat', 'care', 'leisure', 'home', 'back', 'other', 'no information'))

model_data$zweck <- fct_explicit_na(model_data$zweck, na_level = "no information")

model_data$zweck_gr <- factor(model_data$zweck,
                           levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 99),
                           labels = c('work-business', 'work-business', 'education', 'shopping-service', 'shopping-service', 'shopping-service', 'leisure', 'home', 'back', 'other', 'no information'))

model_data$zweck_gr <- fct_explicit_na(model_data$zweck_gr, na_level = "no information")

model_data$work <- ifelse(model_data$zweck == 1,1,0)
model_data$shopping <- ifelse(model_data$zweck == 4,1,0)

# when using HH_education, HH_occupancy or gender_group, clean the modeling data from NAs first  
colSums(is.na(model_data))
sum(is.na(model_data))
model_data <- na.omit(model_data)

################################################################################
# create a train and test data set
set.seed(1)

sample <- sample.int(n = nrow(model_data), size = floor(.70*nrow(model_data)), replace = F)
train <- model_data[sample, ]
test  <- model_data[-sample, ]

#logit train and test data for the model predicting emission class
logit_data_train <- mlogit.data(train, choice = "EUR", shape = "wide") 
logit_data_test <- mlogit.data(test, choice = "EUR", shape = "wide")

### empirical level
## mnl model predicting the pseudo emission classes EUR6, EUR5 EUR1-4 and else
# needs to be adapted according to filtering (add H_ANZAUTO when HH with > 1 cars - H_ANZAUTO + H_GR * H_ANZAUTO + )
# 1CHH: oek_status + single_HH + family + young_household + RegioStaR7_gr + bahn28_impgr_500 + diesel_bin + log(A_JAHRESFL_wins) + seg_kba_imp + company_car + full_time_working + HH_education
# 1st C: oek_status_gr + hhgr_gr2 + family + RegioStaR7_gr2 + bahn28_impgr_more_5000 + bus28_impgr + diesel_bin + log(A_JAHRESFL_wins) + seg_kba_imp + company_car + HH_occupation + HH_education
# 2nd C: oek_status_gr + hhgr_gr2 + young_household + RegioStaR7_gr2 + bus28_impgr + log(A_JAHRESFL_wins) + diesel_bin + seg_kba_imp + company_car + HH_education + HH_occupation

#mnl_EUR <- mlogit(EUR ~ 0 | oek_status_gr + hhgr_gr2 + young_household + RegioStaR7_gr2 + bus28_impgr + log(A_JAHRESFL_wins) + diesel_bin + seg_kba_imp + company_car + HH_education + HH_occupation, data = logit_data_train, weights = A_GEW)
#sink(file = "mnl_EUR_2nd_2_5CHH_empirical_final_attempt.txt")
#summary(mnl_EUR)
#exp(cbind(coef(mnl_EUR), confint(mnl_EUR)))
#sink(file = NULL)

### implementation level
## mnl model predicting the pseudo emission classes EUR6, EUR5 EUR1-4 and else
# needs to be adapted according to filtering (add H_ANZAUTO when HH with > 1 cars - H_ANZAUTO + H_GR * H_ANZAUTO + )
# 1CHH: oek_status + single_HH + family + young_household + RegioStaR7_gr + bahn28_impgr_500 + log(A_JAHRESFL_wins) + HH_education
# 1st C: oek_status_gr + hhgr_gr2 + family + RegioStaR7_gr2 + bus28_impgr + log(A_JAHRESFL_wins) + non_working + HH_education
# 2nd C: oek_status_gr + hhgr_gr2 +  + bus28_impgr + HH_education + part_time_working

mnl_EUR <- mlogit(EUR ~ 0 | oek_status_gr + hhgr_gr2 + only_adult_household + RegioStaR2 + bus28_impgr + HH_education + part_time_working + log(A_JAHRESFL_wins), data = logit_data_train, weights = A_GEW)
#sink(file = "mnl_EUR_1CHH_implementation_final_attempt.txt")
#sink(file = "mnl_EUR_1st_2_5CHH_implementation_final_attempt.txt")
sink(file = "mnl_EUR_2nd_2_5CHH_implementation_final_attempt.txt")
summary(mnl_EUR)
exp(cbind(coef(mnl_EUR), confint(mnl_EUR)))
sink(file = NULL)

#saveRDS(mnl_EUR, file = "mnl_EUR_1CHH_implementation.rds")
#saveRDS(mnl_EUR, file = "mnl_EUR_1st_2_5CHH_implementation.rds")
#saveRDS(mnl_EUR, file = "mnl_EUR_2nd_2_5CHH_implementation.rds")


### implementation level
## mnl model predicting the pseudo emission classes EUR6, EUR5 EUR1-4 and else
# needs to be adapted according to filtering (add H_ANZAUTO when HH with > 1 cars - H_ANZAUTO + H_GR * H_ANZAUTO + )
#mnl_EUR <- mlogit(EUR ~ 0 | oek_status + single_HH + young_household + only_adult_household + log(A_JAHRESFL_wins) + metropolis_regiopolis + bahn28_impgr_500, data = logit_data_train, weights = A_GEW)
#sink(file = "mnl_EUR_1CHH_implementation_level_16.txt")
#summary(mnl_EUR)
#exp(cbind(coef(mnl_EUR), confint(mnl_EUR)))
#sink(file = NULL)

################################################################################
## recoding of variables to clean the model
#make dummy coding of EUR to make variables EUR specific
logit_data_train$EUR_4 <- ifelse(logit_data_train$alt=="EUR 1-4 or else",1,0)
logit_data_train$EUR_5 <- ifelse(logit_data_train$alt=="EUR 5", 1,0)
logit_data_train$EUR_6 <- ifelse(logit_data_train$alt=="EUR 6",1,0)

logit_data_train$higher_education <- ifelse(logit_data_train$HH_education=="university",1,0)
logit_data_train$bus_500 <- ifelse(logit_data_train$bus28_impgr=="more than 500 m",1,0)
logit_data_train$hh_gr3 <- ifelse(logit_data_train$hhgr_gr2=="3 or more persons",1,0)
logit_data_train$hh_gr22 <- ifelse(logit_data_train$hhgr_gr2=="2 persons",1,0)
logit_data_train$RegioStaR2_bin <- ifelse(logit_data_train$RegioStaR2=="urban",1,0)

logit_data_test$EUR_4 <- ifelse(logit_data_test$alt=="EUR 1-4 or else",1,0)
logit_data_test$EUR_5 <- ifelse(logit_data_test$alt=="EUR 5", 1,0)
logit_data_test$EUR_6 <- ifelse(logit_data_test$alt=="EUR 6",1,0)

logit_data_test$higher_education <- ifelse(logit_data_test$HH_education=="university",1,0)
logit_data_test$bus_500 <- ifelse(logit_data_test$bus28_impgr=="more than 500 m",1,0)
logit_data_test$hh_gr3 <- ifelse(logit_data_test$hhgr_gr2=="3 or more persons",1,0)
logit_data_test$hh_gr22 <- ifelse(logit_data_test$hhgr_gr2=="2 persons",1,0)
logit_data_test$RegioStaR2_bin <- ifelse(logit_data_test$RegioStaR2=="urban",1,0)

################################################################################
### clean model version
#single car
mnl_EUR <- mlogit(EUR ~ I(EUR_5 * single_HH) + I(EUR_5 * young_household) | oek_status_gr + family + bahn28_impgr2 + log(A_JAHRESFL_wins), data = logit_data_train, weights = A_GEW)
sink(file = "mnl_EUR_1CHH_implementation_final_clean.txt")
summary(mnl_EUR)
exp(cbind(coef(mnl_EUR), confint(mnl_EUR)))
vif(mnl_EUR)
sink(file = NULL)
saveRDS(mnl_EUR, file = "mnl_EUR_1CHH_implementation_clean.rds")

#1st car
mnl_EUR <- mlogit(EUR ~ I(EUR_6 * family) + I(EUR_5 * bus_500) + I(EUR_5 * higher_education) | oek_status_gr + hhgr_gr3 + RegioStaR7_gr2 + log(A_JAHRESFL_wins) + non_working, data = logit_data_train, weights = A_GEW)
sink(file = "mnl_EUR_1st_2_5CHH_implementation_final_clean.txt")
summary(mnl_EUR)
exp(cbind(coef(mnl_EUR), confint(mnl_EUR)))
vif(mnl_EUR)
sink(file = NULL)
saveRDS(mnl_EUR, file = "mnl_EUR_1st_2_5CHH_implementation_final_clean.rds")

#2nd car
mnl_EUR <- mlogit(EUR ~ I(EUR_6 * only_adult_household) + I(EUR_5 * bus_500) + I(EUR_6 * higher_education) + I(EUR_6 * part_time_working)| oek_status_gr + log(A_JAHRESFL_wins), data = logit_data_train, weights = A_GEW)
sink(file = "mnl_EUR_2nd_2_5CHH_implementation_final_clean.txt")
summary(mnl_EUR)
exp(cbind(coef(mnl_EUR), confint(mnl_EUR)))
vif(mnl_EUR)
sink(file = NULL)
saveRDS(mnl_EUR, file = "mnl_EUR_2nd_2_5CHH_implementation_final_clean.rds")

################################################################################
## test the model first once before doing the 10 seed testing
# Predict probabilities
predicted_probs <- predict(mnl_EUR, newdata = logit_data_test)
length(predicted_probs)
head(predicted_probs)
unique(logit_data_test$alt)
dim(predicted_probs)

# Convert predicted_probs to a data.frame with chid as row names
predicted_probs_df <- as.data.frame(predicted_probs)
predicted_probs_df$chid <- as.numeric(rownames(predicted_probs_df))

# Pivot longer so that each row is one alternative per choice set
predicted_probs_long <- predicted_probs_df %>%
  pivot_longer(cols = -chid, names_to = "alt", values_to = "predicted_prob")

# Add predictions to the logit test dataset
results <- logit_data_test %>%
  left_join(predicted_probs_long, by = c("chid", "alt"))

# For each choice situation (chid), get the alternative with highest predicted probability
predicted_choices <- results %>%
  group_by(chid) %>%
  slice_max(order_by = predicted_prob, n = 1) %>%
  ungroup()

predicted_class <- predicted_choices$alt

actual_choices <- results[results$EUR == TRUE, ]
actual_choices <- actual_choices[order(actual_choices$chid), ]
actual_class <- actual_choices$alt

# Compute  and print confusion statistics
confusion_stats <- confusionMatrix(as.factor(predicted_class), as.factor(actual_class))
sink(file = "mnl_EUR_1CHH_implementation_level_15_ohneEDU_only_diesel_confmatrix.txt")
print(confusion_stats)
sink(file = NULL)

################################################################################
### simulate decisions with 10 different seeds (not deterministically by choosing
### the emission class with the highest probability, but probabilistically/stochastically)

# prediction probabilities for test data
P <- predict(mnl_EUR_1CHH_implementation_final_clean_withoutmileage, newdata = logit_data_test, type = "prob")  

true_choice <- subset(logit_data_test, EUR == TRUE)$alt

seeds <- 1:10
results_list <- list()

for (s in seeds) {
  set.seed(s)
  
  # do probabilistic decision making using the sample() function
  # sample() does 
  #-normalize the prob vector (if not already summing to 1).
  #-generate a random number.
  #-compare that number against cumulative probabilities
  #-return one of the elements according to those probabilities
  
  decisions <- character(nrow(P))
  for (i in 1:nrow(P)) {
    decisions[i] <- sample(colnames(P), 1, prob = as.numeric(P[i, ]))
  }
  
  # Confusion matrix
  cm <- confusionMatrix(factor(decisions, levels = colnames(P)),
                        factor(true_choice, levels = colnames(P)))
  
  # differenciate between metrics for the entire model and for each EUR class
  overall <- cm$overall
  byClass <- cm$byClass
  
  # Flatten class-wise confusion metrics to a vector format
  if (is.matrix(byClass)) {
    byClass_vec <- as.vector(t(byClass))
    class_names <- rep(colnames(byClass), each = nrow(byClass))
    metric_names <- rep(rownames(byClass), times = ncol(byClass))
    names(byClass_vec) <- paste(class_names, metric_names, sep = "_")
  } else {
    byClass_vec <- byClass
    names(byClass_vec) <- paste0("Class_", names(byClass))
  }
  
  # Extract confusion matrix table (counts)
  cm_table <- cm$table
  classes <- colnames(cm_table)
  
  # Compute TP, FP, FN, TN using vectorized code
  TP <- diag(cm_table)
  FP <- rowSums(cm_table) - TP
  FN <- colSums(cm_table) - TP
  TN <- sum(cm_table) - TP - FP - FN
  
  TP_vec <- setNames(TP, paste0("TP_", classes))
  FP_vec <- setNames(FP, paste0("FP_", classes))
  FN_vec <- setNames(FN, paste0("FN_", classes))
  TN_vec <- setNames(TN, paste0("TN_", classes))
  
  # Compute cross entropy for the whole mnl model (=logloss for binary logit model)
  col_indices <- match(true_choice, colnames(P))
  row_indices <- 1:nrow(P)
  chosen_probs <- P[cbind(row_indices, col_indices)]
  chosen_probs[chosen_probs == 0] <- 1e-15  # avoid log(0)
  log_loss <- -mean(log(chosen_probs))
  
  # Compute cross entropy for each EUR class
  class_log_loss <- data.frame(
    true_class = true_choice,
    chosen_prob = chosen_probs
  ) %>%
    group_by(true_class) %>%
    summarise(log_loss = -mean(log(pmax(chosen_prob, 1e-15)))) %>%
    ungroup()
  
  log_loss_vec <- setNames(class_log_loss$log_loss,
                           paste0("LogLoss_", class_log_loss$true_class))
  
  # Combine all metrics
  combined <- c(Seed = s, overall, byClass_vec, TP_vec, TN_vec, FP_vec, FN_vec, log_loss_vec, LogLoss = log_loss)
  results_list[[as.character(s)]] <- combined
}


# Combine all seed results from the list to a data frame
all_names <- unique(unlist(lapply(results_list, names)))

results_df <- do.call(rbind, lapply(results_list, function(x) {
  x_full <- setNames(as.list(rep(NA, length(all_names))), all_names)
  x_full[names(x)] <- x
  return(as.data.frame(x_full, stringsAsFactors = FALSE))
}))

# Convert to numeric and round to 3 decimal places (except "Seed")
for (col in setdiff(colnames(results_df), "Seed")) {
  results_df[[col]] <- round(as.numeric(results_df[[col]]), 3)
}

# Write the results_df with all confusion statistics to CSV
write.csv(results_df, "confusion_stats_1st_2_5CHH_EUR_implementation_clean.csv", row.names = FALSE)


################################################################################
## get the model's probabilities for the real made choice in the test data set
chosen_probs_df <- data.frame(
  true_class = true_choice,
  predicted_prob = chosen_probs,
  error = 1 - chosen_probs
)

# Plot distribution of model's confidence in the chosen emission class
ggplot(chosen_probs_df, aes(x = predicted_prob)) +
  geom_histogram(binwidth = 0.05, fill = "#2c7fb8", color = "white", boundary = 0) +
  labs(
    title = "Distribution of predicted probabilities for the chosen emission class",
    x = "Predicted Probability",
    y = "Count"
  ) +
  theme_minimal()

# Plot distribution of model's uncertainty
ggplot(chosen_probs_df, aes(x = error)) +
  geom_histogram(binwidth = 0.05, fill = "#f03b20", color = "white", boundary = 0) +
  labs(
    title = "Distribution of Model Uncertainty",
    x = "1 - Predicted Probability for True Class",
    y = "Count"
  ) +
  theme_minimal()


## plot emission class specific metrics
# Ensure Seed is treated as a factor for plotting
results_df$Seed <- as.factor(results_df$Seed)

# Sensitivity per emission class
sensitivity_long <- results_df %>%
  select(Seed, contains("Sensitivity")) %>%
  pivot_longer(-Seed, names_to = "Class", values_to = "Sensitivity") %>%
  mutate(
    # Remove any prefix like "Sensitivity_Class." or ".*Sensitivity_"
    Class = gsub("Sensitivity_Class\\.|.*Sensitivity_", "", Class),
    
    # Replace dots with spaces or keep original label
    Class = case_when(
      Class == "EUR.1.4.or.else" ~ "EUR 1-4 or else",
      Class == "EUR.5" ~ "EUR 5",
      Class == "EUR.6" ~ "EUR 6",
      TRUE ~ Class
    )
  )

ggplot(sensitivity_long, aes(x = Class, y = Sensitivity)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Sensitivity per Emission Class across 10 seeds testing",
       y = "Sensitivity", x = "Emission Class") +
  theme_minimal()


# LogLoss per emission class
logloss_long <- results_df %>%
  select(Seed, starts_with("LogLoss_")) %>%
  pivot_longer(-Seed, names_to = "Class", values_to = "LogLoss") %>%
  mutate(Class = gsub("LogLoss_", "", Class))

ggplot(logloss_long, aes(x = Class, y = LogLoss)) +
  geom_boxplot(fill = "salmon", color = "black") +
  labs(title = "Log Loss per Emission Class Across Seeds",
       y = "Log Loss", x = "Class") +
  theme_minimal()


# prediction error per emission class
results_df$EUR4 <- 1 - results_df$Sensitivity_Class..EUR.1.4.or.else  # Sensitivity = Recall = TP / (TP + FN)
results_df$EUR5 <- 1 - results_df$Sensitivity_Class..EUR.5  # Sensitivity = Recall = TP / (TP + FN)
results_df$EUR6 <- 1 - results_df$Sensitivity_Class..EUR.6  # Sensitivity = Recall = TP / (TP + FN)

results_long <- results_df %>%
  select(Seed, starts_with("EUR")) %>%
  pivot_longer(-Seed, names_to = "Class", values_to = "Error")

ggplot(results_long, aes(x = Class, y = Error)) +
  geom_boxplot(fill = "salmon", alpha = 0.6) +
  geom_jitter(width = 0.2, color = "skyblue", alpha = 0.4) +
  labs(
    title = "Prediction error across 10 seeds testing for each emission class",
    x = "Emission Class",
    y = "1 - Sensitivity"
  ) +
  theme_minimal()

summary(pp_synpop$income)
summary(pp_synpop_10pct$income)

t <- logit_data_test <- mnl_EUR_1CHH_implementation_final_clean_withoutmileage
hfmtest(t)
