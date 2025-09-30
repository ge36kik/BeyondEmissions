library(readr)
library(psych)
library(dplyr)
library(MASS)
library(factoextra)
library(ggplot2)
library(tidyr)
library(mlogit)
library(caret)
library(car)
library(pROC)
library(DescTools)
library(MLmetrics)

plot_data <- read_csv("C://Users//tumme//Desktop//plot_data.csv")

################################################################################
###modeling data preparation
##create model data depending on the number of cars per household
##differentiate between
#- households owning only one car
#- the most driven car of households owning 2-5 cars
#- the lesser driven cars of households owning 2-5 cars

model_data <- plot_data %>%
  filter(Diesel == 'Diesel' | Diesel == 'non_Diesel') %>% # filter only diesel car, since the predictors show stronger tendencies for diesel cars
  filter(hhtyp2 != 'other') %>% # filter out cars without any information about the annual mileage; not necessary anymore because of imputation
  
  # filtering for one car households (comment out all filtering lines for 2-5C HHs)
  filter(H_ANZAUTO <= 1) %>% 
  filter(H_GR <= 7) #filter since from H_GR > 7, not all choices are represented for one car households
  
  # filtering for 2-5 car households (comment out the filtering lines for 1C HHs)
  #filter(H_ANZAUTO <= 5 & H_ANZAUTO > 1) %>%
  #filter(H_GR <= 9) %>% #filter since from H_GR > 8, not all choices are represented for multiple car households
  #group_by(H_ID) %>%

  # from these households filter the most driven car
  #filter(A_JAHRESFL_imp == max(A_JAHRESFL_imp)) %>%

  # from these households filter the lesser driven cars
  #arrange(desc(A_JAHRESFL_imp)) %>%
  #mutate(rank = row_number()) %>%
  #filter(rank != 1) %>%
  #dplyr::select(-rank) %>%
  #ungroup()


# refactor the variable for fuel type to get rid of already filtered out levels
#model_data$antrieb <- factor(model_data$antrieb, 
#                             levels = c('Petrol', 'Diesel'),# 'Other'),
#                             labels = c('Petrol', 'Diesel'))#, 'Other'))

# refactor the variable for fuel type to get rid of already filtered out levels
model_data$Diesel <- factor(model_data$Diesel, 
                            levels = c('non_Diesel', 'Diesel'),
                            labels = c('non_Diesel', 'Diesel'))

# create a dummy variable for company car, since it is relevant, 
# >90% of the data in A_HALTER contains no information --> the share of company car is too small to impute it
# filtering leads to a too high data loss
model_data$company_car <- ifelse(model_data$A_HALTER =="company car",1,0)

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

# recode distance to train station, if the model coefficients are to similar (recode function did not work)
model_data$bahn28_impgr <- factor(model_data$bahn28_imp, 
                                  levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                  labels = c('up to 2500 m', 'up to 2500 m', 'up to 2500 m', 'up to 2500 m', 'more than 2500 m', 'more than 2500 m'))

model_data$bahn28_impgr2 <- factor(model_data$bahn28_imp, 
                                 levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                 labels = c('< 250 m', '250 - <500 m', '500 - 2500 m', '500 - 2500 m', '2500 - <5000 m', 'more than 5000 m'))

# aggregate levels for distance to bus station
model_data$bus28_imp <- factor(model_data$bus28_imp, 
                               levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'))

# recode distance to bus station, if model coeffients are to similar (recode function did not work)
model_data$bus28_impgr <- factor(model_data$bus28_imp, 
                                 levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                 labels = c('< 250 m', '250 - <500 m', '500 - 2500 m', '500 - 2500 m', '2500 - <5000 m', 'more than 5000 m'))

# recode distance to bus station, if model coeffients are to similar (recode function did not work)
model_data$bus28_impgr2 <- factor(model_data$bus28_imp, 
                                 levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                 labels = c('up to 500 m', 'up to 500 m', 'more than 500 m', 'more than 500 m', 'more than 500 m', 'more than 500 m'))

# recode distance to bus station, if model coeffients are to similar (recode function did not work)
model_data$bus28_impgr3 <- factor(model_data$bus28_imp, 
                                  levels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'),
                                  labels = c('up to 1000 m', 'up to 1000 m', 'up to 1000 m', 'more than 1000 m', 'more than 1000 m', 'more than 1000 m'))


# aggregate levels of or dummy code RegioStaR7 and level it correctly
model_data$RegioStaR7 <- factor(model_data$RegioStaR7, 
                                levels = c("urban: metropolis", "urban: regiopolis", "urban: medium-sized town", "urban: village area", 
                                           "rural: central city", "rural: medium-sized city", "rural: village area"))

model_data$RegioStaR7_gr <- factor(model_data$RegioStaR7, 
                                   levels = c("urban: metropolis", "urban: regiopolis", "urban: medium-sized town", "urban: village area", 
                                              "rural: central city", "rural: medium-sized city", "rural: village area"),
                                   labels = c("urban: metropolis-regiopolis", "urban: metropolis-regiopolis", "urban: medium-sized city to small town area", "urban: medium-sized city to small town area", 
                                              "rural: central city", "rural: medium-sized city", "rural: village area"))

model_data$RegioStaR7_gr2 <- factor(model_data$RegioStaR7, 
                                   levels = c("urban: metropolis", "urban: regiopolis", "urban: medium-sized town", "urban: village area", 
                                              "rural: central city", "rural: medium-sized city", "rural: village area"),
                                   labels = c("urban: metropolis-regiopolis", "urban: metropolis-regiopolis", "urban: medium-sized city to small town area", "urban: medium-sized city to small town area", 
                                              "rural area", "rural area", "rural area"))

model_data$metropolis <- ifelse(model_data$RegioStaR7=="urban: metropolis",1,0)

# dummy code RegioStaR4 and level it correctly
model_data$RegioStaR4 <- factor(model_data$RegioStaR4, 
                                levels = c("metropolitan urban region", "regiopolitan urban region", "rural region close to an urban region", "peripheral rural region"))

model_data$metropolitan_urban_region <- ifelse(model_data$RegioStaR4=="metropolitan urban region",1,0)
model_data$peripheral_rural_region <- ifelse(model_data$RegioStaR4=="peripheral rural region",1,0)

# order the levels of some categorical variables again to represent them in the correct order in the model
model_data$oek_status <- factor(model_data$oek_status, 
                                levels = c("very low", "low", "medium", "high", "very high"))

model_data$oek_status_gr <- factor(model_data$oek_status, 
                                levels = c("very low", "low", "medium", "high", "very high"),
                                labels = c("very low - low", "very low - low", "medium", "high", "very high"))

model_data$jahresfl_gr <- factor(model_data$jahresfl_gr, 
                                 levels = c("<5000 km", "5000 - <10000 km", "10000 - <15000 km", "15000 - <20000 km", "20000 - <25000 km", "25000 - <50000 km", "50000 km or more", "unplausible", "not specified"))

model_data$seg_kba_imp <- factor(model_data$seg_kba_imp, 
                                 levels  = c('Mini', 'Kleinwagen', 'Kompaktklasse', 'Mittelklasse', 'obere Mittelklasse', 'Oberklasse', 'Sportgeländewagen', 'Geländewagen', 'Sportwagen', 'Mini-Vans', 'Großraum-Vans', 'Utilities'))

model_data$seg_kba_imp_gr <- factor(model_data$seg_kba_imp, 
                                 levels  = c('Mini', 'Kleinwagen', 'Kompaktklasse', 'Mittelklasse', 'obere Mittelklasse', 'Oberklasse', 'Sportgeländewagen', 'Geländewagen', 'Sportwagen', 'Mini-Vans', 'Großraum-Vans', 'Utilities'),
                                 labels  = c('small-medium', 'small-medium', 'small-medium', 'small-medium', 'small-medium', 'Oberklasse', 'Sportgeländewagen', 'Geländewagen', 'Sportwagen', 'Mini-Vans', 'Großraum-Vans', 'Utilities'))

model_data$seg_kba_gr <- factor(model_data$seg_kba_gr, 
                                levels = c('small', 'compact', 'medium', 'big', 'not assignable'))

model_data$hhtyp2 <- factor(model_data$hhtyp2, 
                            levels = c("young:<35", "family", "only adults", "old:+65"),
                            labels = c("young:<35", "family", "only adults", "old:+65"))

# aggregate hhgr_gr
model_data$hhgr_gr2 <- factor(model_data$hhgr_gr, 
                              levels = c("1 person", "2 persons", "3 persons", "4 persons", "5 or more persons"),
                              labels = c("1 person", "2 persons", "3 or more persons", "3 or more persons", "3 or more persons"))

model_data$hhgr_gr3 <- factor(model_data$hhgr_gr, 
                              levels = c("1 person", "2 persons", "3 persons", "4 persons", "5 or more persons"),
                              labels = c("1-2 persons", "1-2 persons", "3 or more persons", "3 or more persons", "3 or more persons"))


model_data$single_HH <- ifelse(model_data$H_GR == 1,1,0)

# create fuel price difference variable
model_data$fuel_price_difference <- model_data$price.petrol_ADAC - model_data$price.diesel_ADAC

# modify annual kilometers travelled since it has too rxtreme values
model_data$A_JAHRESFL_wins <- Winsorize(model_data$A_JAHRESFL_imp, val = quantile(model_data$A_JAHRESFL_imp, probs = c(0.001, 0.99), na.rm = FALSE))
summary(model_data$A_JAHRESFL_wins)

# mean - center some variables
model_data$log_A_JAHRESFL_wins <- log(model_data$A_JAHRESFL_wins)
model_data$log_A_JAHRESFL_wins_c <- model_data$log_A_JAHRESFL_wins - mean(model_data$log_A_JAHRESFL_wins)
model_data$fuel_price_difference_c <- model_data$fuel_price_difference - mean(model_data$fuel_price_difference)

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

# when using HH_education and HH_occupancy, clean the modeling data from NAs first  
colSums(is.na(model_data))
sum(is.na(model_data))
model_data <- na.omit(model_data)

################################################################################
# check the fuel type distribution because of filtering
prop.table(table(model_data$antrieb))
# for 1 car households
# Petrol    Diesel 
# 0.7102183 0.2897817 
# for 2-5 car households
# Petrol   Diesel 
# 0.617459 0.382541
prop.table(table(model_data$Diesel))
# for 1 car households
# non_Diesel     Diesel 
# 0.7154538  0.2845462 
# for 2 - 5 car households
# non_Diesel     Diesel 
# 0.6237353  0.3762647 
################################################################################

# create a train and test data set
set.seed(1)

sample <- sample.int(n = nrow(model_data), size = floor(.70*nrow(model_data)), replace = F)
train <- model_data[sample, ]
test  <- model_data[-sample, ]

################################################################################

### binary logit model predicting the fuel types Diesel and non-Diesel
# needs to be adapted according to filtering (add H_ANZAUTO when HH has > 1 cars; optional: H_GR * H_ANZAUTO)

### empricial level
# for empirical level add: seg_kba_imp + company_car + price.diesel_ADAC + price.petrol_ADAC + EUR
#1CHH
glm_antrieb <- glm(Diesel ~ oek_status_gr + single_HH + hhtyp2 + HH_education + HH_gender + 
                     RegioStaR7_gr2 + bahn28_impgr + bus28_impgr2 + log_A_JAHRESFL_wins_c + 
                     company_car + seg_kba_imp_gr + EUR + fuel_price_difference_c, 
                   family = binomial, data = train, weights = A_GEW)
sink(file = "glm_Diesel_1CHH_empricial_final_gender.txt")

#1st 2_5CHH
glm_antrieb <- glm(Diesel ~ oek_status_gr + young_household + family + HH_education + HH_gender + 
                     RegioStaR7_gr2 + bus28_impgr2 + log_A_JAHRESFL_wins_c + 
                     company_car + seg_kba_imp_gr + EUR + fuel_price_difference_c + log_A_JAHRESFL_wins_c * fuel_price_difference_c, 
                   family = binomial, data = train, weights = A_GEW)
sink(file = "glm_Diesel_1st_2-5CHH_empirical_final_gender.txt")

#2nd 2_5CHH
glm_antrieb <- glm(Diesel ~ oek_status_gr + family + H_ANZAUTO + bus28_impgr2 + RegioStaR7_gr + HH_education + non_working, family = binomial, data = train, weights = A_GEW)
sink(file = "glm_Diesel_2nd_2-5CHH_implementation_final_gender_withoutmileage.txt")


### implementation level
#1CHH
glm_antrieb <- glm(Diesel ~ oek_status_gr + single_HH + hhtyp2 + HH_education + HH_gender + non_working + RegioStaR7_gr2 + bahn28_impgr + bus28_impgr2, family = binomial, data = train, weights = A_GEW)
sink(file = "glm_Diesel_1CHH_implementation_final_gender_withoutmileage.txt")

#1st 2_5CHH
glm_antrieb <- glm(Diesel ~ oek_status_gr + only_adult_household + family + HH_education + full_time_working + RegioStaR7_gr + bus28_impgr2, family = binomial, data = train, weights = A_GEW)
sink(file = "glm_Diesel_1st_2-5CHH_implementation_final_gender_withoutmileage.txt")
summary(`glm_Diesel_1st_2-5CHH_implementation_final_gender_withoutmileage`)
glm_antrieb <- `glm_Diesel_1st_2-5CHH_implementation_final_gender_withoutmileage`

#2nd 2_5CHH
glm_antrieb <- glm(Diesel ~ oek_status_gr + family + H_ANZAUTO + bus28_impgr2 + RegioStaR7_gr + HH_education + non_working, family = binomial, data = train, weights = A_GEW)
sink(file = "glm_Diesel_2nd_2-5CHH_implementation_final_gender_withoutmileage.txt")

summary(glm_antrieb)
exp(cbind(coef(glm_antrieb), confint(glm_antrieb)))
n <- length(glm_antrieb$residuals)
# Cox-Snell R^2
R2cs <- 1-exp((glm_antrieb$deviance-glm_antrieb$null.deviance)/n)
# Nagelkerke R^2
R2n <- R2cs/(1-exp(-(glm_antrieb$null.deviance/n)))
cat(sprintf("Pseudo R^2 Nagelkerke: %.3f\n", R2n))
sink(file = NULL)

saveRDS(glm_antrieb, file = "glm_Diesel_1CHH_implementation_final_gender_withouthhtyp2other.rds")
saveRDS(glm_antrieb, file = "glm_Diesel_1st_2-5CHH_empirical_final_gender_withouthhtyp2.rds")
saveRDS(glm_antrieb, file = "glm_Diesel_2nd_2-5CHH_implementation_final_gender_withoutmileage.rds")

saveRDS(glm_antrieb, file = "glm_Diesel_1CHH_implementation_final_gender_withoutmileage.rds")
saveRDS(glm_antrieb, file = "glm_Diesel_1st_2-5CHH_implementation_final_gender_withoutmileage.rds")
saveRDS(glm_antrieb, file = "glm_Diesel_2nd_2-5CHH_implementation_final_gender_withoutmileage.rds")

# check collinearity problems
vif(glm_antrieb)
# -> no collineartiy problems [1-2.5] < 5

###
# stepwise logistic regression
# default of step() is backwards
# mostly not necessary because result is the same due to manual variable selection based on significance
glm_antrieb_step <- step(glm_antrieb, direction = 'both')
sink(file = "glm_antrieb_2nd_2_5CHH_stepwise_empirical_final_withoutgender.txt")
summary(glm_antrieb_step)
sink(file = NULL)

################################################################################
##### for single model testing (no iterations with seeds)
### clean model version
# model with only significant variables
##glm_antrieb <- glm(antrieb ~ oek_status + H_GR + hhtyp2 + distance_trainstation_max5000 + distance_trainstation_5000_more + RegioStaR7 + log(A_JAHRESFL+1) + seg_kba_gr + company_car, family = binomial, data = train, weights = A_GEW)
#sink(file = "glm_antrieb_output_1CHH_segkbagr_jahrflsqrt_cleaned.txt")
#summary(glm_antrieb)
#sink(file = NULL)

#https://bjoernwalther.com/binaer-logistische-regression-in-r-rechnen-und-interpretieren/
# Omnibus-Test to check if the model with coefficients performs better than the null-model
#glm_antrieb$df.null
#glm_antrieb$df.residual
#glm_antrieb$null.deviance
#glm_antrieb$deviance
#modelchi <- glm_antrieb$null.deviance - glm_antrieb$deviance
#chidf <- glm_antrieb$df.null - glm_antrieb$df.residual
#chisqp <- 1-pchisq(modelchi, chidf)
#chisqp
# --> chisqp = 0 --> rejection of Null-Hypothesis (possible?)

# Modell-Güte
# Gütemaße
#n <- length(glm_antrieb$residuals)
# Cox-Snell R^2
#R2cs <- 1-exp((glm_antrieb$deviance-glm_antrieb$null.deviance)/n)
# Nagelkerke R^2
#R2n <- R2cs/(1-exp(-(glm_antrieb$null.deviance/n)))
#R2n 
# 0.3212507 for 1C_HH, 0.3509906 for 1Cmax_HH, 0.3482347 for 1C_HH_jahresflsqre, 0.3543537 for 1C_HH_jahresfllog, 0.3541645 for 1C_HH_jahresfllog_cleaned
# 0.3128566 for 5C_HH, 0.3130536 for 5C_HH_IA,
# 0.3212799 for 2-5C_HH, 0.3218621 for 2-5C_HH_IAfuelpricejahreslf
# 0.2184822 for 5C_HH_MiDWege_WGEW,
# 0.2605048 for 1C_HH_MiDWege_WGEW, 0.2404109 for 1C_HH_MiDWege_WGEW_ALTER
# 0.3487027 for 1stC_HH_segkba, 0.2904997 for 2ndC_HH_segkba, 0.2773533 for 2ndC_HH_segkba with 2-5 C HH
# 0.3465799 for 1CHH_mostdriven5CHH, 0.2514224 for 2-5CHH_lessdriven5CHH_segkbagr
# 0.3059665 for Diesel_1CHH_segkba, 0.3003456 for Diesel_1CHH_segkba_occ

## test the prediction qualities glm model for Petrol-Diesel with the test data set
# Predict probabilities on the test set
#predicted_probs <- predict(glm_antrieb, newdata = test, type = "response")
#table(Actualvalue = test$antrieb, Predictedvalue = predicted_probs > 0.3)
#auc(test$antrieb,predicted_probs)
#test_roc = roc(test$antrieb ~ predicted_probs, plot = TRUE, print.auc = TRUE)

## test the predictions qualitites glm model for Diesel-non_diesel with the test data set
# Predict probabilities on the test set
prop.table(table(test$Diesel))
predicted_probs <- predict(glm_antrieb, newdata = test, type = "response")
table(Actualvalue = test$Diesel, Predictedvalue = predicted_probs > 0.3)
auc(test$Diesel,predicted_probs)
test_roc = roc(test$Diesel ~ predicted_probs, plot = TRUE, print.auc = TRUE)

################################################################################
### other code for confusion matrix
# Convert probabilities to class labels (assuming "antrieb" is binary with 2 levels)
# Threshold can be adjusted: 0.3, for 2-5CHH segkba 0.5
#predicted_labels <- ifelse(predicted_probs >= 0.5,
#                           levels(test$Diesel)[2],
#                           levels(test$Diesel)[1])

# Convert to factor with the same levels as the true outcome
#predicted_labels <- factor(predicted_labels, levels = levels(test$Diesel))

# Confusion matrix
#conf_matrix <- confusionMatrix(predicted_labels, test$Diesel, positive = 'Diesel')
#sink(file = "conf_matrix_Diesel_output_2-5CHH_IA_segkba.txt")
#print(conf_matrix)
#sink(file = NULL)


################################################################################
##### check fuel distribution among different householdtypes for plausibilty checks of the models
fuel_distribution <- plot_data %>%
  filter(H_ANZAUTO == 1) %>%
  group_by(H_ID) %>%
  #filter(n() > 1) %>%
  summarise(
    unique_fuels = n_distinct(antrieb),
    fuel_combo = paste(sort(unique(antrieb)), collapse = "-")
  ) %>%
  mutate(
    category = case_when(
      unique_fuels == 1 & fuel_combo == "Diesel" ~ "All Diesel",
      unique_fuels == 1 & fuel_combo == "Petrol" ~ "All Petrol",
      unique_fuels > 1 ~ "Mixed",
      TRUE ~ "Other"
    )
  ) %>%
  count(category)

prop.table(table(fuel_distribution))
print(fuel_distribution)

#more than 1 car (63794 HIDs)
#1 All Diesel  8737 14%
#2 All Petrol 22219 35%
#3 Mixed      32603 51%
#4 Other        235 0.4%

# only adults, more than 1 car
#1 All Diesel  4274
#2 All Petrol 12345
#3 Mixed      17542
#4 Other        131

# families, more than 1 car
#1 All Diesel  3464
#2 All Petrol  5264
#3 Mixed      10614
#4 Other         73

# families, more than 1 car, filter geskm
#1 All Diesel   199
#2 All Petrol   340
#3 Mixed        493

# families with more than 1 car, the less driven cars
#1 All Diesel  4204
#2 All Petrol 11884
#3 Mixed       1116

# families with 1 car or more, the most driven car
#1 All Diesel 15453
#2 All Petrol 12153
#3 Mixed        765

#families with more than 1 car, the most driven car
#1 All Diesel 11081
#2 All Petrol  6690
#3 Mixed        765

#families with only 1 car
#1 All Diesel  4372
#2 All Petrol  5463


################################################################################
### create multinominal logit model for Petrol, Diesel, Other
#logit train and test data for the model predicting fuel type
#logit_data_train <- mlogit.data(train, choice = "antrieb", shape = "wide")#, varying = c(84:85)) 
#logit_data_test <- mlogit.data(test, choice = "antrieb", shape = "wide")

###
## mnl model predicting the fuel types Petrol, Diesel, Other
# needs to be adapted according to filtering (add H_ANZAUTO when HH with > 1 cars - H_ANZAUTO + H_GR * H_ANZAUTO + )
#mnl_antrieb <- mlogit(antrieb ~ 0 | oek_status + H_GR + hhtyp2 + bahn28 + RegioStaR7 + A_JAHRESFL + seg_kba_gr, data = logit_data_train, weights = A_GEW)
#sink(file = "mnl_antrieb_output_5CHH_segkbagr_allft_IA.txt")
#summary(mnl_antrieb)
#sink(file = NULL)

#predicted <- predict(mnl_antrieb, logit_data_test)
#conf_matrix <- confusionMatrix(predicted, logit_data_test$antrieb)
#print(conf_matrix)

# get predicted class labels 
#predicted_labels <- colnames(predicted)[apply(predicted, 1, which.max)]
#predicted_labels <- factor(predicted_labels, levels = levels(logit_data_test$antrieb))

# compute confusion matrix
#conf_matrix <- confusionMatrix(predicted_labels, logit_data_test$antrieb)
#print(conf_matrix)

################################################################################
###
# binary logit model predicting the fuel types Petrol and Diesel
# needs to be adapted according to filtering (add H_ANZAUTO when HH with > 1 cars) geskm + H_ANZAUTO + seg_kba_gr + company_car
#glm_antrieb <- glm(antrieb ~ oek_status + H_GR + hhtyp2 + bahn28 + RegioStaR2 + log(A_JAHRESFL+1), family = binomial, data = train, weights = A_GEW)
#sink(file = "glm_antrieb_output_1CHH_loggeskm_nobus_recodedbahn_withoutHHparam.txt")
#summary(glm_antrieb)
#exp(cbind(coef(glm_antrieb), confint(glm_antrieb)))
#vif(glm_antrieb)
#sink(file = NULL)