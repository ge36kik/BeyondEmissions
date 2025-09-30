install.packages(c("readr", "psych", "dplyr","corrplot","pheatmap","gmodels","MASS","pROC","factoextra","ggplot2", "tidyr"))
install.packages("mlogit")
install.packages("caret")
install.packages("carData")

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

#read MiD data files
data_MiD_HH <- read_csv("C://Users//Sonja.Tummel//Downloads//MiD2017_Haushalte.csv")
data_MiD_Autos <- read_csv("C://Users//Sonja.Tummel//Downloads//MiD2017_Autos.csv")
data_MiD_Wege <- read_csv("C://Users//Sonja.Tummel//Downloads//MiD2017_Wege.csv")
data_MiD_Personen <- read_csv("C://Users//Sonja.Tummel//Downloads//MiD2017_Personen.csv")
plot_data <- read_csv("C://Users//Sonja.Tummel//Documents//Uni//MA//R_Skripte//plot_data.csv")

# check data set to check distribution differences among car license holders compared to the whole sample
check <- data_MiD_Personen %>%
  # Filter to licensed people only
  filter(P_FS_PKW == 1)

###check how many H_IDS of MiD_Personen and MiD_Wege are covered in MiD_Autos
common_ids <- intersect(data_MiD_Autos$H_ID, data_MiD_Personen_fl$H_ID)
percent_common <- length(common_ids) / length(unique(data_MiD_Autos$H_ID)) * 100
cat(sprintf("Percentage of IDs in MiD_Autos covered from MiD_Personen: %.2f%%\n", percent_common))
# 100% without filtering, 98.31% with filtering

common_ids <- intersect(data_MiD_Autos$H_ID, data_MiD_Wege$H_ID)
percent_common <- length(common_ids) / length(unique(data_MiD_Autos$H_ID)) * 100
cat(sprintf("Percentage of IDs in MiD_Autos covered from MiD_Wege: %.2f%%\n", percent_common))
# 88.03% without filtering

#create unique car IDs
plot_data <- plot_data %>%
  mutate(car_id = paste0(H_ID, "_", A_ID))

#create unique car IDs
data_MiD_Wege <- data_MiD_Wege %>%
  mutate(car_id = paste0(H_ID, "_", W_WAUTO))

#create unique car IDs
data_MiD_Autos <- data_MiD_Autos %>%
  mutate(car_id = paste0(H_ID, "_", A_ID))

# check how many cars of the Wege data set can exactly identified in MiD_Auto
common_ids <- intersect(data_MiD_Autos$car_id, data_MiD_Wege$car_id)
percent_common <- length(common_ids) / length(unique(data_MiD_Autos$car_id)) * 100
cat(sprintf("Percentage of IDs in MiD_Autos covered from MiD_Wege: %.2f%%\n", percent_common))
# --> 3%

### create aggregated variable for the households' occupancy status
## if all household members owning a drivers license work full time -> all work
## if not all household members owning a drivers license work full time, but also part time or not at all --> part time working
## if none of the household members owning a drivers license works at all -> non working
## non covered case -> other

#plausibility check
#prop.table(table(occupation_status$HP_BKAT)) non_working= nicht berufstätig + unemployed + retired + student + 
#prop.table(table(occupation_status$HP_TAET))
#prop.table(table(occupation_status$alter_gr5))

occupation_status <- data_MiD_Personen %>%
  # Filter to licensed people only
  filter(P_FS_PKW == 1) %>%
  filter(HP_BKAT <= 7) %>%  # Exclude non covered information
  
  group_by(H_ID) %>%
  summarise(
    n_members = n(),
    n_full_time = sum(HP_BKAT == 1),    # all HH members are full-time working
    n_working = sum(HP_BKAT %in% 1:7),  # at least one HH member is working
    n_nonworking = sum(HP_BKAT == 7),   # no one in the HH is working (retired, unemployed, student without job,...)
    .groups = "drop"
  ) %>%
  
  mutate(HH_occupation = case_when(
    n_full_time == n_members ~ "full time working",
    n_working > 0 & n_working == n_members & n_nonworking != n_members ~ "part time working", 
    n_nonworking == n_members ~ "no one is working",
    TRUE ~ NA_character_
  )) %>%
  select(H_ID, HH_occupation)

summary(occupation_status$HH_occupation)

# check the result
table(occupation_status$HH_occupation, if_any)
#full time working no one is working part time working 
#44823             60100             43927 
table(data_MiD_Personen$HP_BKAT)
table(check$HP_BKAT)

# add the variable to the Auto data set
data_MiD_Autos <- data_MiD_Autos %>%
  left_join(
    occupation_status %>% select(H_ID, HH_occupation),  
    by = "H_ID")

### create aggregated variable for the households' education status
## if at least one household member owning a drivers licences has a university or Fachhochschule degree -> university
## otherwise -> else
education_status <- data_MiD_Personen %>%
  filter(P_FS_PKW == 1) %>%   # licensed members only
  group_by(H_ID) %>%
  summarise(
    has_university = any(P_BIL == 5),
    .groups = "drop"
  ) %>%
  mutate(HH_education = if_else(has_university, "university", "else")) %>%
  select(H_ID, HH_education)

# check the result
table(education_status$HH_education)
table(data_MiD_Personen$P_BIL)
table(check$P_BIL)

# add the variable to the Auto data set
data_MiD_Autos <- data_MiD_Autos %>%
  left_join(
    education_status %>% select(H_ID, HH_education),  
    by = "H_ID")

### create aggregated variable for the households' migration background status
## if all members have a migration background -> TRUE
## otherwise -> else
## since migration even with mixed=1 filtering does not affect Diesel-non_Diesel (4.6%-4.7%) and EUR (difference 4.1% - 3.7%  - 4.2%)
## --> leave it
#migration_status <- data_MiD_Personen %>% 
#  filter(P_FS_PKW == 1) %>%                # keep only licensed members
#  group_by(H_ID) %>% 
#  summarise(
#    HH_MIG = as.integer(any(P_MIG == 1)),  # 1 if all have migration background, else 0
#    .groups = "drop"
#  ) %>%
#  select(H_ID, HH_MIG)

# check the result
#table(migration_status$HH_MIG)
#0      1 
#144203   4729
#table(data_MiD_Personen$P_MIG)
#1      2      9    202    206    402 
#8463 139146   1614  92464  53853  20821
#table(check$HH_MIG)

# add the variable to the Auto data set
data_MiD_Autos <- data_MiD_Autos %>%
  left_join(
    migration_status %>% select(H_ID, HH_MIG),  
    by = "H_ID")

# create gender variable on household level
# in that aggregation gender does not have an effect on vehicle age
# EUR: f,m,mixed (27,32,39 - 26,32,41 - 28,31,40)
# in that aggregation there is a difference between male and female HH
# Diesel: f,m,mi (24,33,41) non_Diesel: 28, 31, 39)
gender <- data_MiD_Personen %>%
  filter(P_FS_PKW == 1) %>%
  filter(HP_SEX %in% c(1, 2)) %>%
  group_by(H_ID) %>%
  summarise(
    hh_size = n(),
    male_count = sum(HP_SEX == 1),
    female_count = sum(HP_SEX == 2),
    single_male = as.integer(hh_size == 1 & male_count == 1),
    single_female = as.integer(hh_size == 1 & female_count == 1),
    HH_gender = case_when(
      hh_size == 1 & male_count == 1 ~ "single male",
      hh_size == 1 & female_count == 1 ~ "single female",
      male_count > female_count ~ "mostly male",
      female_count > male_count ~ "mostly female",
      male_count == female_count ~ "equal number",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

table(gender$HH_gender)
table(data_MiD_Personen$HP_SEX)
table(check$HP_SEX)

# add the variable to the Auto data set
data_MiD_Autos <- data_MiD_Autos %>%
  left_join(
    gender %>% select(H_ID, gender_group),  
    by = "H_ID")

# add the variable to the plot data set
plot_data <- plot_data %>%
  left_join(
    gender %>% select(H_ID, HH_gender),  
    by = "H_ID")

## create main trip purpose variable
main_trip_purpose_per_car <- data_MiD_Wege %>%
  group_by(car_id, zweck) %>%
  summarise(n = sum(W_GEW, na.rm = TRUE), .groups = "drop") %>%
  group_by(car_id) %>%
  slice_max(n, with_ties = FALSE) %>%  # select the zweck with highest weighted count
  ungroup()

# add the variable to the plot data set
plot_data <- plot_data %>%
  left_join(main_trip_purpose_per_car %>% select(car_id, zweck) %>% distinct(), by = "car_id")

# add household weight H_ID to the Auto data set
data_MiD_Autos <- data_MiD_Autos %>%
  left_join(
    data_MiD_HH %>% select(H_ID, H_GEW),  
    by = "H_ID") 

df_counts <- plot_data %>%
  #filter(H_GR==1) %>%
  #filter(Diesel=='Diesel') %>%
  group_by(EUR, gender_group) %>%
  summarise(CarCount = n(), .groups = "drop")

df_shares <- df_counts %>%
  group_by(EUR) %>%
  mutate(Share = CarCount / sum(CarCount)) %>%
  ungroup()

df_counts <- plot_data %>%
  group_by(H_ID) %>%
  filter(max(A_BAUJ)) %>%
  ungroup()

# create a variable to categorize the vehicles into fuel type combined with 'Emission class'
# facilitation: 
# 1 = Benzin EUR1-3 or none, 
# 2 = Benzin EUR4, 
# 3 = Benzin EUR5, 
# 4 = Benzin EUR6
# 5 = Diesel EUR1-3 or none, 
# 6 = Diesel EUR4, 
# 7 = Diesel EUR5, 
# 8 = Diesel EUR6, 
# 9 = Other
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(vehEmissionClass = case_when(A_BAUJ < 2006 & antrieb == 1 ~ 1,
                                      A_BAUJ < 2011 & A_BAUJ >= 2006 & antrieb == 1 ~ 2,
                                      A_BAUJ < 2015 & A_BAUJ >= 2011 & antrieb == 1 ~ 3,
                                      A_BAUJ >= 2015 & antrieb == 1 ~ 4,
                                      A_BAUJ < 2006 & antrieb == 2 ~ 5,
                                      A_BAUJ < 2011 & A_BAUJ >= 2006 & antrieb == 2 ~ 6,
                                      A_BAUJ < 2015 & A_BAUJ >= 2011 & antrieb == 2 ~ 7,
                                      A_BAUJ >= 2015 & antrieb == 2 ~ 8,
                                      antrieb > 2 ~ 9))

# create a variable indicating "Diesel" or "non-Diesel"
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(Diesel = case_when(antrieb == 1 ~ 1,
                            antrieb == 2 ~ 2,
                            antrieb == 3 ~ 1,
                            antrieb > 3 ~ 3))

# create a variable for emissions classes EUR1-3, EUR4, EUR5, EUR6
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(EUR_old = case_when(A_BAUJ < 2006 ~ 1,
                             A_BAUJ < 2011 & A_BAUJ >= 2006 ~ 2,
                             A_BAUJ < 2015 & A_BAUJ >= 2011 ~ 3,
                             A_BAUJ >= 2015 ~ 4))

# create a variable for emissions classes EUR1-3, EUR4, EUR5, EUR6
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(EUR = case_when(A_BAUJ < 2011 ~ 1,
                         A_BAUJ < 2015 & A_BAUJ >= 2011 ~ 2,
                         A_BAUJ >= 2015 ~ 3))

# create a variable for the needed prediction categories in the first car type modeling attempt (multinominal logit model): "Petrol", "Diesel EUR1-3", "Diesel EUR4", "Diesel EUR5", "Diesel EUR6", "Other"
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(EUR_Diesel = case_when(antrieb == 1 ~ 1,
                                A_BAUJ < 2006 & antrieb == 2 ~ 5,
                                A_BAUJ < 2011 & A_BAUJ >= 2006 & antrieb == 2 ~ 6,
                                A_BAUJ < 2015 & A_BAUJ >= 2011 & antrieb == 2 ~ 7,
                                A_BAUJ >= 2015 & antrieb == 2 ~ 8,
                                antrieb > 2 ~ 9))

# https://www.bundestag.de/resource/blob/561134/4376c6bc0fc0b4286ecb7323cce04912/wd-5-069-18-pdf-data.pdf
# mark vehicles which could have been purchased because of the Abwrackprämie 2009
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(Abwrack_2009 = case_when(A_BAUJ == 2008 ~ 1,
                                  A_BAUJ == 2009 ~ 1,
                                  A_BAUJ < 2008 & A_BAUJ != 2009 ~ 0,
                                  A_BAUJ > 2009 ~ 2))

# create a petrol price category based on data retrieved from the ADAC
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(price.petrol_ADAC = case_when(A_BAUJ == 2000 ~ 198.8,
                                       A_BAUJ == 2001 ~ 200.0,
                                       A_BAUJ == 2002 ~ 104.6,
                                       A_BAUJ == 2003 ~ 109.2,
                                       A_BAUJ == 2004 ~ 113.2,
                                       A_BAUJ == 2005 ~ 121.7,
                                       A_BAUJ == 2006 ~ 128.0,
                                       A_BAUJ == 2007 ~ 133.7,
                                       A_BAUJ == 2008 ~ 138.9,
                                       A_BAUJ == 2009 ~ 127.3,
                                       A_BAUJ == 2010 ~ 140.5,
                                       A_BAUJ == 2011 ~ 152.2,
                                       A_BAUJ == 2012 ~ 158.9,
                                       A_BAUJ == 2013 ~ 154.9,
                                       A_BAUJ == 2014 ~ 149.3,
                                       A_BAUJ == 2015 ~ 136.9,
                                       A_BAUJ == 2016 ~ 128.1,
                                       A_BAUJ == 2017 ~ 134.7,
                                       A_BAUJ < 2000 ~ 0,
                                       A_BAUJ > 2017 ~ 0))

# create a diesel price category based on data retrieved from the ADAC
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(price.diesel_ADAC = case_when(A_BAUJ == 2000 ~ 156.9,
                                       A_BAUJ == 2001 ~ 160.3,
                                       A_BAUJ == 2002 ~ 83.6,
                                       A_BAUJ == 2003 ~ 88.4,
                                       A_BAUJ == 2004 ~ 93.7,
                                       A_BAUJ == 2005 ~ 106.1,
                                       A_BAUJ == 2006 ~ 110.9,
                                       A_BAUJ == 2007 ~ 116.0,
                                       A_BAUJ == 2008 ~ 132.4,
                                       A_BAUJ == 2009 ~ 107.7,
                                       A_BAUJ == 2010 ~ 121.4,
                                       A_BAUJ == 2011 ~ 141.1,
                                       A_BAUJ == 2012 ~ 147.8,
                                       A_BAUJ == 2013 ~ 142.0,
                                       A_BAUJ == 2014 ~ 135.0,
                                       A_BAUJ == 2015 ~ 116.9,
                                       A_BAUJ == 2016 ~ 107.8,
                                       A_BAUJ == 2017 ~ 116.1,
                                       A_BAUJ < 2000 ~ 0,
                                       A_BAUJ > 2017 ~ 0))

# relation number of cars per person in a household                                        
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(rel_car_person = H_ANZAUTO / H_GR)

# test, if the creation of the new variables caused any NAs
colSums(is.na(data_MiD_Autos))
sum(is.na(data_MiD_Autos))
# --> 6400 NAs because of HH_occupation and HH_education

################################################################################
### transform numeric variables to categorical variables (code retreived from my plotting script)

#create data file for plotting
plot_data <- data_MiD_Autos

##transform discrete variables to categorical variables

# Economic Status Household (oek_status)
plot_data$oek_status <- factor(plot_data$oek_status, 
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("very low", "low", "medium", "high", "very high"))

# Household Income Group (hheink_gr2)
plot_data$hheink_gr2 <- factor(plot_data$hheink_gr2, 
                               levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                               labels = c("<500", "<900", "<1500", "<2000", "<3000", "<4000", "<5000", "<6000", "<=7000", ">7000"))

# Household Type (hhtyp)
plot_data$hhtyp <- factor(plot_data$hhtyp, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 95),
                          labels = c("1P/18-29y", "1P/30-59y", "1P/+60y", "2P/min:18-29y", "2P/min:30-59y", "2P/min:+60y", 
                                     ">=3 adults", ">=1 child<6y", ">=1 child<14y", ">=1 child<18y", "Single Parent", "Other"))

# Household Type 2 (hhtyp2)
plot_data$hhtyp2 <- factor(plot_data$hhtyp2, 
                           levels = c(1, 2, 3, 4, 95),
                           labels = c("young:<35", "family", "only adults", "old:+65", "other"))

# Household Size Group (hhgr_gr)
plot_data$hhgr_gr <- factor(plot_data$hhgr_gr, 
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("1 person", "2 persons", "3 persons", "4 persons", "5 or more persons"))

# Vehicles per Household (anzauto_gr1)
plot_data$anzauto_gr1 <- factor(plot_data$anzauto_gr1, 
                                levels = c(1, 2, 3, 4, 9),
                                labels = c("1 car", "2 cars", "3 cars", "4+ cars", "not specified"))

# Vehicle Usage (A_HALTER)
plot_data$A_HALTER <- factor(plot_data$A_HALTER, 
                             levels = c(1, 2, 3, 9, 101, 202),
                             labels = c("private", "company car", "other", "not specified", "not available", "not collected"))

# english translation for region types
# https://www.bmv.de/SharedDocs/DE/Anlage/G/regiostar-raumtypologie-englisch.pdf?__blob=publicationFile
# Region Type (RegioStaR7)
plot_data$RegioStaR7 <- factor(plot_data$RegioStaR7, 
                               levels = c(71, 72, 73, 74, 75, 76, 77),
                               labels = c("urban: metropolis", "urban: regiopolis", "urban: medium-sized town", "urban: village area", 
                                          "rural: central city", "rural: medium-sized city", "rural: village area"))

# Region Type (RegioStaR4)
plot_data$RegioStaR4 <- factor(plot_data$RegioStaR4, 
                               levels = c(11, 12, 21, 22),
                               labels = c("metropolitan urban region", "regiopolitan urban region", "rural region close to an urban region", "peripheral rural region"))

# Region Type (RegioStaR2)
plot_data$RegioStaR2 <- factor(plot_data$RegioStaR2, 
                               levels = c(1, 2),
                               labels = c("urban region", "rural region"))

# Living Situation (wohnlage)
plot_data$wohnlage <- factor(plot_data$wohnlage, 
                             levels = c(1, 2, 3, 4, 95),
                             labels = c("urban", "suburban", "rural", "other", "not available"))

# distance to the closest Oberzentrum / Mittelzentrum
plot_data$min_ozmz <- factor(plot_data$min_ozmz, 
                             levels = c(1, 2, 3, 4, 5, 95),
                             labels = c('<10 min', '10 - <20 min', '20 - <30 min', '30 - <40min', '>= 40 min', 'not assignable'))

# distance to the closest train station
plot_data$bahn28 <- factor(plot_data$bahn28, 
                           levels = c(1, 2, 3, 4, 5, 6, 95),
                           labels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m', 'not assignable'))

# distance to the closest bus station
plot_data$bus28 <- factor(plot_data$bus28, 
                          levels = c(1, 2, 3, 4, 5, 6, 95),
                          labels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m', 'not assignable'))

# Quality of Public Transport (quali_opnv)
plot_data$quali_opnv <- factor(plot_data$quali_opnv, 
                               levels = c(1, 2, 3, 4, 95),
                               labels = c("very bad", "bad", "good", "very good", "other"))

# Quality of Local Services (quali_nv)
plot_data$quali_nv <- factor(plot_data$quali_nv, 
                             levels = c(1, 2, 3, 4, 95),
                             labels = c("very bad", "bad", "good", "very good", "other"))

# Annual kilometers travelled (jahresfl_gr)
plot_data$jahresfl_gr <- factor(plot_data$jahresfl_gr, 
                                levels = c(1, 2, 3, 4, 5, 6, 7, 94, 99),
                                labels = c("<5000 km", "5000 - <10000 km", "10000 - <15000 km", "15000 - <20000 km", "20000 - <25000 km", "25000 - <50000 km", "50000 km or more", "unplausible", "not specified"))

# vehicle type
plot_data$seg_kba <- factor(plot_data$seg_kba, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 95),
                            labels = c('Mini', 'Kleinwagen', 'Kompaktklasse', 'Mittelklasse', 'obere Mittelklasse', 'Oberklasse', 'Sportgeländewagen', 'Geländewagen', 'Sportwagen', 'Mini-Vans', 'Großraum-Vans', 'Utilities', 'not assignable' ))

# vehicle type grouped
plot_data$seg_kba_gr <- factor(plot_data$seg_kba_gr, 
                               levels = c(1, 2, 3, 4, 95),
                               labels = c('small', 'compact', 'medium', 'big', 'not assignable' ))

# fuel type grouped
plot_data$antrieb <- factor(plot_data$antrieb, 
                            levels = c(1, 2, 3, 94, 99),
                            labels = c('Petrol', 'Diesel', 'Other', 'unplausible', 'not specified'))

# detailed fuel type
plot_data$A_ANTRIEB <- factor(plot_data$A_ANTRIEB, 
                              levels = c(1, 2, 3, 4, 5, 6, 94, 99),
                              labels = c('Petrol', 'Diesel', 'Gas', 'Hybrid', 'Electric', 'Other', 'unplausible', 'not specified'))

# emission class equivalent composed by manufacturing years
plot_data$EUR_old <- factor(plot_data$EUR_old, 
                            levels = c(1, 2, 3, 4),
                            labels = c('before 2006', '2006-2010', '2011-2014', '2015-2017'))

# emission class equivalent composed by manufacturing years
plot_data$EUR <- factor(plot_data$EUR, 
                        levels = c(1, 2, 3),
                        labels = c('EUR 1-4 or else', 'EUR 5', 'EUR 6'))


# combination fuel type and vehicle emission class
plot_data$vehEmissionClass <- factor(plot_data$vehEmissionClass, 
                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                     labels = c('Petrol EUR1-3', 'Petrol EUR4', 'Petrol EUR5', 'Petrol EUR6', 
                                                'Diesel EUR1-3', 'Diesel EUR4', 'Diesel EUR5', 'Diesel EUR6', 'Other'))

# choice variable for first multinominal logit model (Petrol, Diesel differenciated by emission class, other)
plot_data$EUR_Diesel <- factor(plot_data$EUR_Diesel, 
                               levels = c(1, 5, 6, 7, 8, 9),
                               labels = c('Petrol', 'Diesel_EUR_3', 'Diesel_EUR_4', 'Diesel_EUR_5', 'Diesel_EUR_6', 'Other'))

# fuel type categories for a binary logit model
plot_data$Diesel <- factor(plot_data$Diesel, 
                           levels = c(1, 2, 3),
                           labels = c('non_Diesel', 'Diesel', 'None'))

# subsidise year for Abwrackprämie
plot_data$Abwrack_2009 <- factor(plot_data$Abwrack_2009, 
                                 levels = c(0, 1, 2),
                                 labels = c('cars manufactured before 2008', 'cars manufactured from 2008-2009', 'cars manufactured after 2009'))

################################################################################
### data imputation section
## non covered information forming a share of less than 15% can be imputed
# check annual mileage
prop.table(table(plot_data$jahresfl_gr))
#  unplausible  not specified 
# 0.0001844644  0.0383363155 
# < 15% --> impute data

# check car ownership type
prop.table(table(plot_data$A_HALTER))
#not specified not available not collected
# 0.0005580048  0.8967322130  0.0298740108
# 92% > 15% --> data imputation definitley no recommended

# check distance to closest train station
prop.table(table(plot_data$bahn28))
# <NA> 
# 0.19325414 
# > 15% --> since it is only slightly > 15% --> try data imputation

# check distance to closest bus station
prop.table(table(plot_data$bus28))
# 0.19325414 
# > 15% --> since it is only slightly > 15% --> try data imputation

# check fuel type
prop.table(table(plot_data$antrieb))
# unplausible not specified 
# 0.000106067   0.003062109 

# check manufacturing year
prop.table(table(plot_data$bauj_gr))
#gr99 0.02498570

#https://www.appsilon.com/post/imputation-in-r
install.packages("mice")
library(mice)
library(cowplot)

#create NA values for data imputation
data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(A_JAHRESFL_NA = case_when(A_JAHRESFL > 250000 ~ NA,
                                   A_JAHRESFL <= 250000 ~ A_JAHRESFL))

data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(bahn28_NA = case_when(bahn28 == 95 ~ NA,
                               bahn28 < 95 ~ bahn28))

data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(bus28_NA = case_when(bus28 == 95 ~ NA,
                              bus28 < 95 ~ bus28))

data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(seg_kba_gr_NA = case_when(seg_kba_gr == 95 ~ NA,
                                   seg_kba_gr < 95 ~ seg_kba_gr))

data_MiD_Autos <- data_MiD_Autos %>% 
  mutate(seg_kba_NA = case_when(seg_kba == 95 ~ NA,
                                seg_kba < 95 ~ seg_kba))

# check NA creation
table(data_MiD_Autos$A_JAHRESFL)
table(data_MiD_Autos$A_JAHRESFL_NA, useNA = "ifany")
table(data_MiD_Autos$bahn28)
table(data_MiD_Autos$bahn28_NA, useNA = "ifany")
table(data_MiD_Autos$bus28)
table(data_MiD_Autos$bus28_NA, useNA = "ifany")
table(data_MiD_Autos$bus28)
table(data_MiD_Autos$seg_kba_gr_NA, useNA = "ifany")
table(data_MiD_Autos$seg_kba_gr)

# for imputing bahn28 and annual mileage
mice_data <- data.frame(
  data_MiD_Autos$A_JAHRESFL_NA,
  data_MiD_Autos$bahn28_NA,
  data_MiD_Autos$A_BAUJ,
  data_MiD_Autos$seg_kba_NA,
  data_MiD_Autos$A_HALTER,
  data_MiD_Autos$H_ANZAUTO,
  data_MiD_Autos$H_GR,
  data_MiD_Autos$hhtyp2,
  data_MiD_Autos$oek_status,
  data_MiD_Autos$antrieb
)

# for imputing bus28 and annual mileage
mice_data1 <- data.frame(
  data_MiD_Autos$A_JAHRESFL_NA,
  data_MiD_Autos$bus28_NA,
  data_MiD_Autos$A_BAUJ,
  data_MiD_Autos$seg_kba,
  data_MiD_Autos$A_HALTER,
  data_MiD_Autos$H_ANZAUTO,
  data_MiD_Autos$H_GR,
  data_MiD_Autos$hhtyp2,
  data_MiD_Autos$oek_status,
  data_MiD_Autos$antrieb
)

#https://www.appsilon.com/post/imputation-in-r
AJ_pmm <- mice(mice_data, method = "pmm")
AJ_imputed_pmm <- complete(AJ_pmm)
AJ_cart <- mice(mice_data, method = "cart")
AJ_imputed_cart <- complete(AJ_cart)
AJ_lasso <- mice(mice_data, method = "lasso.norm")
AJ_imputed_lasso <- complete(AJ_lasso)

AJ_pmm1 <- mice(mice_data1, method = "pmm")
AJ_imputed_pmm1 <- complete(AJ_pmm1)
AJ_cart1 <- mice(mice_data1, method = "cart")
AJ_imputed_cart1 <- complete(AJ_cart1)
AJ_lasso1 <- mice(mice_data1, method = "lasso.norm")
AJ_imputed_lasso1 <- complete(AJ_lasso1)

# compare the different imputed distributions with histograms 
# for annual mileage
h1 <- ggplot(mice_data, aes(x = data_MiD_Autos.A_JAHRESFL_NA)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(AJ_imputed_pmm, aes(x = data_MiD_Autos.A_JAHRESFL_NA)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(AJ_imputed_cart, aes(x = data_MiD_Autos.A_JAHRESFL_NA)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(AJ_imputed_lasso, aes(x = data_MiD_Autos.A_JAHRESFL_NA)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)
# --> pmm seems to be best

# compare the different imputed distributions with histograms 
# for closest distance to train station
# although this variable is supposed to be a categorical variable, it seems okay
# to use a numeric approach, since the categories are ordered and aggregated numeric values
h1 <- ggplot(mice_data, aes(x = data_MiD_Autos.bahn28_NA)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(AJ_imputed_pmm, aes(x = data_MiD_Autos.bahn28_NA)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(AJ_imputed_cart, aes(x = data_MiD_Autos.bahn28_NA)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, nrow = 2, ncol = 2)
# --> pmm seems to be best

# compare the different imputed distributions with histograms 
# for closest distance to train station
# although this variable is supposed to be a categorical variable, it seems okay
# to use a numeric approach, since the categories are ordered (small to big)
h1 <- ggplot(mice_data, aes(x = data_MiD_Autos.seg_kba_gr_NA)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(AJ_imputed_pmm, aes(x = data_MiD_Autos.seg_kba_gr_NA)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, nrow = 1, ncol = 2)
# --> pmm seems to be best


# check imputation resuslts numerically
table(data_MiD_Autos$A_JAHRESFL_NA)
table(AJ_imputed_pmm$data_MiD_Autos.A_JAHRESFL_NA)
table(AJ_imputed_cart$data_MiD_Autos.A_JAHRESFL_NA)
#> summary(data_MiD_Autos$A_JAHRESFL_NA)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      0    7500   10000   14283   17500  250000    8353 
#> summary(AJ_imputed_pmm$data_MiD_Autos.A_JAHRESFL_NA)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      0    7500   10000   14324   18000  250000 
#> summary(AJ_imputed_cart$data_MiD_Autos.A_JAHRESFL_NA)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      0    7500   10000   14314   18000  250000 

prop.table(table(data_MiD_Autos$bahn28_NA))
#NAs are left out
#1          2          3          4          5          6 
#0.02457442 0.07193977 0.17877762 0.33108301 0.21068607 0.18293910 
prop.table(table(AJ_imputed_pmm$data_MiD_Autos.bahn28_NA))
#1          2          3          4          5          6 
#0.02446459 0.07181199 0.17798048 0.33056944 0.21086588 0.18430761
prop.table(table(AJ_imputed_cart$data_MiD_Autos.bahn28_NA))
#1          2          3          4          5          6 
#0.02433086 0.07129549 0.17691520 0.32931508 0.21192193 0.18622143 
#> summary(data_MiD_Autos$bahn28_NA)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.00    3.00    4.00    4.18    5.00    6.00   41906 
#> summary(AJ_imputed_pmm$data_MiD_Autos.bahn28_NA)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   3.000   4.000   4.184   5.000   6.000 
#> summary(AJ_imputed_cart$data_MiD_Autos.bahn28_NA)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   3.000   4.000   4.192   5.000   6.000 

prop.table(table(data_MiD_Autos$seg_kba_gr_NA))
#NAs are left out
#1         2         3         4 
#0.2419137 0.3378287 0.3023316 0.1179260 
prop.table(table(AJ_imputed_pmm$data_MiD_Autos.seg_kba_gr_NA))
#1         2         3         4 
#0.2427598 0.3374361 0.3017792 0.1180249 

prop.table(table(data_MiD_Autos$bus28_NA))
#NAs are left out
#1          2          3          4          5          6 
#0.45526415 0.26375630 0.10388823 0.07140816 0.06427992 0.04140324 
prop.table(table(AJ_imputed_pmm1$data_MiD_Autos.bus28_NA))
#1          2          3          4          5          6 
#0.45442346 0.26376104 0.10353987 0.07129088 0.06499603 0.04198871 

## tests, when imputation did not work
str(mice_data)
bahn28_polreg$visitSequence
bahn28_polreg$predictorMatrix

# add imputed variables to the plot_data set
plot_data <- cbind(plot_data, AJ_imputed_pmm[, c("data_MiD_Autos.A_JAHRESFL_NA", "data_MiD_Autos.bahn28_NA", "data_MiD_Autos.seg_kba_NA")], AJ_imputed_pmm1[, c("data_MiD_Autos.bus28_NA"), drop = FALSE])
plot_data <- plot_data %>%
  rename(
    A_JAHRESFL_imp = data_MiD_Autos.A_JAHRESFL_NA,
    bahn28_imp = data_MiD_Autos.bahn28_NA,
    bus28_imp = data_MiD_Autos.bus28_NA,
    seg_kba_imp = data_MiD_Autos.seg_kba_NA
  )

## factorize the imputed variables
# distance to the closest train station
plot_data$bahn28_imp <- factor(plot_data$bahn28_imp, 
                               levels = c(1, 2, 3, 4, 5, 6),
                               labels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'))

# distance to the closest bus station
plot_data$bus28_imp <- factor(plot_data$bus28_imp, 
                              levels = c(1, 2, 3, 4, 5, 6),
                              labels = c('< 250 m', '250 - <500 m', '500 - <1000 m', '1000 - <2500 m', '2500 - <5000 m', '>= 5000 m'))

plot_data$seg_kba_imp <- factor(plot_data$seg_kba_imp, 
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                labels = c('Mini', 'Kleinwagen', 'Kompaktklasse', 'Mittelklasse', 'obere Mittelklasse', 'Oberklasse', 'Sportgeländewagen', 'Geländewagen', 'Sportwagen', 'Mini-Vans', 'Großraum-Vans', 'Utilities'))

# vehicle type grouped
plot_data$seg_kba_gr_imp <- factor(plot_data$seg_kba_gr_imp, 
                                   levels = c(1, 2, 3, 4),
                                   labels = c('small', 'compact', 'medium', 'big'))

plot_data = subset(plot_data, select = -c("seg_kba_NA","A_JAHRESFL_NA", "bahn28_NA", "bus28_NA") )
drop <- c("seg_kba_NA","A_JAHRESFL_NA", "bahn28_NA", "bus28_NA")
plot_data <- plot_data[,!(names(plot_data) %in% drop)]
write.csv(plot_data, file = "plot_data.csv", row.names = FALSE)

################################################################################
### work with the Wege Data Set
# create unique car IDs
plot_data <- plot_data %>%
  mutate(car_id = paste0(H_ID, "_", A_ID))

# create unique car IDs
data_MiD_Wege <- data_MiD_Wege %>%
  mutate(car_id = paste0(H_ID, "_", W_WAUTO))

# get the top driver of one car and make that person the owner
main_drivers <- data_MiD_Wege %>%
  filter(W_FMF == 1)  %>%
  filter(wegkm <= 950)  %>%
  group_by(car_id, HP_ID) %>%
  summarise(total_km = sum(wegkm, na.rm = TRUE), .groups = "drop") %>%
  group_by(car_id) %>%
  slice_max(total_km, n = 1, with_ties = FALSE) %>%
  rename(main_driver_id = HP_ID)

# attribute the main_driver in the data_MiD_wege data set
data_MiD_Wege <- data_MiD_Wege %>%
  left_join(main_drivers, by = "car_id") %>%
  mutate(driver_role = ifelse(HP_ID == main_driver_id, "main driver", NA)) %>%
  select(-main_driver_id)

# add information from MiD_Wege to plot_data
plot_data <- plot_data %>%
  left_join(
    data_MiD_Wege %>% select(car_id, P_BIL, HP_SEX, HP_BERUF),  
    by = c("car_id" = "car_id")) 

### variables of MiD_Wege
# education
data_MiD_Wege$P_BIL <- factor(data_MiD_Wege$P_BIL, 
                              levels = c(1, 2, 3, 4, 5, 6, 9, 206),
                              labels = c("no degree", "Hauptschule", "Realschule", "high school", "University", "other", "not specified", "adult prox from 14 years old"))

# occupancy
data_MiD_Wege$HP_BERUF <- factor(data_MiD_Wege$HP_BERUF, 
                                 levels = c(1, 2, 9), 
                                 labels = c("yes (inkl. Azubi)", "no", "not specified"))

# gender
data_MiD_Wege$HP_SEX <- factor(data_MiD_Wege$HP_SEX,
                               levels = c(1, 2, 9),
                               labels = c('male', 'female', 'not specified'))