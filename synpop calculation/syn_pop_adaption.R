install.packages("splitstackshape")
library(splitstackshape)
library(readr)
library(psych)
library(dplyr)

# read original synpop files
hh_synpop <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//hh_2022_20231229.csv")
pp_synpop <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//pp_2022_20231229.csv")
pp_MiD <- read.csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//MiD2017_Personen.csv", sep = ';')
zz_synpop <- read.csv("Z://Masterarbeit//Data_Research//zones_20231212_bus_rail_500_500_2500.csv", sep = ';')
vv_MiD <- read_csv("C://Users//tumme//Documents//plot_data.csv")
plot_data <- read_csv("C://Users//tumme//OneDrive - TUM//Dokumente//MA//plot_data.csv")

pp_synpop <- pp_synpop %>%
  left_join(
    zz_synpop %>% select(id, JUR_NAME), 
    by = c("homeZone" = "id")
  )

hh_synpop <- hh_synpop %>%
  left_join(
    zz_synpop %>% select(id, JUR_NAME), 
    by = c("zone" = "id")
  )

################################################################################
# check received input files
colnames(pp_MiD)
colnames(hh_synpop)
colnames(pp_synpop)
#[1] "hhid"                 "id"                   "age"                  "gender"              
#[5] "relationShip"         "race"                 "occupation"           "driversLicense"      
#[9] "workplace"            "income"               "nationality"          "education"           
#[13] "homeZone"             "schoolId"             "disability"           "jobType"             
#[17] "jobDuration"          "jobStartTimeWorkdays" "jobStartTimeWeekends" "zone" 

describe(pp_synpop)

#check how many hhids have a member with a drivers license
pp_synpop_dl <- pp_synpop %>%
  filter(driversLicense == TRUE)
common_ids <- intersect(hh_synpop$id, pp_synpop_dl$hhid)
percent_common <- length(common_ids) / length(unique(hh_synpop$id)) * 100
cat(sprintf("Percentage of households with a dirvers license: %.2f%%\n", percent_common))
# 92.48%

# add hhsize to pp_synpop
pp_synpop <- pp_synpop %>%
  left_join(
    hh_synpop %>% select(id, hhSize),  
    by = c("hhid" = "id"))

# check relation occupation and jobType
df_counts <- pp_synpop %>%
  group_by(occupation, jobType) %>%
  summarise(OccCount = n(), .groups = "drop")

df_shares <- df_counts %>%
  group_by(jobType) %>%
  mutate(Share = OccCount / sum(OccCount)) %>%
  ungroup()
# noInfo: students 25%, unemployed 75%

################################################################################
# check distribution of important variables
pp_synpop$age_group <- cut(pp_synpop$age,
                           breaks = c(0, 18, 29, 39, 49, 59, 69, Inf),
                           labels = c("0-18", "19-29", "30-39", "40-49", "50-59", "60-69", "76+"),
                           right = FALSE)

pp_synpop$income_group <- cut(pp_synpop$income,
                              breaks = c(0, 1500, 5600, Inf),
                              labels = c("0-1500", "1501-5600", "5601+"),
                              right = FALSE)

pp_synpop$income_gr2 <- cut(pp_synpop$income / 5,
                            breaks = c(0, 499, 899, 1499, 1999, 2999, 3999, 4999, 5999, 6999, Inf),
                            labels = c(">500", "500->900", "900-<1500", "1500-<2000", "2000-<3000", "3000-<4000", "4000-<5000", "5000-<6000", "6000-<7000", "7000+"),
                            right = FALSE)

library(ggplot2)
ggplot(pp_synpop, aes(x = income_gr2)) +
  geom_bar(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "income distribution",
       x = "income",
       y = "Count") +
  theme_minimal()

ggplot(pp_MiD, aes(x = hheink_gr2)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "income distribution",
       x = "income",
       y = "Count") +
  theme_minimal()

## adapt income distribution because currently it does not represent the monthly net income
# https://stadt.muenchen.de/dam/jcr:52aeb1bd-840e-45a3-91dd-62503582dfef/Expertise_Reichtum_und_Verteilung_Muenchen_2022_final.pdf

# define target values
median_income <- 2700
target_props <- c(0.19, 0.43, 0.26, 0.12)
bounds <- c(0, 0.6, 1.2, 2.0, Inf)


# loss_fun(sigma):
# 1. Fix the log-mean (mu) so that the lognormal distribution has the desired median.
# 2. Generate a large sample (500,000) from a lognormal distribution 
#    with the given spread parameter sigma.
# 3. Define absolute income class boundaries by scaling the relative bounds with the median income.
# 4. Bin the simulated sample into these income classes.
# 5. Calculate the proportion of simulated incomes in each class.
# 6. Compare these simulated proportions to the target proportions.
# 7. Return the sum of squared differences between simulated and target proportions 
#    (smaller = better fit).
loss_fun <- function(sigma) {
  mu <- log(median_income)
  x <- rlnorm(5e5, meanlog = mu, sdlog = sigma)
  breaks_abs <- bounds * median_income
  props <- as.numeric(prop.table(table(cut(x, breaks = breaks_abs, include.lowest = TRUE))))
  sum((props - target_props)^2)
}

#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html
opt <- optim(par = 0.5, fn = loss_fun, method = "Brent", lower = 0.05, upper = 2)
opt_sigma <- opt$par

# map pp_synpop
mu <- log(median_income)
n <- length(pp_synpop$income)
u <- rank(pp_synpop$income, ties.method = "random") / (n + 1)

# map ranks on the optimized target distribution
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Lognormal.html
income_mapped <- qlnorm(u, meanlog = mu, sdlog = opt_sigma)

# check mapping
windows(width = 10, height = 7)
hist(income_mapped, breaks = 500, main = "Quantile Mapped", xlab = "Income")
breaks_abs <- bounds * median_income
group <- cut(income_mapped, breaks = breaks_abs, include.lowest = TRUE)
round(prop.table(table(group)), 4)
# looks reasonable

pp_synpop <- pp_synpop %>%
  mutate(income_adapted = income_mapped)

# Plot before/after comparison
par(mfrow = c(1, 2))
hist(pp_synpop$income, breaks = 100, main = "Original", xlab = "Income")
hist(income_mapped, breaks = 50, main = "Quantile Mapped", xlab = "Income")
# looks reasonable

# Set seed for reproducibility
set.seed(123)

# Stratified Sampling
hh_synpop_10pct <- stratified(hh_synpop, group = c("JUR_NAME"), size = 0.1, replace = FALSE)
pp_synpop_10pct <- pp_synpop %>%
  filter(hhid %in% hh_synpop_10pct$id)

prop.table(table(pp_synpop$relationShip))
prop.table(table(pp_synpop_10pct$relationShip))
prop.table(table(pp_synpop$age_group))
prop.table(table(pp_synpop_10pct$age_group))
prop.table(table(pp_synpop$income_gr2))
prop.table(table(pp_synpop_10pct$income_gr2))
summary(pp_synpop$income_adapted)
summary(pp_synpop_10pct$income_adapted)
prop.table(table(pp_synpop$gender))
prop.table(table(pp_synpop_10pct$gender))
prop.table(table(pp_synpop$occupation))
prop.table(table(pp_synpop_10pct$occupation))
prop.table(table(pp_synpop$jobType))
prop.table(table(pp_synpop_10pct$jobType))
prop.table(table(pp_synpop$education))
prop.table(table(pp_synpop_10pct$education))
prop.table(table(pp_synpop$driversLicense))
prop.table(table(pp_synpop_10pct$driversLicense))
prop.table(table(pp_synpop$JUR_NAME))
prop.table(table(pp_synpop_10pct$JUR_NAME))
#distribution looks very similar, although only households were filtered according to zones

pp_synpop_10pct_withNonCarOwners <- pp_synpop_10pct
hh_synpop_10pct_withNonCarOwners <- hh_synpop_10pct


# get housholds owning at least one car
hh_synpop_10pct <- hh_synpop_10pct_withNonCarOwners %>%
  filter(autos >= 1)
prop.table(table(hh_synpop$autos))
#0          1          2          3 
#0.19364088 0.55247524 0.20488622 0.04899766 
prop.table(table(hh_synpop_10pct$autos))
#1          2          3 
#0.68762838 0.25250934 0.05986228 

# get only persons of households owning a car
pp_synpop_10pct <- pp_synpop %>%
  filter(hhid %in% hh_synpop_10pct$id)

################################################################################
### add new variables to the hh_synpop
##create occupation status on household level
occupation_status <- pp_synpop_10pct %>%
  # Filter to licensed people only
  filter(driversLicense == TRUE) %>%
  
  group_by(hhid) %>%
  summarise(
    n_members = n(),
    n_full_time = sum(jobType == 'fullTime'),    # all HH members are full-time working
    n_working = sum(jobType %in% c('fullTime', 'partTime', 'noInfo')),  # at least one HH member is working
    n_nonworking = sum(occupation == 2 | occupation == 4),   # no one in the HH is working (retired, unemployed, student without job,...)
    .groups = "drop"
  ) %>%
  
  mutate(HH_occupation = case_when(
    n_full_time == n_members ~ "full time working",
    n_working > 0 & n_working == n_members & n_nonworking != n_members ~ "partly working", 
    n_nonworking == n_members ~ "no one is working",
    TRUE ~ NA_character_
  )) %>%
  select(hhid, HH_occupation)

# add HH_occupation to the pp file to check the assignment
pp_synpop_10pct <- pp_synpop_10pct %>%
  left_join(
    occupation_status %>% select(hhid, HH_occupation),  
    by = "hhid")

# add HH_occupation to the hh file
hh_synpop_10pct <- hh_synpop_10pct %>%
  left_join(
    occupation_status %>% select(hhid, HH_occupation),  
    by = c("id" = "hhid"))
table(hh_synpop_10pct$HH_occupation)
# 92.48% are covered with occupation --> all households with a drivers license have an occupation status


##create education level on household level
# all households with at least one member having a degree from the University or Fachhochschule = 1
education_status <- pp_synpop_10pct %>%
  filter(driversLicense == TRUE) %>%   # licensed members only
  group_by(hhid) %>%
  summarise(
    has_university = any(education == 3 | education == 4),
    .groups = "drop"
  ) %>%
  mutate(HH_higher_education = if_else(has_university, 1, 0)) %>%
  select(hhid, HH_higher_education)

# add education status to the pp file
pp_synpop_10pct <- pp_synpop_10pct %>%
  left_join(
    education_status %>% select(hhid, HH_higher_education),  
    by = "hhid")

# add education status to the hh file
hh_synpop_10pct <- hh_synpop_10pct %>%
  left_join(
    education_status %>% select(hhid, HH_higher_education),  
    by = c("id" = "hhid"))

##create aggregated gender on household level
gender <- pp_synpop_10pct %>%
  filter(driversLicense == TRUE) %>%
  group_by(hhid) %>%
  summarise(
    hh_size = n(),
    male_count = sum(gender == 1),
    female_count = sum(gender == 2),
    HH_gender = case_when(
      male_count > female_count ~ "mostly male",
      female_count > male_count ~ "mostly female",
      male_count == female_count ~ "equal number",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

#add gender to the pp file
pp_synpop_10pct <- pp_synpop_10pct %>%
  left_join(
    gender %>% select(hhid, HH_gender),  
    by = "hhid")

#add gender to the hh file
hh_synpop_10pct <- hh_synpop_10pct %>%
  left_join(
    gender %>% select(hhid, HH_gender),  
    by = c("id" = "hhid"))

##create aggregated age on household level(equivalent to hhtyp2)
age <- pp_synpop_10pct %>%
  group_by(hhid) %>%
  summarise(
    hh_size = n(),
    child = sum(age < 18),
    young = sum(age < 35),
    old = sum(age >= 65),
    adult = sum(age >= 18 & age < 65),
    HH_age = case_when(
      child > 0 ~ "family",
      young == hh_size ~ "young",
      old == hh_size ~ "old",
      adult == hh_size ~ "adult",
      young < hh_size & adult < hh_size & child == 0 ~ "adult",
      old < hh_size & adult < hh_size & child == 0 ~ "adult",
      old < hh_size & young < hh_size & child == 0 ~ "adult",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

#add hh_age to the pp file
pp_synpop_10pct <- pp_synpop_10pct %>%
  left_join(
    age %>% select(hhid, HH_age),  
    by = c("hhid" = "hhid"))

#add age to the hh file
hh_synpop_10pct <- hh_synpop_10pct %>%
  left_join(
    age %>% select(hhid, HH_age),  
    by = c("id" = "hhid"))

## create updated csv files
#write.csv(pp_synpop, file = "pp_2022_20250710_ST.csv", row.names = FALSE)
#write.csv(hh_synpop, file = "hh_2022_20250710_ST.csv", row.names = FALSE)

################################################################################
## create economic household status
# calculate weighted household size
hh_weights <- pp_synpop_10pct %>%
  group_by(hhid) %>%
  summarise(
    hh_size_weighted = case_when(
      sum(age >= 14) == 0 ~ sum(age < 14) * 0.3,
      TRUE ~ 1 + (sum(age >= 14) - 1) * 0.5 + sum(age < 14) * 0.3
    )
  )

pp_synpop_10pct <- pp_synpop_10pct %>%
  group_by(hhid) %>%
  summarise(
    hh_income = sum(income_adapted)) %>%
  ungroup()
summary(pp_synpop_10pct$hh_income)

# add weighted household size and household income to the household data set
economic <- hh_weights %>%
  inner_join(
    pp_synpop_10pct %>% select(hhid, hh_income),
    by = "hhid"
  )

economic <- economic %>%
  mutate(hh_income_adapted = hh_income*0.52)

# Define economic status classification function according to https://www.mobilitaet-in-deutschland.de/archive/pdf/MiD2017_Nutzerhandbuch.pdf
get_economic_status <- function(net_income, hh_size_weighted) {
  dplyr::case_when(
    hh_size_weighted >= 3.0 & net_income < 4000 ~ "very low - low",
    hh_size_weighted >= 3.0 & net_income < 5600 ~ "medium",
    hh_size_weighted >= 3.0 & net_income <= 7000 ~ "high",
    hh_size_weighted >= 3.0 & net_income > 7000 ~ "very high",
    
    hh_size_weighted >= 2.7 & net_income < 4000 ~ "very low - low",
    hh_size_weighted >= 2.7 & net_income < 5000 ~ "medium",
    hh_size_weighted >= 2.7 & net_income <= 7000 ~ "high",
    hh_size_weighted >= 2.7 & net_income > 7000 ~ "very high",
    
    hh_size_weighted >= 2.5 & net_income < 4000 ~ "very low - low",
    hh_size_weighted >= 2.5 & net_income < 4600 ~ "medium",
    hh_size_weighted >= 2.5 & net_income <= 7000 ~ "high",
    hh_size_weighted >= 2.5 & net_income > 7000 ~ "very high",
    
    hh_size_weighted >= 2.3 & net_income < 3600 ~ "very low - low",
    hh_size_weighted >= 2.3 & net_income < 4600 ~ "medium",
    hh_size_weighted >= 2.3 & net_income < 6600 ~ "high",
    hh_size_weighted >= 2.3 & net_income >= 6600 ~ "very high",
    
    hh_size_weighted >= 2.0 & net_income < 2600 ~ "very low - low",
    hh_size_weighted >= 2.0 & net_income < 4000 ~ "medium",
    hh_size_weighted >= 2.0 & net_income < 6000 ~ "high",
    hh_size_weighted >= 2.0 & net_income >= 6000 ~ "very high",
    
    hh_size_weighted >= 1.6 & net_income < 2600 ~ "very low - low",
    hh_size_weighted >= 1.6 & net_income < 3600 ~ "medium",
    hh_size_weighted >= 1.6 & net_income < 5600 ~ "high",
    hh_size_weighted >= 1.6 & net_income >= 5600 ~ "very high",
    
    hh_size_weighted >= 1.5 & net_income < 2000 ~ "very low - low",
    hh_size_weighted >= 1.5 & net_income < 3600 ~ "medium",
    hh_size_weighted >= 1.5 & net_income < 5600 ~ "high",
    hh_size_weighted >= 1.5 & net_income >= 5600 ~ "very high",
    
    hh_size_weighted >= 1.3 & net_income < 2000 ~ "very low - low",
    hh_size_weighted >= 1.3 & net_income < 3000 ~ "medium",
    hh_size_weighted >= 1.3 & net_income < 5000 ~ "high",
    hh_size_weighted >= 1.3 & net_income >= 5000 ~ "very high",
    
    hh_size_weighted >= 1.0 & net_income < 1500 ~ "very low - low",
    hh_size_weighted >= 1.0 & net_income < 2600 ~ "medium",
    hh_size_weighted >= 1.0 & net_income < 4000 ~ "high",
    hh_size_weighted >= 1.0 & net_income >= 4000 ~ "very high",
    
    TRUE ~ NA_character_
  )
}

# classify the economic household status
economic <- economic %>%
  mutate(
    economic_status = get_economic_status(hh_income_adapted, hh_size_weighted)
  )

prop.table(table(economic$economic_status))

# add economic household status to the hh_synpop
hh_synpop_10pct <- hh_synpop_10pct %>%
  left_join(
    economic %>% select(hhid, economic_status),  
    by = c("id" = "hhid"))

# check the economic status distribution
prop.table(table(hh_synpop_10pct$economic_status))
# target distribution
#      high        low     medium  very high   very low 
#0.41122189 0.07063603 0.37962314 0.11392061 0.02459833 
# current distribution
#     high         medium      very high very low - low 
#0.2914452      0.2250769      0.3765558      0.1069222 
prop.table(table(pp_MiD$oek_status))


## add spatial information to the hh_synpop
hh_synpop_10pct <- hh_synpop_10pct %>%
  left_join(
    zz_synpop %>% select(id, RegioStaR2, RegioStaR4, RegioStaR7, bus_500, rail_500, rail_2500, JUR_NAME),  
    by = c("zone" = "id"))

#hh_synpop_10pct_2CHH <- hh_synpop_10pct_2CHH %>%
#  left_join(
#    zz_synpop %>% select(id, RegioStaR2),  
#    by = c("zone" = "id"))

hh_synpop_10pct$RegioStaR7_gr <- factor(hh_synpop_10pct$RegioStaR7, 
                                   levels = c(71, 72, 73, 74, 75, 76, 77),
                                   labels = c("urban: metropolis-regiopolis", "urban: metropolis-regiopolis", "urban: medium-sized city to small town area", "urban: medium-sized city to small town area", 
                                              "rural: central city", "rural: medium-sized city", "rural: village area"))


hh_synpop_10pct$RegioStaR7_gr2 <- factor(hh_synpop_10pct$RegioStaR7, 
                                    levels = c(71, 72, 73, 74, 75, 76, 77),
                                    labels = c("urban: metropolis-regiopolis", "urban: metropolis-regiopolis", "urban: medium-sized city to small town area", "urban: medium-sized city to small town area", 
                                               "rural area", "rural area", "rural area"))

hh_synpop_10pct$RegioStaR2 <- factor(hh_synpop_10pct$RegioStaR2, 
                               levels = c(1, 2),
                               labels = c("urban region", "rural region"))

## write hh population file
#write.csv(hh_synpop_10pct, file = "hh_synpop_10pct_20250717_ST.csv", row.names = FALSE)
################################################################################
###synchronize all variables and their levels with the model's expression
#hh_synpop_10pct <- read_csv("Z://Masterarbeit//simulation//hh_synpop_10pct_20250717_ST.csv")

## transform variables for HH_age
hh_synpop_10pct <- hh_synpop_10pct %>% rename(hhtyp2 = HH_age)
hh_synpop_10pct$hhtyp2 <- factor(hh_synpop_10pct$hhtyp2,
                                 levels = c("young", "family", "adult", "old"),
                                 labels = c("young:<35", "family", "only adults", "old:+65"))

hh_synpop_10pct$young_household <- ifelse(hh_synpop_10pct$hhtyp2 == "young:<35",1,0)
hh_synpop_10pct$family <- ifelse(hh_synpop_10pct$hhtyp2 == "family",1,0)
hh_synpop_10pct$only_adult_household <- ifelse(hh_synpop_10pct$hhtyp2 == "only adults",1,0)
hh_synpop_10pct$elderly_people_household <- ifelse(hh_synpop_10pct$hhtyp2 == "old:+65",1,0)

## transform variables for hhsize
hh_synpop_10pct$single_HH <- ifelse(hh_synpop_10pct$hhSize == 1,1,0)

hh_synpop_10pct$hhgr_gr2 <- factor(hh_synpop_10pct$hhSize, 
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 11),
                              labels = c("1 person", "2 persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons"))

hh_synpop_10pct$hhgr_gr3 <- factor(hh_synpop_10pct$hhSize, 
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 11),
                              labels = c("1-2 persons", "1-2 persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons", "3 or more persons"))

# create dummy variables for HH_occupation
hh_synpop_10pct$full_time_working <- ifelse(hh_synpop_10pct$HH_occupation == "full time working",1,0)
hh_synpop_10pct$part_time_working <- ifelse(hh_synpop_10pct$HH_occupation == "partly working",1,0)
hh_synpop_10pct$non_working <- ifelse(hh_synpop_10pct$HH_occupation == "no one is working",1,0)

# adapt access to PuT variables
hh_synpop_10pct <- hh_synpop_10pct %>% rename(bahn28_impgr = rail_2500)
hh_synpop_10pct$bahn28_impgr <- factor(hh_synpop_10pct$bahn28_impgr, 
                                   levels = c(0,1),
                                   labels = c("up to 2500 m", "more than 2500 m"))

hh_synpop_10pct <- hh_synpop_10pct %>% rename(bahn28_impgr2 = rail_500)
hh_synpop_10pct$bahn28_impgr2 <- factor(hh_synpop_10pct$bahn28_impgr2, 
                                       levels = c(0,1),
                                       labels = c("up to 500 m", "more than 500 m"))

hh_synpop_10pct <- hh_synpop_10pct %>% rename(bus28_impgr2 = bus_500)
hh_synpop_10pct$bus28_impgr2 <- factor(hh_synpop_10pct$bus28_impgr2, 
                                       levels = c(0,1),
                                       labels = c("up to 500 m", "more than 500 m"))

# rename and refactor HH_education
hh_synpop_10pct <- hh_synpop_10pct %>% rename(HH_education = HH_higher_education)
hh_synpop_10pct$HH_education <- factor(hh_synpop_10pct$HH_education, 
                                       levels = c(0,1),
                                       labels = c("else", "university"))


# rename variable economic status
hh_synpop_10pct <- hh_synpop_10pct %>% rename(oek_status_gr = economic_status)
hh_synpop_10pct <- hh_synpop_10pct %>% rename(H_ANZAUTO = autos)


# create binary variables for the EUR model
hh_synpop_10pct$higher_education = ifelse(hh_synpop_10pct$HH_education == "university", 1, 0)
hh_synpop_10pct$bus_500 = ifelse(hh_synpop_10pct$bus28_impgr2 == "more than 500 m", 1, 0)
hh_synpop_10pct$hh_gr3 = ifelse(hh_synpop_10pct$hhgr_gr2 == "3 or more persons", 1, 0)
hh_synpop_10pct$hh_gr22 = ifelse(hh_synpop_10pct$hhgr_gr2 == "2 persons", 1, 0)
hh_synpop_10pct$RegioStaR2_bin = ifelse(hh_synpop_10pct$RegioStaR2 == "urban region", 1, 0)

# write hh population file
#write.csv(hh_synpop_10pct, file = "hh_synpop_10pct_20250723_ST.csv", row.names = FALSE)
write.csv(hh_synpop_10pct, file = "hh_synpop_10pct_20250815_ST.csv", row.names = FALSE)

#write population and household files without cars
hh_synpop_10pct_NonCarOwners <- hh_synpop_10pct_withNonCarOwners %>%
  filter(autos == 0)

pp_synpop_10pct_NonCarOwners <- pp_synpop %>%
  filter(hhid %in% hh_synpop_10pct_NonCarOwners$id)

write.csv(pp_synpop_10pct_NonCarOwners, file = "pp_synpop_10pct_20250815_ST_nonCarOwners.csv", row.names = FALSE)
write.csv(hh_synpop_10pct_NonCarOwners, file = "hh_synpop_10pct_20250815_ST_nonCarOwners.csv", row.names = FALSE)


# --- filter logsums for plotting
logsum_base_EUR4 <- read_csv("C://Users//tumme//Documents//logsumTable_SHOPPING_EURO4_base.csv")
logsum_base_EUR4_790 <- logsum_base_EUR4 %>%
  filter(origin == 790)
write.csv(logsum_base_EUR4_790, file = "logsumTable_SHOPPING_EURO4_base_790.csv", row.names = FALSE)
