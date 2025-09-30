# trip purpose distribution across economic household status groups
oek_purpose_distribtion <- legs_week_hh %>%
  group_by(oek_status_gr, next_purpose) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))

library(ggplot2)

ggplot(oek_purpose_distribtion, aes(x = oek_status_gr, y = share, fill = next_purpose)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Household Economic Status",
       y = "Share of Next Purpose",
       fill = "Next Purpose",
       title = "Distribution of Next Purpose by Household Status")

# fleet size distributions across economic household status groups
oek_autos_distribtion <- legs_week_hh %>%
  group_by(oek_status_gr, autos) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))

library(ggplot2)

ggplot(oek_autos_distribtion, aes(x = oek_status_gr, y = share, fill = autos)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Household Economic Status",
       y = "Share of Next Purpose",
       fill = "Next Purpose",
       title = "Distribution of Next Purpose by Household Status")

# household size distribution across economic household status groups
hh_check <- hh %>%
  left_join(
    hh_adapted %>%
      dplyr::select(id, oek_status_gr, RegioStaR2, RegioStaR4, RegioStaR7) %>%
      distinct(id, .keep_all = TRUE),
    by = "id"
  ) %>%
  left_join(
    vv %>%
      filter(type == "DIESEL") %>%
      dplyr::select(id, type, emissionClass) %>%
      distinct(id, .keep_all = TRUE),
    by = "id")

prop.table(table(hh_check$oek_status_gr))
#high         medium      very high very low - low 
#0.35416366     0.39991880     0.14731931     0.09859824

oek_size_distribution <- hh_check %>%
  distinct(id, oek_status_gr, hhSize) %>%   # keep each household only once
  group_by(oek_status_gr, hhSize) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))


library(ggplot2)

ggplot(oek_size_distribtion, aes(x = oek_status_gr, y = share, fill = hhSize)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Household Economic Status",
       y = "Share of Next Purpose",
       fill = "Next Purpose",
       title = "Distribution of Next Purpose by Household Status")

# regional type distribution across economic status household groups
oek_regio_distribtion <- legs_week_hh %>%
  group_by(oek_status_gr, RegioStaR7) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))

oek1_regio_distribtion <- legs1_week_hh %>%
  group_by(oek_status_gr, RegioStaR7) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))


# check if it is income related, how often hosueholds travel in the lez
oek_LEZ_distribtion <- legs_week_hh %>%
  group_by(oek_status_gr, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))

oek1_LEZ_distribtion <- legs1_week_hh %>%
  group_by(oek_status_gr, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))
# no differences could be observed among the income groups, but households travel generally less in the LEZ after the tighter ban

# check if it is regional type realted, how often households travel in the lez
regio_LEZ_distribtion <- legs_week_hh %>%
  group_by(RegioStaR7, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RegioStaR7) %>%
  mutate(share = count / sum(count))

regio1_LEZ_distribtion <- legs1_week_hh %>%
  group_by(RegioStaR7, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RegioStaR7) %>%
  mutate(share = count / sum(count))
# after the ban: metro travel 2 - 4 times more in the LEZ compared to the others


#check if low income households have a lower share of working trips
work_low_oek <- legs_week_hh %>%
  filter(oek_status_gr == "very low - low")

table(work_low_oek$next_purpose)
prop.table(table(work_low_oek$next_purpose))


# check how many trips go into the lez before and after the extended ban
lez_trips <- legs_weekend_hh %>%
  group_by(oek_status_gr, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))

lez_trips1 <- legs1_weekend_hh %>%
  group_by(oek_status_gr, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))

lez_trips <- legs_weekend_hh %>%
  group_by(RegioStaR7, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RegioStaR7) %>%
  mutate(share = count / sum(count))

lez_trips1 <- legs1_weekend_hh %>%
  group_by(RegioStaR7, in_LEZ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RegioStaR7) %>%
  mutate(share = count / sum(count))


# check household size distribution across all car owning households
car_hh <- hh %>%
  filter(autos >0)
prop.table(table(car_hh$hhSize))*100
#1            2            3            4            5            6            7 
#27.416613335 36.322660115 16.811522396 13.472615042  5.525850382  0.366784340  0.070879525 
#8           11 
#0.006881507  0.006193357 

car_hh <- car_hh %>%
left_join(
  hh_adapted %>%
    dplyr::select(id, oek_status_gr, RegioStaR2, RegioStaR4, RegioStaR7) %>%
    distinct(id, .keep_all = TRUE),
  by = "id"
)

oek_size_distribution <- car_hh %>%
  distinct(id, oek_status_gr, hhSize) %>%   # keep each household only once
  group_by(oek_status_gr, hhSize) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(oek_status_gr) %>%
  mutate(share = count / sum(count))

vv_hh <- vv %>%
  left_join(
    hh_adapted %>%
      dplyr::select(id, oek_status_gr, RegioStaR2, RegioStaR4, RegioStaR7, hhtyp2, hhgr_gr2) %>%
      distinct(id, .keep_all = TRUE),
    by = "id"
  )

vv_hh_diesel <- vv_hh %>%
  filter(type == "DIESEL") %>%
  distinct(id, .keep_all = TRUE)

prop.table(table(vv_hh_diesel$hhtyp2))
#    family     old:+65 only adults   young:<35 
#0.32979720  0.10708133  0.47440082  0.08872065 
prop.table(table(vv_hh_diesel$hhgr_gr2))
# 1 person         2 persons 3 or more persons 
#0.1757034         0.3386224         0.4856741 
prop.table(table(vv_hh_diesel$autos,vv_hh_diesel$hhgr_gr2))
#     1 person   2 persons 3 or more persons
#1 0.146643697 0.208223774       0.184891347
#2 0.027065010 0.113352676       0.213316408
#3 0.001994741 0.017045970       0.087466377


# do there exist many households in 72, 75 of RegioStaR7?
legs_regio_test <- legs %>%
  distinct(hhid, .keep_all = TRUE)
prop.table(table(legs_regio_test$RegioStaR7))
prop.table(table(legs_regio_test$RegioStaR4))
