library(readr)
library(dplyr)
library(ggplot2)

calib_long2 <- read.csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//calibration_output_2CHH_20250816//hh_synpop_10pct_2CHH_EUR_all_seeds_calibrated_20250816.csv")
calib_long2 <- read.csv("C://Users//tumme//Documents//Calibrated_Simulation_Data_from_R//calibration_output_1CHH_20250816//hh_synpop_10pct_1CHH_EUR_all_seeds_calibrated_20250816.csv")

calib_long2 <- map2_df(calib_results, 1:length(calib_results), 
                       ~mutate(.x, seed = .y))

calib_long3 <- map2_df(eur_datasets, 1:length(eur_datasets), 
                       ~mutate(.x, seed = .y))

# --- barplots EURO means and standard deviation for RegioStaR2 ---

# Step 1: count how often each EUR occurs within each seed × region
df_counts <- calib_long2 %>%
  count(seed, RegioStaR4, EUR_choice_updated, name = "n")

# Step 2: calculate proportions per seed × region
df_props <- df_counts %>%
  group_by(seed, RegioStaR4) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Step 3: average across seeds (mean and sd of proportions)
df_summary <- df_props %>%
  group_by(RegioStaR4, EUR_choice_updated) %>%
  summarise(
    mean_prop = mean(prop, na.rm = TRUE),
    sd_prop   = sd(prop, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: plot facetted by rural/urban
ggplot(df_summary, aes(x = EUR_choice_updated, y = mean_prop)) +
  geom_col(width = 0.6, fill = "steelblue") +
  geom_errorbar(
    aes(ymin = mean_prop - sd_prop, ymax = mean_prop + sd_prop),
    width = 0.2
  ) +
  facet_wrap(~ RegioStaR4) +
  labs(x = "EURO emission class", y = "% of households") +
  theme_minimal(base_size = 14)

plot <- ggplot(df_summary, aes(x = EUR_choice_updated, y = mean_prop, fill = EUR_choice_updated)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_prop - sd_prop, ymax = mean_prop + sd_prop),
                width = 0.2) +
  #geom_text(aes(label = scales::percent(mean_prop, accuracy = 1)),
  #         vjust = -0.3, size = 3) +
  facet_wrap(~ RegioStaR4, nrow = 2, ncol = 2,
             labeller = labeller(RegioStaR4 = c(
               "11" = "metropolitan urban region",
               "12" = "regiopolitan urban region",
               "21" = "rural region close to an urban region",
               "22" = "peripheral rural region"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5)) +
  scale_fill_manual(values = c("EUR4"="#a6cee3", "EUR5"="#1f78b4", "EUR6"="#08306b")) +
  labs(
    x = "EURO emission class",
    y = "% of households",
    title = "Distribution of EURO emission classes among diesel cars by region type",
    subtitle = "Bars show mean across 10 seeds. Error bars indicate standard deviation."
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("EUR_RegioStaR4_1CHH_Calibrated_20250830.jpg", plot, width = 16, height = 8, dpi = 400)


# --- barplots EURO means and standard deviation for oek_status_gr ---

# Step 1: count EUR classes per seed × oek_status_gr
df_counts <- calib_long2 %>%
  #filter(seed == 1) %>%
  count(seed, oek_status_gr, EUR_choice_updated, name = "n")

# Step 2: calculate proportions within each seed × oek_status_gr
df_props <- df_counts %>%
  group_by(seed, oek_status_gr) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Step 3: summarise across seeds (mean ± sd)
df_summary <- df_props %>%
  group_by(oek_status_gr, EUR_choice_updated) %>%
  summarise(
    mean_prop = mean(prop, na.rm = TRUE),
    sd_prop   = sd(prop, na.rm = TRUE),
    .groups = "drop"
  )

df_summary <- df_summary %>%
  mutate(oek_status_gr = factor(oek_status_gr,
                                levels = c("very low - low", "medium",
                                           "high", "very high")))

# Step 4: plot, one facet per oek_status_gr
ggplot(df_summary, aes(x = EUR_choice_updated, y = mean_prop)) +
  geom_col(width = 0.6, fill = "steelblue") +
  geom_errorbar(
    aes(ymin = mean_prop - sd_prop, ymax = mean_prop + sd_prop),
    width = 0.2
  ) +
  facet_wrap(~ oek_status_gr) +
  labs(
    x = "EURO emission class",
    y = "Mean and standard deviation across 10 seeds",
    title = "Distribution of EURO emission classes by economic household status (very low - very high)"
  ) +
  theme_minimal(base_size = 14)

plot <- ggplot(df_summary, aes(x = EUR_choice_updated, y = mean_prop, fill = EUR_choice_updated)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_prop - sd_prop, ymax = mean_prop + sd_prop),
                width = 0.2) +
  #geom_text(aes(label = scales::percent(mean_prop, accuracy = 1)),
   #         vjust = -0.3, size = 3) +
  facet_wrap(~ oek_status_gr, nrow = 2, ncol = 2,
             labeller = labeller(oek_status_gr = c(
               "very low - low" = "Income group: Very Low – Low",
               "medium" = "Income group: Medium",
               "high" = "Income group: High",
               "very high" = "Income group: Very High"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5)) +
  scale_fill_manual(values = c("EUR4"="#a6cee3", "EUR5"="#1f78b4", "EUR6"="#08306b")) +
  labs(
    x = "EURO emission class",
    y = "% of households",
    title = "Distribution of EURO emission classes among diesel cars by economic household status",
    subtitle = "Bars show mean across 10 seeds. Error bars indicate standard deviation."
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("EUR_oek_1CHH_Calibrated_20250830.jpg", plot, width = 16, height = 8, dpi = 400)


# --- plot interaction region type and economic household status ---
# relation EUR, fuel type, economic household status
calib_long2_6 <- calib_long2 %>%
  filter(seed == 6)

plot_data_oek_regiostar2_EUR <- calib_long2_6 %>%
  group_by(RegioStaR2, EUR_choice_updated, oek_status_gr) %>%
  summarise(weighted_count = sum(na.rm = TRUE), .groups = "drop") %>%
  group_by(RegioStaR2, EUR_choice_updated) %>%
  mutate(relative_share = weighted_count / sum(weighted_count)) %>%
  ungroup()

p1 <- ggplot(plot_data_oek_regiostar2_EUR, aes(x = 1, y = relative_share, fill = oek_status_gr)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  facet_grid(EUR_choice_updated ~ RegioStaR2, scales = "free_y", space = "free", switch = "y") +
  scale_fill_manual(values = c("very low - low" = "lightpink2",
                               "medium" = "lightblue1", 
                               "high" = "lightblue3", 
                               "very high" = "lightblue4")) +
  coord_flip() +
  scale_y_continuous(
    name = "Relative Share",
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent_format(accuracy = 1)  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_text(angle = 0, size = 11),
    strip.text.x = element_text(size = 11),
    panel.spacing = unit(1, "lines"),  # Increased spacing
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Relative Share of the economic household status per emission class and fuel type",
    fill = "Economic household status"
  )

ggsave("test.png", plot = p1, width = 16, height = 6, dpi = 300, bg = "white") 