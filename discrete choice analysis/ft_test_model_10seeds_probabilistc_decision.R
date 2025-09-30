library(caret)
library(pROC)
library(MLmetrics)

#glm_antrieb <- glm_Diesel_1CHH_implementation_final_gender_withoutmileage
#glm_antrieb <- `glm_Diesel_1st_2-5CHH_implementation_final_gender_withoutmileage`
glm_antrieb <- `glm_Diesel_2nd_2-5CHH_implementation_final_gender_withoutmileage`

# Omnibus and Chi-square test for model comparison
#https://bjoernwalther.com/binaer-logistische-regression-in-r-rechnen-und-interpretieren/
# Omnibus-Test to check if the model with coefficients performs better than the null-model (chisqp > 0.05 --> performs better!)
modelchi <- glm_antrieb$null.deviance - glm_antrieb$deviance
chidf <- glm_antrieb$df.null - glm_antrieb$df.residual
chisqp <- 1 - pchisq(modelchi, chidf)
chisqp

n <- length(glm_antrieb$residuals)
# Cox-Snell R^2
R2cs <- 1-exp((glm_antrieb$deviance-glm_antrieb$null.deviance)/n)
# Nagelkerke R^2
R2n <- R2cs/(1-exp(-(glm_antrieb$null.deviance/n)))

# Store results across refits
results <- data.frame()

# Matrix for per-car predicted probabilities
prob_matrix <- matrix(NA, nrow = nrow(test), ncol = 10)
colnames(prob_matrix) <- paste0("seed_", 1:10)

# Matrix for per-car binary decisions
decision_matrix <- matrix(NA, nrow = nrow(test), ncol = 10)
colnames(decision_matrix) <- paste0("seed_", 1:10)

seeds <- 1:10

for (s_idx in seq_along(seeds)) {
  s <- seeds[s_idx]
  set.seed(s)
  
  # ---- Bootstrap sample from training data ----
  boot_idx <- sample(nrow(train), replace = TRUE)
  train_boot <- train[boot_idx, ]
  
  # ---- Refit the model ----
  glm_boot <- glm(formula(glm_antrieb), data = train_boot, family = binomial)
  
  # ---- Predict probabilities on test set ----
  P_boot <- predict(glm_boot, newdata = test, type = "response")
  prob_matrix[, s_idx] <- P_boot
  
  # ---- Generate binary decisions ----
  decisions <- rbinom(length(P_boot), 1, P_boot)
  decision_matrix[, s_idx] <- decisions
  
  # Factor version for metrics
  decisions_factor <- factor(ifelse(decisions == 1, "Diesel", "non_Diesel"),
                             levels = c("non_Diesel", "Diesel"))
  true_choice <- factor(test$Diesel, levels = c("non_Diesel", "Diesel"))
  
  # ---- Log-loss ----
  y_true <- ifelse(true_choice == "Diesel", 1, 0)
  log_loss <- LogLoss(P_boot, y_true)
  
  # ---- Confusion matrix ----
  cm <- caret::confusionMatrix(decisions_factor, true_choice)
  acc <- cm$overall["Accuracy"]
  
  cm_table <- cm$table
  TP <- cm_table["Diesel", "Diesel"]
  FP <- cm_table["Diesel", "non_Diesel"]
  FN <- cm_table["non_Diesel", "Diesel"]
  TN <- cm_table["non_Diesel", "non_Diesel"]
  
  error_rate <- ((FP + FN) / (TP + FP + TN + FN))
  
  # ---- AUC ----
  roc_obj <- pROC::roc(true_choice, P_boot, levels = c("non_Diesel", "Diesel"), direction = "<")
  auc_value <- pROC::auc(roc_obj)
  
  # ---- Chi-square & pseudo-R² ----
  modelchi <- glm_boot$null.deviance - glm_boot$deviance
  chidf <- glm_boot$df.null - glm_boot$df.residual
  chisqp <- 1 - pchisq(modelchi, chidf)
  
  n <- length(glm_boot$residuals)
  R2cs <- 1 - exp((glm_boot$deviance - glm_boot$null.deviance) / n)
  R2n <- R2cs / (1 - exp(-(glm_boot$null.deviance / n)))
  
  # ---- Store metrics ----
  results <- rbind(results, data.frame(
    seed = s,
    accuracy = round(acc, 3),
    auc = round(as.numeric(auc_value), 3),
    log_loss = round(as.numeric(log_loss), 3),
    chi_square = round(modelchi, 3),
    chi_df = chidf,
    chi_p = round(chisqp, 5),
    R2_CoxSnell = round(R2cs, 3),
    R2_Nagelkerke = round(R2n, 3),
    TP = TP,
    FP = FP,
    TN = TN,
    FN = FN,
    error_rate = error_rate
  ))
}

#write.csv(results, "confusion_stats_1CHH_EUR_implementation_clean.csv", row.names = FALSE)
#write.csv(results, "confusion_stats_1st_2_5CHH_EUR_implementation_clean.csv", row.names = FALSE)
write.csv(results, "confusion_stats_2nd_2_5CHH_EUR_implementation_clean.csv", row.names = FALSE)

# ---- Per-car statistics ----
test$prob_mean <- rowMeans(prob_matrix)
test$prob_sd   <- apply(prob_matrix, 1, sd)
test$decision_mean <- rowMeans(decision_matrix)
test$decision_sd   <- apply(decision_matrix, 1, sd)

# diesel share range across 10 seeds
decision_mean_seed <- colMeans(decision_matrix)
ggplot(results, aes(x = "", y = decision_mean_seed)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Diesel share across 10 seeds testing", y = "Diesel share", x = "") +
  theme_minimal()


# --- Visualization: Accuracy, error rate and AUC Distributions ---
# Boxplot for accuracy#
box_acc <- ggplot(results, aes(x = "", y = accuracy)) +
  geom_boxplot(fill = "#4C9AFF", color = "black", alpha = 0.6) +  
  geom_jitter(width = 0.1, alpha = 0.5, color = "#1B4F72") +      
  labs(title = "Accuracy across 10 seeds", 
       y = "Accuracy", 
       x = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
# Save as PNG
ggsave("box_accuracy_across_seeds_1CHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)
ggsave("box_accuracy_across_seeds_1stCHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)
ggsave("box_accuracy_across_seeds_2ndCHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)


# Boxplot for error rate
ggplot(results, aes(x = "", y = error_rate)) +
  geom_boxplot(fill = "#4C9AFF", color = "black", alpha = 0.6) +  
  geom_jitter(width = 0.1, alpha = 0.5, color = "#1B4F72") +       
  labs(title = "Error rate across 10 seeds", y = "Error rate", x = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
# Save as PNG
ggsave("box_errorrate_across_seeds_1CHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)
ggsave("box_errorrate_across_seeds_1stCHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)
ggsave("box_errorrate_across_seeds_2ndCHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)


# Boxplot for AUC
ggplot(results, aes(x = "", y = auc)) +
  geom_boxplot(fill = "#4C9AFF", color = "black", alpha = 0.6) +  
  geom_jitter(width = 0.1, alpha = 0.5, color = "#1B4F72") +       
  labs(title = "AUC Across Seeds", y = "AUC", x = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
# Save as PNG
ggsave("box_AUC_across_seeds_1CHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)
ggsave("box_AUC_across_seeds_1stCHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)
ggsave("box_AUC_across_seeds_2ndCHH_impl.png", box_acc, width = 6, height = 4, dpi = 300)
