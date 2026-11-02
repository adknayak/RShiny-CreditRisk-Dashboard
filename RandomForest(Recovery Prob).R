
# Part C - Stage 1: Random Forest comparison

# Install once if needed:
# install.packages("randomForest")

library(randomForest)
library(pROC)

set.seed(123)

# Ensure target is a factor for classification
train_rf <- train_data
test_rf <- test_data

train_rf$positive_recovery <- factor(
  train_rf$positive_recovery,
  levels = c(0, 1)
)

test_rf$positive_recovery <- factor(
  test_rf$positive_recovery,
  levels = c(0, 1)
)

# Fit Random Forest
recovery_rf <- randomForest(
  positive_recovery ~ .,
  data = train_rf,
  ntree = 500,
  mtry = floor(sqrt(length(predictors))),
  importance = TRUE
)

cat("\n--- Random Forest model ---\n")
print(recovery_rf)

# Predict probability of positive recovery
rf_prob <- predict(
  recovery_rf,
  newdata = test_rf,
  type = "prob"
)[, "1"]

rf_actual <- as.integer(as.character(test_rf$positive_recovery))

# Evaluation metrics
rf_prob_safe <- pmin(pmax(rf_prob, 1e-15), 1 - 1e-15)

rf_roc <- roc(
  response = rf_actual,
  predictor = rf_prob,
  quiet = TRUE
)

rf_auc <- as.numeric(auc(rf_roc))
rf_brier <- mean((rf_prob_safe - rf_actual)^2)

rf_log_loss <- -mean(
  rf_actual * log(rf_prob_safe) +
    (1 - rf_actual) * log(1 - rf_prob_safe)
)

rf_class <- as.integer(rf_prob >= 0.5)

rf_tp <- sum(rf_class == 1 & rf_actual == 1)
rf_tn <- sum(rf_class == 0 & rf_actual == 0)
rf_fp <- sum(rf_class == 1 & rf_actual == 0)
rf_fn <- sum(rf_class == 0 & rf_actual == 1)

rf_accuracy <- mean(rf_class == rf_actual)
rf_precision <- rf_tp / (rf_tp + rf_fp)
rf_recall <- rf_tp / (rf_tp + rf_fn)
rf_specificity <- rf_tn / (rf_tn + rf_fp)
rf_f1 <- 2 * rf_precision * rf_recall /
  (rf_precision + rf_recall)

rf_metrics <- data.frame(
  Metric = c(
    "ROC-AUC",
    "Brier score",
    "Log loss",
    "Accuracy (threshold 0.5)",
    "Precision (positive recovery)",
    "Recall (positive recovery)",
    "Specificity (zero recovery)",
    "F1 score (positive recovery)"
  ),
  Value = round(c(
    rf_auc,
    rf_brier,
    rf_log_loss,
    rf_accuracy,
    rf_precision,
    rf_recall,
    rf_specificity,
    rf_f1
  ), 4)
)

cat("\n--- Random Forest test performance ---\n")
print(rf_metrics, row.names = FALSE)

cat("\n--- Random Forest confusion matrix ---\n")
print(table(
  Actual = factor(rf_actual, levels = c(0, 1)),
  Predicted = factor(rf_class, levels = c(0, 1))
))

cat("\n--- Variable importance ---\n")
print(importance(recovery_rf))
varImpPlot(recovery_rf)


#---------------------------------
# Fine Tuning


# Part C - Stage 1: Random Forest tuning
# Cross-validation uses training data only.
# The test set is not used for tuning.

library(randomForest)
library(pROC)

set.seed(456)

# Use the existing training data only
tuning_data <- train_data

tuning_data$positive_recovery <- factor(
  tuning_data$positive_recovery,
  levels = c(0, 1)
)

# Create stratified 3-fold assignments
fold_id <- integer(nrow(tuning_data))

for (class_value in c(0, 1)) {
  class_rows <- which(
    tuning_data$positive_recovery == class_value
  )
  
  fold_id[class_rows] <- sample(
    rep(1:3, length.out = length(class_rows))
  )
}

# Parameter combinations
tuning_grid <- expand.grid(
  mtry = c(3, 4, 6),
  nodesize = c(1, 5)
)

# Store cross-validation results
tuning_results <- data.frame(
  mtry = tuning_grid$mtry,
  nodesize = tuning_grid$nodesize,
  Mean_AUC = NA_real_,
  SD_AUC = NA_real_,
  Mean_Brier = NA_real_
)

for (i in seq_len(nrow(tuning_grid))) {
  
  fold_auc <- numeric(3)
  fold_brier <- numeric(3)
  
  for (k in 1:3) {
    
    fold_train <- tuning_data[fold_id != k, ]
    fold_valid <- tuning_data[fold_id == k, ]
    
    set.seed(1000 + i * 10 + k)
    
    rf_fit <- randomForest(
      positive_recovery ~ .,
      data = fold_train,
      ntree = 200,
      mtry = tuning_grid$mtry[i],
      nodesize = tuning_grid$nodesize[i]
    )
    
    fold_prob <- predict(
      rf_fit,
      newdata = fold_valid,
      type = "prob"
    )[, "1"]
    
    fold_actual <- as.integer(
      as.character(fold_valid$positive_recovery)
    )
    
    fold_auc[k] <- as.numeric(
      pROC::auc(
        pROC::roc(
          fold_actual,
          fold_prob,
          levels = c(0, 1),
          direction = "<",
          quiet = TRUE
        )
      )
    )
    
    fold_brier[k] <- mean(
      (fold_prob - fold_actual)^2
    )
  }
  
  tuning_results$Mean_AUC[i] <- mean(fold_auc)
  tuning_results$SD_AUC[i] <- sd(fold_auc)
  tuning_results$Mean_Brier[i] <- mean(fold_brier)
  
  cat(
    "Completed combination", i, "of",
    nrow(tuning_grid), "\n"
  )
}

# Rank by AUC, then Brier score
tuning_results <- tuning_results[
  order(
    -tuning_results$Mean_AUC,
    tuning_results$Mean_Brier
  ),
]

rownames(tuning_results) <- NULL

cat("\n--- Random Forest cross-validation tuning results ---\n")
print(
  transform(
    tuning_results,
    Mean_AUC = round(Mean_AUC, 4),
    SD_AUC = round(SD_AUC, 4),
    Mean_Brier = round(Mean_Brier, 4)
  ),
  row.names = FALSE
)

# Save the leading configuration for the next step
best_rf_params <- tuning_results[1, c("mtry", "nodesize")]

cat("\n--- Leading parameter configuration ---\n")
print(best_rf_params)


#-------------------------
# Checking performance of tuned 


# Part C - Stage 1: Final tuned Random Forest

set.seed(789)

train_final <- train_data
test_final <- test_data

train_final$positive_recovery <- factor(
  train_final$positive_recovery,
  levels = c(0, 1)
)

test_final$positive_recovery <- factor(
  test_final$positive_recovery,
  levels = c(0, 1)
)

# Fit the selected configuration on the full training set
recovery_rf_final <- randomForest(
  positive_recovery ~ .,
  data = train_final,
  ntree = 500,
  mtry = best_rf_params$mtry,
  nodesize = best_rf_params$nodesize,
  importance = TRUE
)

# Predict on the untouched test set
final_rf_prob <- predict(
  recovery_rf_final,
  newdata = test_final,
  type = "prob"
)[, "1"]

final_rf_actual <- as.integer(
  as.character(test_final$positive_recovery)
)

# Protect probability calculations from log(0)
final_rf_prob_safe <- pmin(
  pmax(final_rf_prob, 1e-15),
  1 - 1e-15
)

# Probability metrics
final_rf_auc <- as.numeric(
  pROC::auc(
    pROC::roc(
      final_rf_actual,
      final_rf_prob,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
  )
)

final_rf_brier <- mean(
  (final_rf_prob_safe - final_rf_actual)^2
)

final_rf_logloss <- -mean(
  final_rf_actual * log(final_rf_prob_safe) +
    (1 - final_rf_actual) * log(1 - final_rf_prob_safe)
)

# Classification metrics at threshold 0.5
final_rf_class <- as.integer(final_rf_prob >= 0.5)

tp <- sum(final_rf_class == 1 & final_rf_actual == 1)
tn <- sum(final_rf_class == 0 & final_rf_actual == 0)
fp <- sum(final_rf_class == 1 & final_rf_actual == 0)
fn <- sum(final_rf_class == 0 & final_rf_actual == 1)

final_rf_precision <- tp / (tp + fp)
final_rf_recall <- tp / (tp + fn)
final_rf_specificity <- tn / (tn + fp)
final_rf_f1 <- 2 * final_rf_precision * final_rf_recall /
  (final_rf_precision + final_rf_recall)

final_rf_metrics <- data.frame(
  Metric = c(
    "ROC-AUC",
    "Brier score",
    "Log loss",
    "Accuracy (threshold 0.5)",
    "Precision (positive recovery)",
    "Recall (positive recovery)",
    "Specificity (zero recovery)",
    "F1 score (positive recovery)"
  ),
  Value = round(c(
    final_rf_auc,
    final_rf_brier,
    final_rf_logloss,
    mean(final_rf_class == final_rf_actual),
    final_rf_precision,
    final_rf_recall,
    final_rf_specificity,
    final_rf_f1
  ), 4)
)

cat("\n--- Final tuned Random Forest: test performance ---\n")
print(final_rf_metrics, row.names = FALSE)

cat("\n--- Test confusion matrix ---\n")
print(table(
  Actual = factor(final_rf_actual, levels = c(0, 1)),
  Predicted = factor(final_rf_class, levels = c(0, 1))
))

#----------------------
# Generate calibration table


# Part C - Stage 1: Calibration assessment
# Uses the existing held-out test predictions

# Divide predictions into 10 groups by predicted probability
calibration_data <- data.frame(
  actual = final_rf_actual,
  predicted = final_rf_prob
)

calibration_data$decile <- dplyr::ntile(
  calibration_data$predicted,
  10
)

rf_calibration <- aggregate(
  cbind(predicted, actual) ~ decile,
  data = calibration_data,
  FUN = mean
)

names(rf_calibration) <- c(
  "Decile",
  "MeanPredictedProbability",
  "ObservedPositiveRecoveryRate"
)

rf_calibration$MeanPredictedProbability <- round(
  rf_calibration$MeanPredictedProbability, 4
)

rf_calibration$ObservedPositiveRecoveryRate <- round(
  rf_calibration$ObservedPositiveRecoveryRate, 4
)

cat("\n--- Random Forest calibration table ---\n")
print(rf_calibration, row.names = FALSE)

# Calibration plot
library(ggplot2)

ggplot(
  rf_calibration,
  aes(
    x = MeanPredictedProbability,
    y = ObservedPositiveRecoveryRate
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_point(size = 3) +
  geom_line() +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Random Forest Calibration: Positive Recovery",
    x = "Mean predicted probability",
    y = "Observed positive recovery rate"
  ) +
  theme_minimal()


#-----------------
# create rds file 


# ============================================================
# SAVE STAGE 1 RESULTS FOR THE SHINY DASHBOARD
# ============================================================

stage1_dashboard <- list(
  model = recovery_rf_final,
  metrics = list(
    auc = 0.6252,
    brier = 0.2344,
    log_loss = 0.6608,
    accuracy = 0.6025,
    precision = 0.6124,
    recall = 0.8031,
    specificity = 0.3432,
    f1 = 0.6949
  ),
  tuning = list(
    best_mtry = 3,
    best_nodesize = 5,
    cv_auc = 0.6174,
    cv_auc_sd = 0.0026
  )
)

saveRDS(
  stage1_dashboard,
  file = "data/stage1_dashboard.rds"
)

cat("Stage 1 dashboard results saved successfully.\n")
