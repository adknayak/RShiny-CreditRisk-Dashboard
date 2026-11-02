
# Part C - Stage 2: Conditional recovery-rate baseline
# Only loans with positive recoveries are included.
# Reuse the existing train/test split.

# Identify positive-recovery rows within each split
positive_train_rows <- train_rows[
  loan_model_data$recoveries[train_rows] > 0
]

positive_test_rows <- test_rows[
  loan_model_data$recoveries[test_rows] > 0
]

# Keep the target and the established predictor set
stage2_train <- loan_model_data[
  positive_train_rows,
  c("recovery_rate", predictors)
]

stage2_test <- loan_model_data[
  positive_test_rows,
  c("recovery_rate", predictors)
]

# Convert character predictors to factors
character_vars <- names(stage2_train)[
  vapply(stage2_train, is.character, logical(1))
]

for (v in character_vars) {
  stage2_train[[v]] <- as.factor(stage2_train[[v]])
  stage2_test[[v]] <- factor(
    stage2_test[[v]],
    levels = levels(stage2_train[[v]])
  )
}

# Check sample sizes and target distribution
cat("\n--- Stage 2 sample sizes ---\n")
cat("Training observations:", nrow(stage2_train), "\n")
cat("Test observations:", nrow(stage2_test), "\n")

cat("\n--- Conditional recovery-rate distribution (training) ---\n")
print(summary(stage2_train$recovery_rate))

cat(
  "\nTraining recovery rates equal to 1:",
  sum(stage2_train$recovery_rate == 1),
  "\n"
)

# Fit baseline linear regression
stage2_formula <- reformulate(
  predictors,
  response = "recovery_rate"
)

recovery_lm <- lm(
  stage2_formula,
  data = stage2_train
)

cat("\n--- Linear regression baseline ---\n")
print(summary(recovery_lm))

# Predict on the held-out positive-recovery test observations
lm_pred_raw <- predict(
  recovery_lm,
  newdata = stage2_test
)

# Recovery rates must lie between 0 and 1.
# Clip predictions to that valid range for evaluation.
lm_pred <- pmin(pmax(lm_pred_raw, 0), 1)

lm_actual <- stage2_test$recovery_rate

# Test metrics
lm_mae <- mean(abs(lm_pred - lm_actual))
lm_rmse <- sqrt(mean((lm_pred - lm_actual)^2))

lm_metrics <- data.frame(
  Metric = c("MAE", "RMSE"),
  Value = round(c(lm_mae, lm_rmse), 4)
)

cat("\n--- Linear regression test performance ---\n")
print(lm_metrics, row.names = FALSE)

cat(
  "\nRaw predictions below 0:",
  sum(lm_pred_raw < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Raw predictions above 1:",
  sum(lm_pred_raw > 1, na.rm = TRUE),
  "\n"
)
