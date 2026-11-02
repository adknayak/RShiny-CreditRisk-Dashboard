
# Part C - Stage 2: Random Forest regression
# Target: recovery rate, conditional on positive recovery

library(randomForest)

set.seed(789)

recovery_rf_stage2 <- randomForest(
  recovery_rate ~ .,
  data = stage2_train,
  ntree = 500,
  mtry = floor(sqrt(length(predictors))),
  nodesize = 5,
  importance = TRUE
)

cat("\n--- Stage 2 Random Forest model ---\n")
print(recovery_rf_stage2)

# Predict conditional recovery rates on the held-out test set
rf_stage2_pred_raw <- predict(
  recovery_rf_stage2,
  newdata = stage2_test
)

# Restrict predictions to the valid recovery-rate range
rf_stage2_pred <- pmin(
  pmax(rf_stage2_pred_raw, 0),
  1
)

rf_stage2_actual <- stage2_test$recovery_rate

# Test performance
rf_stage2_mae <- mean(
  abs(rf_stage2_pred - rf_stage2_actual)
)

rf_stage2_rmse <- sqrt(
  mean((rf_stage2_pred - rf_stage2_actual)^2)
)

rf_stage2_metrics <- data.frame(
  Metric = c("MAE", "RMSE"),
  Value = round(
    c(rf_stage2_mae, rf_stage2_rmse),
    4
  )
)

cat("\n--- Stage 2 Random Forest test performance ---\n")
print(rf_stage2_metrics, row.names = FALSE)

cat(
  "\nRaw predictions below 0:",
  sum(rf_stage2_pred_raw < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Raw predictions above 1:",
  sum(rf_stage2_pred_raw > 1, na.rm = TRUE),
  "\n"
)

cat("\n--- Stage 2 variable importance ---\n")
print(importance(recovery_rf_stage2))


#-----------------------------
# Tuning


# Part C - Stage 2: Random Forest tuning
# Cross-validation on training data only

set.seed(456)

# Create 3 folds for the positive-recovery training observations
stage2_fold_id <- sample(
  rep(1:3, length.out = nrow(stage2_train))
)

stage2_tuning_grid <- expand.grid(
  mtry = c(3, 4, 6),
  nodesize = c(1, 5, 10)
)

stage2_tuning_results <- data.frame(
  mtry = stage2_tuning_grid$mtry,
  nodesize = stage2_tuning_grid$nodesize,
  Mean_MAE = NA_real_,
  SD_MAE = NA_real_,
  Mean_RMSE = NA_real_
)

for (i in seq_len(nrow(stage2_tuning_grid))) {
  
  fold_mae <- numeric(3)
  fold_rmse <- numeric(3)
  
  for (k in 1:3) {
    
    fold_train <- stage2_train[stage2_fold_id != k, ]
    fold_valid <- stage2_train[stage2_fold_id == k, ]
    
    set.seed(2000 + i * 10 + k)
    
    rf_fit <- randomForest(
      recovery_rate ~ .,
      data = fold_train,
      ntree = 200,
      mtry = stage2_tuning_grid$mtry[i],
      nodesize = stage2_tuning_grid$nodesize[i]
    )
    
    pred <- predict(rf_fit, newdata = fold_valid)
    pred <- pmin(pmax(pred, 0), 1)
    
    actual <- fold_valid$recovery_rate
    
    fold_mae[k] <- mean(abs(pred - actual))
    fold_rmse[k] <- sqrt(mean((pred - actual)^2))
  }
  
  stage2_tuning_results$Mean_MAE[i] <- mean(fold_mae)
  stage2_tuning_results$SD_MAE[i] <- sd(fold_mae)
  stage2_tuning_results$Mean_RMSE[i] <- mean(fold_rmse)
  
  cat(
    "Completed combination", i, "of",
    nrow(stage2_tuning_grid), "\n"
  )
}

# Rank primarily by MAE, then by RMSE
stage2_tuning_results <- stage2_tuning_results[
  order(
    stage2_tuning_results$Mean_MAE,
    stage2_tuning_results$Mean_RMSE
  ),
]

rownames(stage2_tuning_results) <- NULL

cat("\n--- Stage 2 Random Forest tuning results ---\n")
print(
  transform(
    stage2_tuning_results,
    Mean_MAE = round(Mean_MAE, 4),
    SD_MAE = round(SD_MAE, 4),
    Mean_RMSE = round(Mean_RMSE, 4)
  ),
  row.names = FALSE
)

best_stage2_params <- stage2_tuning_results[
  1, c("mtry", "nodesize")
]

cat("\n--- Leading Stage 2 configuration ---\n")
print(best_stage2_params)


#----------------------------
# Evaluate tuned model on test set



# Part C - Stage 2: Final tuned Random Forest

set.seed(456)

stage2_rf_tuned <- randomForest(
  recovery_rate ~ .,
  data = stage2_train,
  ntree = 500,
  mtry = best_stage2_params$mtry,
  nodesize = best_stage2_params$nodesize,
  importance = TRUE
)

# Predict conditional recovery rates on the test set
stage2_rf_pred_raw <- predict(
  stage2_rf_tuned,
  newdata = stage2_test
)

# Keep predictions within the valid recovery-rate range
stage2_rf_pred <- pmin(
  pmax(stage2_rf_pred_raw, 0),
  1
)

stage2_actual <- stage2_test$recovery_rate

# Calculate test-set performance
stage2_rf_mae <- mean(
  abs(stage2_rf_pred - stage2_actual)
)

stage2_rf_rmse <- sqrt(
  mean((stage2_rf_pred - stage2_actual)^2)
)

cat("\n--- Final tuned Stage 2 Random Forest ---\n")
cat("mtry:", best_stage2_params$mtry, "\n")
cat("nodesize:", best_stage2_params$nodesize, "\n")
cat("Trees:", 500, "\n")
cat("Test MAE:", round(stage2_rf_mae, 4), "\n")
cat("Test RMSE:", round(stage2_rf_rmse, 4), "\n")

# Compare with the Stage 2 linear regression baseline
cat("\n--- Comparison with linear regression ---\n")
cat("Linear regression MAE:", round(lm_mae, 4), "\n")
cat("Tuned Random Forest MAE:", round(stage2_rf_mae, 4), "\n")
cat("Linear regression RMSE:", round(lm_rmse, 4), "\n")
cat("Tuned Random Forest RMSE:", round(stage2_rf_rmse, 4), "\n")



# ============================================================
# SAVE STAGE 2 RESULTS FOR THE SHINY DASHBOARD
# ============================================================

stage2_dashboard <- list(
  model = stage2_rf_tuned,
  metrics = list(
    mae = 0.0515,
    rmse = 0.0938
  ),
  tuning = list(
    best_mtry = 3,
    best_nodesize = 10,
    cv_mae = 0.0512,
    cv_mae_sd = 0.0012,
    cv_rmse = 0.0933
  ),
  variable_importance = c(
    open_acc = 29.34,
    revol_bal = 26.90,
    total_acc = 25.47,
    revol_util = 23.78,
    funded_amnt = 22.91,
    dti = 20.49,
    int_rate = 19.07,
    term = 18.33,
    annual_inc = 18.32
  )
)

saveRDS(
  stage2_dashboard,
  file = "data/stage2_dashboard.rds"
)

cat("Stage 2 dashboard results saved successfully.\n")

