
# Part C - Corrected two-stage LGD estimates

# Stage 1: Probability of positive recovery for every test loan
stage1_lgd_prob <- predict(
  recovery_rf_final,
  newdata = test_final,
  type = "prob"
)[, "1"]

# Stage 2: Predict conditional recovery rate for EVERY test loan
# Align factor levels with the Stage 2 training data
stage2_all_test <- test_final[, predictors, drop = FALSE]

for (v in predictors) {
  if (is.factor(stage2_train[[v]])) {
    stage2_all_test[[v]] <- factor(
      stage2_all_test[[v]],
      levels = levels(stage2_train[[v]])
    )
  }
}

stage2_conditional_all <- predict(
  stage2_rf_tuned,
  newdata = stage2_all_test
)

# Check for missing predictions
cat(
  "Missing Stage 2 predictions:",
  sum(is.na(stage2_conditional_all)), "\n"
)

# Stop if factor-level mismatches caused missing predictions
if (anyNA(stage2_conditional_all)) {
  stop("Missing Stage 2 predictions detected. Check factor levels.")
}

# Keep predicted conditional recovery rates within [0, 1]
stage2_conditional_all <- pmin(
  pmax(stage2_conditional_all, 0),
  1
)

# Combine Stage 1 and Stage 2 without using observed test outcomes
estimated_recovery_rate <- stage1_lgd_prob *
  stage2_conditional_all

estimated_lgd <- pmin(
  pmax(1 - estimated_recovery_rate, 0),
  1
)

# Observed LGD proxy, used ONLY for evaluation
observed_lgd <- 1 -
  loan_model_data$recovery_rate[test_rows]

# Store aligned test-set predictions and outcomes
lgd_results <- data.frame(
  positive_recovery_probability = stage1_lgd_prob,
  conditional_recovery_rate = stage2_conditional_all,
  estimated_recovery_rate = estimated_recovery_rate,
  estimated_lgd = estimated_lgd,
  observed_lgd = observed_lgd
)

# Evaluate combined LGD estimates
lgd_mae <- mean(abs(
  lgd_results$estimated_lgd - lgd_results$observed_lgd
))

lgd_rmse <- sqrt(mean(
  (lgd_results$estimated_lgd - lgd_results$observed_lgd)^2
))

cat("\n--- Corrected two-stage LGD results ---\n")
cat("Test observations:", nrow(lgd_results), "\n")
cat("Mean estimated LGD:",
    round(mean(lgd_results$estimated_lgd), 4), "\n")
cat("Mean observed LGD proxy:",
    round(mean(lgd_results$observed_lgd), 4), "\n")
cat("Median estimated LGD:",
    round(median(lgd_results$estimated_lgd), 4), "\n")
cat("Median observed LGD proxy:",
    round(median(lgd_results$observed_lgd), 4), "\n")
cat("LGD MAE:", round(lgd_mae, 4), "\n")
cat("LGD RMSE:", round(lgd_rmse, 4), "\n")



#------------------------
# Diagnostics 


# Part C - LGD prediction diagnostics

lgd_error <- lgd_results$estimated_lgd -
  lgd_results$observed_lgd

lgd_abs_error <- abs(lgd_error)

lgd_diagnostics <- data.frame(
  Metric = c(
    "Mean prediction error (bias)",
    "Median prediction error",
    "Median absolute error",
    "75th percentile absolute error",
    "90th percentile absolute error",
    "95th percentile absolute error",
    "Correlation: predicted vs observed LGD"
  ),
  Value = c(
    mean(lgd_error),
    median(lgd_error),
    median(lgd_abs_error),
    unname(quantile(lgd_abs_error, 0.75)),
    unname(quantile(lgd_abs_error, 0.90)),
    unname(quantile(lgd_abs_error, 0.95)),
    cor(
      lgd_results$estimated_lgd,
      lgd_results$observed_lgd
    )
  )
)

lgd_diagnostics$Value <- round(
  lgd_diagnostics$Value, 4
)

cat("\n--- LGD prediction diagnostics ---\n")
print(lgd_diagnostics, row.names = FALSE)

#----------------------------
# Compare our model to benchmark of mean


# Part C - Benchmark against constant mean LGD

# Training-set observed LGD proxy
train_lgd_actual <- 1 -
  loan_model_data$recovery_rate[train_rows]

benchmark_lgd <- mean(train_lgd_actual)

# Predict the same training mean for every test loan
benchmark_prediction <- rep(
  benchmark_lgd,
  nrow(lgd_results)
)

# Benchmark metrics
benchmark_mae <- mean(abs(
  benchmark_prediction - lgd_results$observed_lgd
))

benchmark_rmse <- sqrt(mean(
  (benchmark_prediction - lgd_results$observed_lgd)^2
))

# Compare model and benchmark
lgd_model_comparison <- data.frame(
  Model = c(
    "Two-stage Random Forest",
    "Training-mean benchmark"
  ),
  MAE = c(lgd_mae, benchmark_mae),
  RMSE = c(lgd_rmse, benchmark_rmse)
)

lgd_model_comparison$MAE <- round(
  lgd_model_comparison$MAE, 4
)

lgd_model_comparison$RMSE <- round(
  lgd_model_comparison$RMSE, 4
)

cat("\n--- LGD model versus benchmark ---\n")
print(lgd_model_comparison, row.names = FALSE)


#----------------------------
# Check for systemic bias


# Part C - LGD calibration by predicted-LGD decile

lgd_calibration_data <- data.frame(
  predicted_lgd = lgd_results$estimated_lgd,
  observed_lgd = lgd_results$observed_lgd
)

# Group loans by predicted LGD
lgd_calibration_data$decile <- dplyr::ntile(
  lgd_calibration_data$predicted_lgd,
  10
)

lgd_decile_summary <- aggregate(
  cbind(predicted_lgd, observed_lgd) ~ decile,
  data = lgd_calibration_data,
  FUN = mean
)

lgd_decile_count <- as.data.frame(
  table(lgd_calibration_data$decile)
)

names(lgd_decile_count) <- c("decile", "LoanCount")
lgd_decile_count$decile <- as.integer(
  as.character(lgd_decile_count$decile)
)

lgd_decile_summary <- merge(
  lgd_decile_summary,
  lgd_decile_count,
  by = "decile"
)

lgd_decile_summary <- lgd_decile_summary[
  order(lgd_decile_summary$decile),
]

names(lgd_decile_summary) <- c(
  "PredictedLGDDecile",
  "MeanPredictedLGD",
  "MeanObservedLGD",
  "LoanCount"
)

lgd_decile_summary$MeanPredictedLGD <- round(
  lgd_decile_summary$MeanPredictedLGD, 4
)

lgd_decile_summary$MeanObservedLGD <- round(
  lgd_decile_summary$MeanObservedLGD, 4
)

cat("\n--- LGD calibration by predicted-LGD decile ---\n")
print(lgd_decile_summary, row.names = FALSE)


#--------------------------
# Distribution Plot


# Part C - Plot predicted versus observed LGD

library(ggplot2)

# Plot 1: Compare distributions of predicted and observed LGD
lgd_distribution_data <- data.frame(
  LGD = c(
    lgd_results$estimated_lgd,
    lgd_results$observed_lgd
  ),
  Type = rep(
    c("Predicted LGD", "Observed LGD"),
    each = nrow(lgd_results)
  )
)

p_lgd_distribution <- ggplot(
  lgd_distribution_data,
  aes(x = LGD, colour = Type)
) +
  geom_density(linewidth = 1) +
  labs(
    title = "Distribution of Predicted and Observed LGD",
    x = "LGD",
    y = "Density",
    colour = "Series"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal()

print(p_lgd_distribution)


# Plot 2: Compare mean predicted and observed LGD by decile
lgd_decile_plot_data <- rbind(
  data.frame(
    Decile = lgd_decile_summary$PredictedLGDDecile,
    LGD = lgd_decile_summary$MeanPredictedLGD,
    Type = "Predicted LGD"
  ),
  data.frame(
    Decile = lgd_decile_summary$PredictedLGDDecile,
    LGD = lgd_decile_summary$MeanObservedLGD,
    Type = "Observed LGD"
  )
)

p_lgd_decile <- ggplot(
  lgd_decile_plot_data,
  aes(x = Decile, y = LGD, colour = Type, group = Type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Predicted vs Observed LGD by Predicted-LGD Decile",
    x = "Predicted LGD decile (low to high)",
    y = "Mean LGD",
    colour = "Series"
  ) +
  theme_minimal()

print(p_lgd_decile)


# Numerical summary of predicted and observed LGD

cat("\n--- LGD Distribution Summary ---\n")

lgd_distribution_summary <- data.frame(
  Series = c("Predicted LGD", "Observed LGD"),
  Mean = c(
    mean(lgd_results$estimated_lgd),
    mean(lgd_results$observed_lgd)
  ),
  Median = c(
    median(lgd_results$estimated_lgd),
    median(lgd_results$observed_lgd)
  ),
  SD = c(
    sd(lgd_results$estimated_lgd),
    sd(lgd_results$observed_lgd)
  ),
  Minimum = c(
    min(lgd_results$estimated_lgd),
    min(lgd_results$observed_lgd)
  ),
  Maximum = c(
    max(lgd_results$estimated_lgd),
    max(lgd_results$observed_lgd)
  )
)

lgd_distribution_summary[, -1] <- round(
  lgd_distribution_summary[, -1], 4
)

print(lgd_distribution_summary, row.names = FALSE)

cat("\n--- Decile Calibration Differences ---\n")

lgd_decile_summary$Difference <- round(
  lgd_decile_summary$MeanPredictedLGD -
    lgd_decile_summary$MeanObservedLGD,
  4
)

print(
  lgd_decile_summary[
    , c(
      "PredictedLGDDecile",
      "MeanPredictedLGD",
      "MeanObservedLGD",
      "Difference"
    )
  ],
  row.names = FALSE
)


cat("\n--- LGD Distribution Summary ---\n")

lgd_distribution_summary <- data.frame(
  Series = c("Predicted LGD", "Observed LGD"),
  Mean = c(
    mean(lgd_results$estimated_lgd),
    mean(lgd_results$observed_lgd)
  ),
  Median = c(
    median(lgd_results$estimated_lgd),
    median(lgd_results$observed_lgd)
  ),
  SD = c(
    sd(lgd_results$estimated_lgd),
    sd(lgd_results$observed_lgd)
  ),
  Minimum = c(
    min(lgd_results$estimated_lgd),
    min(lgd_results$observed_lgd)
  ),
  Maximum = c(
    max(lgd_results$estimated_lgd),
    max(lgd_results$observed_lgd)
  )
)

lgd_distribution_summary[, -1] <- round(
  lgd_distribution_summary[, -1], 4
)

print(lgd_distribution_summary, row.names = FALSE)


# ============================================================
# SAVE LGD RESULTS FOR THE SHINY DASHBOARD
# ============================================================

lgd_dashboard <- list(
  
  # Test-set predictions and observed outcomes
  results = lgd_results,
  
  # Model and benchmark performance
  metrics = list(
    mae = lgd_mae,
    rmse = lgd_rmse,
    benchmark_mae = benchmark_mae,
    benchmark_rmse = benchmark_rmse,
    mean_predicted_lgd = mean(lgd_results$estimated_lgd),
    mean_observed_lgd = mean(lgd_results$observed_lgd),
    median_predicted_lgd = median(lgd_results$estimated_lgd),
    median_observed_lgd = median(lgd_results$observed_lgd)
  ),
  
  # Prediction error diagnostics
  diagnostics = lgd_diagnostics,
  
  # Model-versus-benchmark table
  model_comparison = lgd_model_comparison,
  
  # Calibration by predicted-LGD decile
  calibration = lgd_decile_summary,
  
  # Predicted-versus-observed distribution summary
  distribution_summary = lgd_distribution_summary,
  
  # Data used for the distribution plot
  distribution_data = lgd_distribution_data,
  
  # Data used for the decile calibration plot
  decile_plot_data = lgd_decile_plot_data
)

saveRDS(
  lgd_dashboard,
  file = "data/lgd_dashboard.rds"
)

cat("\nLGD dashboard results saved successfully.\n")
cat("Test observations saved:", nrow(lgd_dashboard$results), "\n")
cat("LGD MAE:", round(lgd_dashboard$metrics$mae, 4), "\n")
cat("LGD RMSE:", round(lgd_dashboard$metrics$rmse, 4), "\n")
