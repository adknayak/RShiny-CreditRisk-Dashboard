
# ============================================================
# PART D: ROBUSTNESS, PORTFOLIO SENSITIVITY AND MODEL RISK
# Step 1: Inspect existing data and model objects
# This script reads existing files only.
# ============================================================

# Load the prepared modelling dataset and saved model objects
loan_model_data <- readRDS("data/loan_data_model_prepared.rds")

stage1_dashboard <- readRDS("data/stage1_dashboard.rds")
stage2_dashboard <- readRDS("data/stage2_dashboard.rds")
lgd_dashboard <- readRDS("data/lgd_dashboard.rds")

# 1. Check the prepared dataset
cat("\n--- Prepared modelling dataset ---\n")
cat("Rows:", nrow(loan_model_data), "\n")
cat("Columns:", ncol(loan_model_data), "\n")

# 2. Check the available columns
cat("\n--- Available columns ---\n")
print(names(loan_model_data))

# 3. Inspect the saved Stage 1 model object
cat("\n--- Stage 1 object structure ---\n")
print(names(stage1_dashboard))

# 4. Inspect the saved Stage 2 model object
cat("\n--- Stage 2 object structure ---\n")
print(names(stage2_dashboard))

# 5. Inspect the combined LGD object
cat("\n--- Combined LGD object structure ---\n")
print(names(lgd_dashboard))


# ============================================================
# PART D.1: ROBUSTNESS AND RELIABILITY
# Step 2: Inspect existing validation results
# ============================================================

# 1. Stage 1 classification metrics
cat("\n--- Stage 1 metrics ---\n")
print(stage1_dashboard$metrics)

# 2. Stage 2 regression metrics
cat("\n--- Stage 2 metrics ---\n")
print(stage2_dashboard$metrics)

# 3. Combined LGD metrics
cat("\n--- Combined LGD metrics ---\n")
print(lgd_dashboard$metrics)

# 4. Existing LGD diagnostics
cat("\n--- LGD diagnostics ---\n")
print(lgd_dashboard$diagnostics)

# 5. Existing calibration results
cat("\n--- LGD calibration ---\n")
print(lgd_dashboard$calibration)

# 6. Distribution summary
cat("\n--- LGD distribution summary ---\n")
print(lgd_dashboard$distribution_summary)

# 7. Inspect the combined prediction results
cat("\n--- Combined LGD results: column names ---\n")
print(names(lgd_dashboard$results))

cat("\n--- First six combined LGD results ---\n")
print(head(lgd_dashboard$results))

# 8. Inspect saved tuning information
cat("\n--- Stage 1 tuning ---\n")
print(stage1_dashboard$tuning)

cat("\n--- Stage 2 tuning ---\n")
print(stage2_dashboard$tuning)


# ============================================================
# PART D.1: Step 3
# Check whether test predictions can be matched to loan records
# This is an inspection only; no files are modified.
# ============================================================

lgd_results <- lgd_dashboard$results

cat("\n--- Number of LGD test predictions ---\n")
print(nrow(lgd_results))

cat("\n--- Prediction row names ---\n")
print(head(rownames(lgd_results), 10))

cat("\n--- Last prediction row names ---\n")
print(tail(rownames(lgd_results), 10))

cat("\n--- Prepared dataset row names ---\n")
print(head(rownames(loan_model_data), 10))

cat("\n--- Grade distribution in prepared data ---\n")
print(table(loan_model_data$grade, useNA = "ifany"))


# ============================================================
# PART D.1: Robustness by Loan Grade
# Match test predictions to the original loan records
# ============================================================

lgd_results <- lgd_dashboard$results

# Match each prediction to its original dataset row
row_index <- match(
  rownames(lgd_results),
  rownames(loan_model_data)
)

# Stop if any prediction cannot be matched
if (anyNA(row_index)) {
  stop("Some LGD predictions could not be matched to loan records.")
}

# Build a temporary analysis dataset
lgd_grade_analysis <- data.frame(
  grade = loan_model_data$grade[row_index],
  predicted_lgd = lgd_results$estimated_lgd,
  observed_lgd = lgd_results$observed_lgd
)

# Calculate errors
lgd_grade_analysis$error <-
  lgd_grade_analysis$predicted_lgd -
  lgd_grade_analysis$observed_lgd

lgd_grade_analysis$absolute_error <-
  abs(lgd_grade_analysis$error)

lgd_grade_analysis$squared_error <-
  lgd_grade_analysis$error^2

# Summarise performance by grade
grade_robustness <- aggregate(
  cbind(
    absolute_error,
    squared_error,
    error,
    predicted_lgd,
    observed_lgd
  ) ~ grade,
  data = lgd_grade_analysis,
  FUN = mean
)

# Add loan counts and within-grade variability
grade_counts <- as.data.frame(
  table(lgd_grade_analysis$grade)
)

names(grade_counts) <- c("grade", "loan_count")

grade_robustness$RMSE <- sqrt(grade_robustness$squared_error)

grade_robustness <- merge(
  grade_robustness,
  grade_counts,
  by = "grade",
  sort = TRUE
)

grade_sd <- aggregate(
  cbind(predicted_lgd, observed_lgd) ~ grade,
  data = lgd_grade_analysis,
  FUN = sd
)

names(grade_sd) <- c(
  "grade",
  "predicted_lgd_sd",
  "observed_lgd_sd"
)

grade_robustness <- merge(
  grade_robustness,
  grade_sd,
  by = "grade",
  sort = TRUE
)

# Keep the final table readable
grade_robustness <- grade_robustness[
  ,
  c(
    "grade",
    "loan_count",
    "absolute_error",
    "RMSE",
    "error",
    "predicted_lgd",
    "observed_lgd",
    "predicted_lgd_sd",
    "observed_lgd_sd"
  )
]

names(grade_robustness) <- c(
  "Grade",
  "Loans",
  "MAE",
  "RMSE",
  "Mean_Error",
  "Mean_Predicted_LGD",
  "Mean_Observed_LGD",
  "Predicted_LGD_SD",
  "Observed_LGD_SD"
)

cat("\n--- LGD robustness by loan grade ---\n")
print(grade_robustness, row.names = FALSE, digits = 4)


# ============================================================
# PART D.1: Bootstrap uncertainty of LGD test-set errors
# ============================================================

set.seed(2026)

# Use the existing test-set predictions and observed outcomes
predicted <- lgd_results$estimated_lgd
observed <- lgd_results$observed_lgd

# Check that the inputs are valid
stopifnot(
  length(predicted) == length(observed),
  all(is.finite(predicted)),
  all(is.finite(observed))
)

n <- length(predicted)
B <- 1000

# Store bootstrap estimates
bootstrap_metrics <- matrix(
  NA_real_,
  nrow = B,
  ncol = 2
)

colnames(bootstrap_metrics) <- c("MAE", "RMSE")

# Resample test observations with replacement
for (b in seq_len(B)) {
  idx <- sample.int(n, size = n, replace = TRUE)
  
  errors <- predicted[idx] - observed[idx]
  
  bootstrap_metrics[b, "MAE"] <- mean(abs(errors))
  bootstrap_metrics[b, "RMSE"] <- sqrt(mean(errors^2))
}

# Calculate percentile 95% confidence intervals
bootstrap_summary <- data.frame(
  Metric = c("MAE", "RMSE"),
  Test_Set_Estimate = c(
    mean(abs(predicted - observed)),
    sqrt(mean((predicted - observed)^2))
  ),
  CI_Lower = c(
    quantile(bootstrap_metrics[, "MAE"], 0.025),
    quantile(bootstrap_metrics[, "RMSE"], 0.025)
  ),
  CI_Upper = c(
    quantile(bootstrap_metrics[, "MAE"], 0.975),
    quantile(bootstrap_metrics[, "RMSE"], 0.975)
  )
)

cat("\n--- Bootstrap uncertainty: 95% percentile intervals ---\n")
print(bootstrap_summary, row.names = FALSE, digits = 5)


# ============================================================
# PART D.2: PORTFOLIO SENSITIVITY
# Step 1: Change portfolio grade composition
# ============================================================

# Use the existing test-set predictions and matched loan grades
lgd_results <- lgd_dashboard$results

row_index <- match(
  rownames(lgd_results),
  rownames(loan_model_data)
)

if (anyNA(row_index)) {
  stop("Some LGD predictions could not be matched to loan records.")
}

portfolio_data <- data.frame(
  grade = as.character(loan_model_data$grade[row_index]),
  predicted_lgd = lgd_results$estimated_lgd,
  observed_lgd = lgd_results$observed_lgd
)

# Calculate the existing test-set composition and LGD by grade
grade_summary <- aggregate(
  cbind(predicted_lgd, observed_lgd) ~ grade,
  data = portfolio_data,
  FUN = mean
)

grade_counts <- table(
  factor(portfolio_data$grade, levels = LETTERS[1:7])
)

baseline_shares <- as.numeric(grade_counts) / sum(grade_counts)
names(baseline_shares) <- LETTERS[1:7]

# Function to create an illustrative composition scenario.
# A positive shift transfers portfolio share from grades E-G
# to grades A-C. A negative shift does the reverse.
make_grade_scenario <- function(shift_to_low) {
  
  shares <- baseline_shares
  
  low_grades <- c("A", "B", "C")
  high_grades <- c("E", "F", "G")
  
  low_share <- sum(shares[low_grades])
  high_share <- sum(shares[high_grades])
  
  if (shift_to_low > high_share ||
      -shift_to_low > low_share) {
    stop("The requested composition shift is not feasible.")
  }
  
  # Redistribute shares proportionally within each group
  shares[low_grades] <-
    shares[low_grades] *
    (low_share + shift_to_low) / low_share
  
  shares[high_grades] <-
    shares[high_grades] *
    (high_share - shift_to_low) / high_share
  
  shares
}

# Construct the three scenarios
scenario_shares <- list(
  Baseline = baseline_shares,
  Lower_Risk_Mix = make_grade_scenario(0.10),
  Higher_Risk_Mix = make_grade_scenario(-0.10)
)

# Display grade composition under each scenario
composition_table <- data.frame(
  Grade = LETTERS[1:7],
  Baseline = scenario_shares$Baseline,
  Lower_Risk_Mix = scenario_shares$Lower_Risk_Mix,
  Higher_Risk_Mix = scenario_shares$Higher_Risk_Mix
)

cat("\n--- Portfolio composition by loan grade ---\n")
print(composition_table, row.names = FALSE, digits = 4)

# Calculate portfolio-level outcomes using grade-level averages
scenario_results <- do.call(
  rbind,
  lapply(names(scenario_shares), function(scenario_name) {
    
    shares <- scenario_shares[[scenario_name]]
    
    predicted_by_grade <- setNames(
      grade_summary$predicted_lgd,
      grade_summary$grade
    )
    
    observed_by_grade <- setNames(
      grade_summary$observed_lgd,
      grade_summary$grade
    )
    
    portfolio_predicted_lgd <- sum(
      shares[names(predicted_by_grade)] *
        predicted_by_grade
    )
    
    portfolio_observed_lgd <- sum(
      shares[names(observed_by_grade)] *
        observed_by_grade
    )
    
    data.frame(
      Scenario = scenario_name,
      Predicted_LGD = portfolio_predicted_lgd,
      Implied_Recovery_Rate = 1 - portfolio_predicted_lgd,
      Observed_LGD_Reference = portfolio_observed_lgd,
      Prediction_Error = portfolio_predicted_lgd -
        portfolio_observed_lgd
    )
  })
)

cat("\n--- Portfolio sensitivity results ---\n")
print(scenario_results, row.names = FALSE, digits = 5)

# Express changes relative to the baseline scenario
baseline_lgd <- scenario_results$Predicted_LGD[
  scenario_results$Scenario == "Baseline"
]

scenario_results$Change_vs_Baseline_pp <-
  100 * (scenario_results$Predicted_LGD - baseline_lgd)

cat("\n--- Change in estimated LGD versus baseline ---\n")
print(
  scenario_results[
    ,
    c("Scenario", "Predicted_LGD", "Change_vs_Baseline_pp")
  ],
  row.names = FALSE,
  digits = 5
)


# ============================================================
# PART D.2: Diagnostic check of grade-level LGD patterns
# ============================================================

cat("\n--- Mean observed recovery rate by grade in prepared data ---\n")

print(
  aggregate(
    recovery_rate ~ grade,
    data = loan_model_data,
    FUN = function(x) mean(x, na.rm = TRUE)
  ),
  row.names = FALSE
)


# ============================================================
# PART D.2: Correct scenario labels
# No models or saved files are modified.
# ============================================================

# Correct the composition table column names
names(composition_table) <- c(
  "Grade",
  "Baseline",
  "More_A_to_C",
  "More_E_to_G"
)

# Correct the scenario labels in the results
scenario_results$Scenario[
  scenario_results$Scenario == "Lower_Risk_Mix"
] <- "More_A_to_C"

scenario_results$Scenario[
  scenario_results$Scenario == "Higher_Risk_Mix"
] <- "More_E_to_G"

cat("\n--- Corrected portfolio composition ---\n")
print(composition_table, row.names = FALSE, digits = 4)

cat("\n--- Corrected portfolio sensitivity results ---\n")
print(
  scenario_results[
    ,
    c(
      "Scenario",
      "Predicted_LGD",
      "Implied_Recovery_Rate",
      "Observed_LGD_Reference",
      "Prediction_Error",
      "Change_vs_Baseline_pp"
    )
  ],
  row.names = FALSE,
  digits = 5
)

cat("\n--- Mean observed LGD by grade in the test set ---\n")

print(
  aggregate(
    observed_lgd ~ grade,
    data = portfolio_data,
    FUN = mean
  ),
  row.names = FALSE
)

cat("\n--- Mean predicted LGD by grade in the test set ---\n")

print(
  aggregate(
    predicted_lgd ~ grade,
    data = portfolio_data,
    FUN = mean
  ),
  row.names = FALSE
)



# ============================================================
# Save Part D analysis results for the Shiny dashboard
# ============================================================

part_d_results <- list(
  grade_robustness = grade_robustness,
  bootstrap_summary = bootstrap_summary,
  composition_table = composition_table,
  scenario_results = scenario_results
)

saveRDS(part_d_results, "data/part_d_results.rds")

cat("\nPart D results saved successfully.\n")
cat("Saved file: part_d_results.rds\n")
cat("Objects included:\n")
print(names(part_d_results))
