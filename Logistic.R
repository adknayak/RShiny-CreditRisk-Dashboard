
# Part C - Stage 1: Recovery Probability
# Baseline logistic regression

# 1. Load modelling data
loan_model_data <- readRDS("data/loan_data_model_prepared.rds")

# 2. Define target
loan_model_data$positive_recovery <- as.integer(
  loan_model_data$recoveries > 0
)

# 3. Define predictors
predictors <- c(
  "funded_amnt",
  "term",
  "int_rate",
  "grade",
  "purpose",
  "home_ownership",
  "annual_inc",
  "verification_status",
  "emp_length_model",
  "dti",
  "delinq_2yrs",
  "inq_last_6mths",
  "open_acc",
  "pub_rec",
  "revol_bal",
  "revol_util",
  "total_acc",
  "mths_since_earliest_cr_line",
  "acc_now_delinq"
)

# 4. Prepare modelling dataset
model_data <- loan_model_data[
  ,
  c("positive_recovery", predictors)
]

# Convert character predictors to factors
character_vars <- names(model_data)[
  vapply(model_data, is.character, logical(1))
]

for (v in character_vars) {
  model_data[[v]] <- as.factor(model_data[[v]])
}

# Remove unused factor levels, if any
model_data <- droplevels(model_data)

# 5. Stratified train/test split
set.seed(123)

positive_rows <- which(model_data$positive_recovery == 1)
zero_rows <- which(model_data$positive_recovery == 0)

train_positive <- sample(
  positive_rows,
  size = floor(0.8 * length(positive_rows))
)

train_zero <- sample(
  zero_rows,
  size = floor(0.8 * length(zero_rows))
)

train_rows <- sort(c(train_positive, train_zero))
test_rows <- setdiff(seq_len(nrow(model_data)), train_rows)

train_data <- model_data[train_rows, ]
test_data <- model_data[test_rows, ]

# Align factor levels between training and test data
for (v in predictors) {
  if (is.factor(train_data[[v]])) {
    test_data[[v]] <- factor(
      test_data[[v]],
      levels = levels(train_data[[v]])
    )
  }
}

# 6. Confirm split and target balance
cat("\n--- Training set ---\n")
cat("Rows:", nrow(train_data), "\n")
print(table(train_data$positive_recovery))

cat("\n--- Test set ---\n")
cat("Rows:", nrow(test_data), "\n")
print(table(test_data$positive_recovery))

# 7. Fit baseline logistic regression
recovery_formula <- reformulate(
  predictors,
  response = "positive_recovery"
)

recovery_logit <- glm(
  recovery_formula,
  data = train_data,
  family = binomial(link = "logit")
)

cat("\n--- Logistic regression summary ---\n")
print(summary(recovery_logit))

# 8. Generate test-set probabilities
test_data$predicted_probability <- predict(
  recovery_logit,
  newdata = test_data,
  type = "response"
)

cat("\n--- Predicted probabilities ---\n")
print(summary(test_data$predicted_probability))


#-----------------------------------------------
# Evaluation


# Step 2: Evaluate baseline logistic regression

# Install packages if needed, once:
# install.packages(c("pROC", "ggplot2"))

library(pROC)
library(ggplot2)

# 1. Actual and predicted outcomes
actual <- test_data$positive_recovery
prob <- test_data$predicted_probability

# 2. ROC-AUC
roc_obj <- roc(
  response = actual,
  predictor = prob,
  quiet = TRUE
)

auc_value <- as.numeric(auc(roc_obj))

# 3. Brier score and log loss
epsilon <- 1e-15
prob_safe <- pmin(pmax(prob, epsilon), 1 - epsilon)

brier_score <- mean((prob_safe - actual)^2)

log_loss <- -mean(
  actual * log(prob_safe) +
    (1 - actual) * log(1 - prob_safe)
)

# 4. Classification metrics at threshold 0.5
predicted_class <- as.integer(prob >= 0.5)

tp <- sum(predicted_class == 1 & actual == 1)
tn <- sum(predicted_class == 0 & actual == 0)
fp <- sum(predicted_class == 1 & actual == 0)
fn <- sum(predicted_class == 0 & actual == 1)

accuracy <- (tp + tn) / length(actual)
precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
f1_score <- if (!is.na(precision + recall) &&
                (precision + recall) > 0) {
  2 * precision * recall / (precision + recall)
} else {
  NA_real_
}

cat("\n--- Logistic regression: test performance ---\n")

metrics <- data.frame(
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
  Value = c(
    auc_value,
    brier_score,
    log_loss,
    accuracy,
    precision,
    recall,
    specificity,
    f1_score
  )
)

metrics$Value <- round(metrics$Value, 4)
print(metrics, row.names = FALSE)

cat("\nConfusion matrix (rows = actual, columns = predicted):\n")
print(
  table(
    Actual = factor(actual, levels = c(0, 1)),
    Predicted = factor(predicted_class, levels = c(0, 1))
  )
)

# 5. Calibration table: ten probability groups
calibration_data <- data.frame(
  Actual = actual,
  Predicted = prob
)

calibration_data$Group <- dplyr::ntile(
  calibration_data$Predicted,
  10
)

calibration_table <- aggregate(
  cbind(Predicted, Actual) ~ Group,
  data = calibration_data,
  FUN = mean
)

names(calibration_table) <- c(
  "Probability_Group",
  "Mean_Predicted_Probability",
  "Observed_Recovery_Rate"
)

calibration_table$Mean_Predicted_Probability <-
  round(calibration_table$Mean_Predicted_Probability, 4)

calibration_table$Observed_Recovery_Rate <-
  round(calibration_table$Observed_Recovery_Rate, 4)

cat("\n--- Calibration table ---\n")
print(calibration_table, row.names = FALSE)

# 6. Calibration plot
ggplot(
  calibration_table,
  aes(
    x = Mean_Predicted_Probability,
    y = Observed_Recovery_Rate
  )
) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Recovery Probability Model: Calibration",
    x = "Mean predicted probability",
    y = "Observed positive-recovery proportion"
  ) +
  coord_equal() +
  theme_minimal()
