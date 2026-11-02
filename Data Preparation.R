
# ============================================================
# SocGen Assignment
# ============================================================


# SECTION 1: DATA IMPORT
# ------------------------------------------------------------

# Import the CSV file from the current working directory.
# stringsAsFactors = FALSE keeps text columns as character data.
# check.names = FALSE preserves the original column names.
# created object loan_data to store dataset

loan_data <- read.csv(
  "loan_data_defaults.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# SECTION 2: INITIAL DATASET DIMENSIONS
# ------------------------------------------------------------

# Count the number of observations (rows) and variables (columns).

dim(loan_data)



# SECTION 3: COLUMN NAME INSPECTION
# ------------------------------------------------------------

# Display the names of all 212 variables in the dataset.

names(loan_data)




# SECTION 4: INSPECT
# ------------------------------------------------------------

# Display the names and positions of the first five columns.
data.frame(
  position = 1:5,
  column_name = names(loan_data)[1:5]
)

# Inspect the first 6 elements of the first three columns by position.
# This works even if a column has an empty name.
head(loan_data[, 1:3], 6)




# SECTION 5: VERIFY IF DATA REDUNDANT
# ------------------------------------------------------------

# Check whether the first column and the next two Unnamed columns contain identical values across all observations.

all(
  loan_data[[1]] == loan_data[["Unnamed: 0"]]
)

all(
  loan_data[["Unnamed: 0"]] == loan_data[["Unnamed: 0.1"]]
)



# SECTION 6: INSPECT RECOVERY RELATED VARIABLES
# ------------------------------------------------------------
# Display the first six observations of the key outcome variables.
# This is an inspection step only; it does not modify the dataset.

head(
  loan_data[, c("loan_amnt", "funded_amnt", "loan_status",
                "recoveries", "collection_recovery_fee",
                "recovery_rate", "CCF")],
  6
)



# SECTION 7: CHECK LOAN STATUS AND RECOVERY OUTCOMES
# ------------------------------------------------------------

# Count observations in each loan status category.
table(loan_data$loan_status, useNA = "ifany")

# Count observations which are positive  / zero or negative
table(
  ifelse(
    is.na(loan_data$recoveries),
    "Missing",
    ifelse(loan_data$recoveries > 0, "Positive", "Zero or Negative")
  )
)


# SECTION 8: VALIDATE RECOVERY VARIABLE VALUES
# ------------------------------------------------------------

# Check the distribution and range of the key variables.
summary(
  loan_data[, c(
    "loan_amnt",
    "funded_amnt",
    "recoveries",
    "recovery_rate",
    "CCF"
  )]
)

# Count the number of negative recovery amounts.
sum(loan_data$recoveries < 0, na.rm = TRUE)



# SECTION 9: VERIFY THE RECOVERY-RATE FORMULA
# ------------------------------------------------------------

# Calculate recovery rate using the requested loan amount.
calculated_rate_loan <- loan_data$recoveries / loan_data$loan_amnt

# Calculate recovery rate using the funded loan amount.
calculated_rate_funded <- loan_data$recoveries / loan_data$funded_amnt

# Count mismatches between the supplied rate and each calculation.
mismatch_loan <- sum(
  abs(loan_data$recovery_rate - calculated_rate_loan) > 1e-8,
  na.rm = TRUE
)

mismatch_funded <- sum(
  abs(loan_data$recovery_rate - calculated_rate_funded) > 1e-8,
  na.rm = TRUE
)

# Display the mismatch counts.
mismatch_loan
mismatch_funded




# SECTION 10: INSPECT RECOVERY-RATE MISMATCHES
# ------------------------------------------------------------

# Identify rows where the supplied rate differs from the
# rate calculated using funded_amnt.
mismatch_rows <- which(
  abs(loan_data$recovery_rate - calculated_rate_funded) > 1e-8
)

# Create a temporary table for inspection.
# Add the calculated rate as a separate column.
mismatch_data <- loan_data[
  mismatch_rows,
  c("loan_amnt", "funded_amnt", "recoveries",
    "recovery_rate", "CCF")
]

mismatch_data$calculated_recovery_rate <-
  mismatch_data$recoveries / mismatch_data$funded_amnt

# Display the first 10 mismatched observations.
head(mismatch_data, 10)


# SECTION 11: SUMMARISE RECOVERY-RATE MISMATCHES
# ------------------------------------------------------------

# Count mismatched records where the supplied rate is 1.
sum(mismatch_data$recovery_rate == 1)

# Count mismatched records where the calculated rate exceeds 1.
sum(mismatch_data$calculated_recovery_rate > 1)

# Show the range of calculated rates among mismatched records.
range(mismatch_data$calculated_recovery_rate)



# SECTION 12: COMPARE LOAN AMOUNT AND FUNDED AMOUNT
# ------------------------------------------------------------

# Count loans where the two amounts are equal.
sum(loan_data$loan_amnt == loan_data$funded_amnt, na.rm = TRUE)

# Count loans where the two amounts differ.
sum(loan_data$loan_amnt != loan_data$funded_amnt, na.rm = TRUE)

# Summarise the difference between requested and funded amounts.
summary(loan_data$loan_amnt - loan_data$funded_amnt)

# Count loans where the funded amount exceeds the requested amount.
sum(loan_data$funded_amnt > loan_data$loan_amnt, na.rm = TRUE)



# SECTION 13: LINK AMOUNT DIFFERENCES TO RATE MISMATCHES
# ------------------------------------------------------------

# Count loans where loan_amnt and funded_amnt differ
# AND the supplied rate mismatches the funded-amount formula.
sum(
  loan_data$loan_amnt != loan_data$funded_amnt &
    abs(loan_data$recovery_rate - calculated_rate_funded) > 1e-8,
  na.rm = TRUE
)

# Count loans where the supplied rate mismatches the
# loan-amount formula but the two amounts are equal.
sum(
  loan_data$loan_amnt == loan_data$funded_amnt &
    abs(loan_data$recovery_rate - calculated_rate_loan) > 1e-8,
  na.rm = TRUE
)




# SECTION 14: INVESTIGATE CCF
# ------------------------------------------------------------
# Examine whether CCF is associated with key loan and recovery
# variables. These correlations are exploratory, not proof
# of a mathematical formula.

ccf_variables <- loan_data[, c(
  "CCF",
  "loan_amnt",
  "funded_amnt",
  "recoveries",
  "recovery_rate"
)]

# Calculate pairwise correlations between the variables.
cor(
  ccf_variables,
  use = "complete.obs"
)


# SECTION 15: TEST POSSIBLE CCF FORMULAS
# ------------------------------------------------------------
# Calculate plausible ratios and compare them with CCF.
# This helps us identify whether CCF follows a recognizable
# mathematical relationship with the available variables.

ccf_checks <- data.frame(
  CCF = loan_data$CCF,
  Recovery_Rate = loan_data$recovery_rate,
  Unrecovered_Rate = 1 - loan_data$recovery_rate,
  Recovery_to_Funded = loan_data$recoveries / loan_data$funded_amnt,
  Unrecovered_Amount_Ratio =
    (loan_data$funded_amnt - loan_data$recoveries) /
    loan_data$funded_amnt
)

# Display correlations between CCF and the candidate ratios.
cor(
  ccf_checks,
  use = "complete.obs"
)[
  "CCF",
  c("Recovery_Rate", "Unrecovered_Rate",
    "Recovery_to_Funded", "Unrecovered_Amount_Ratio")
]


# SECTION 16: INSPECT CCF VALUES
# ------------------------------------------------------------

# Display the first 15 CCF observations alongside key variables.
head(
  loan_data[, c(
    "loan_amnt",
    "funded_amnt",
    "recoveries",
    "recovery_rate",
    "CCF"
  )],
  15
)

# Check the number of distinct CCF values.
length(unique(loan_data$CCF))

# Check whether any CCF values are missing.
sum(is.na(loan_data$CCF))



# SECTION 17: VERIFY THE CCF FORMULA
# ------------------------------------------------------------

# Recalculate CCF using the documented formula.
calculated_ccf <- (
  loan_data$funded_amnt - loan_data$total_rec_prncp
) / loan_data$funded_amnt

# Count observations where the calculated CCF differs
# from the CCF supplied in the dataset.
ccf_mismatches <- sum(
  abs(loan_data$CCF - calculated_ccf) > 1e-8,
  na.rm = TRUE
)

# Display the mismatch count.
ccf_mismatches

# Display the first 10 observations for direct comparison.
ccf_comparison <- data.frame(
  funded_amnt = loan_data$funded_amnt,
  total_rec_prncp = loan_data$total_rec_prncp,
  CCF_supplied = loan_data$CCF,
  CCF_calculated = calculated_ccf
)

head(ccf_comparison, 10)



# SECTION 18: INSPECT MISSING VALUES (TOP 20 COlUMNS)
# ------------------------------------------------------------
# Count missing values in each column.
# This is an inspection step; the dataset is not modified.

missing_count <- colSums(is.na(loan_data))

# Calculate the percentage of missing values in each column.
missing_percentage <- (missing_count / nrow(loan_data)) * 100

# Create a summary table.
missing_summary <- data.frame(
  column_name = names(loan_data),
  missing_count = missing_count,
  missing_percentage = round(missing_percentage, 2)
)

# Show columns with at least one missing value,
# ordered from highest to lowest missing percentage.
missing_summary <- missing_summary[
  missing_summary$missing_count > 0,
]

missing_summary <- missing_summary[
  order(-missing_summary$missing_percentage),
]

head(missing_summary, 20)



# SECTION 19: OVERALL MISSING-DATA SUMMARY (ALL COLUMNS)
# ------------------------------------------------------------

# Count columns with at least one missing value.
columns_with_missing <- sum(colSums(is.na(loan_data)) > 0)

# Count columns with no observed values.
completely_empty_columns <- sum(colSums(!is.na(loan_data)) == 0)

# Count columns with no missing values.
complete_columns <- sum(colSums(is.na(loan_data)) == 0)

# Count the total number of missing cells in the dataset.
total_missing_cells <- sum(is.na(loan_data))

# Display the summary.
data.frame(
  total_rows = nrow(loan_data),
  total_columns = ncol(loan_data),
  columns_with_missing = columns_with_missing,
  completely_empty_columns = completely_empty_columns,
  complete_columns = complete_columns,
  total_missing_cells = total_missing_cells
)



# SECTION 20: INSPECT PARTIALLY MISSING COLUMNS (22-17 = 5 COLUMNS)
# ------------------------------------------------------------

# Identify columns that have some missing values,
# but are not completely empty.
partial_missing_columns <- missing_summary[
  missing_summary$missing_count < nrow(loan_data),
]

partial_missing_columns



# SECTION 21: MISSINGNESS AND RECOVERY OUTCOMES
# ------------------------------------------------------------

# Create an indicator showing whether total current balance
# is missing for each loan.
balance_missing <- is.na(loan_data$tot_cur_bal)

# Identify whether each loan has a positive recovery.
positive_recovery <- loan_data$recoveries > 0

# Calculate the positive-recovery rate for each group.
recovery_rate_by_missingness <- aggregate(
  positive_recovery,
  by = list(balance_missing = balance_missing),
  FUN = mean
)

# Add the number of observations in each group.
group_sizes <- table(balance_missing)

recovery_rate_by_missingness$loan_count <-
  as.integer(group_sizes[
    as.character(recovery_rate_by_missingness$balance_missing)
  ])

# Display the comparison.
recovery_rate_by_missingness



# SECTION 22: STATISTICAL SIGNIFICANCE TEST FOR DIFFERENCE IN POSITIVE-RECOVERY RATES
# ------------------------------------------------------------

# Count positive recoveries in each missingness group.
positive_counts <- c(
  sum(positive_recovery[!balance_missing]),
  sum(positive_recovery[balance_missing])
)

# Count total loans in each group.
total_counts <- c(
  sum(!balance_missing),
  sum(balance_missing)
)

# Perform a two-proportion test.
prop.test(
  x = positive_counts,
  n = total_counts,
  correct = FALSE
)



# SECTION 23: MISSINGNESS IN TOTAL COLLECTION AMOUNT
# ------------------------------------------------------------

# Identify loans where total collection amount is missing.
collection_missing <- is.na(loan_data$tot_coll_amt)

# Compare positive-recovery rates across the two groups.
collection_recovery_comparison <- aggregate(
  positive_recovery,
  by = list(collection_missing = collection_missing),
  FUN = mean
)

# Add the number of loans in each group.
collection_group_sizes <- table(collection_missing)

collection_recovery_comparison$loan_count <-
  as.integer(collection_group_sizes[
    as.character(collection_recovery_comparison$collection_missing)
  ])

collection_recovery_comparison



# SECTION 24: COMPARE MISSINGNESS PATTERNS
# ------------------------------------------------------------

# Count loans where both variables are missing.
sum(
  is.na(loan_data$tot_cur_bal) &
    is.na(loan_data$tot_coll_amt)
)

# Count loans where only total current balance is missing.
sum(
  is.na(loan_data$tot_cur_bal) &
    !is.na(loan_data$tot_coll_amt)
)

# Count loans where only total collection amount is missing.
sum(
  !is.na(loan_data$tot_cur_bal) &
    is.na(loan_data$tot_coll_amt)
)


# SECTION 25: INSPECT LOW-MISSINGNESS VARIABLES
# ------------------------------------------------------------

# Summarise the distributions of both variables.
summary(
  loan_data[, c("revol_util", "collections_12_mths_ex_med")]
)

# Count the observed zero values in each variable.
sum(loan_data$revol_util == 0, na.rm = TRUE)

sum(
  loan_data$collections_12_mths_ex_med == 0,
  na.rm = TRUE
)


# SECTION 26: LIST COMPLETELY EMPTY COLUMNS
# ------------------------------------------------------------

# Identify columns with no observed values.
empty_columns <- names(loan_data)[
  colSums(!is.na(loan_data)) == 0
]

# Display their names.
empty_columns

# Confirm the number of empty columns.
length(empty_columns)



# SECTION 27: INSPECT VARIABLE DATA TYPES
# ------------------------------------------------------------

# Count how many columns belong to each R data type.
table(sapply(loan_data, class))

# Display the data type of each column.
data_types <- data.frame(
  column_name = names(loan_data),
  data_type = sapply(loan_data, function(x) class(x)[1])
)

head(data_types, 30)


# Check all data types present within the dataset
table(sapply(loan_data, function(x) class(x)[1]))



# SECTION 29: INSPECT HOW MANY LOGICAL COLUMNS ARE EMPTY COLUMNS
# ------------------------------------------------------------

# Identify all columns stored as logical.
logical_columns <- names(loan_data)[
  sapply(loan_data, is.logical)
]

# Display their names.
logical_columns

# Check how many logical columns are completely empty.
sum(logical_columns %in% empty_columns)



# SECTION 30: CHECK FOR EXACT DUPLICATE ROWS
# ------------------------------------------------------------

# Count rows that exactly duplicate an earlier row.
sum(duplicated(loan_data))

# Check whether the loan ID is unique for every row.
sum(duplicated(loan_data$id))


# SECTION 31: VERIFY LOAN-STATUS DISTRIBUTION
# ------------------------------------------------------------

# Count observations for each loan status.
table(loan_data$loan_status, useNA = "ifany")

# Calculate the percentage represented by each status.
round(
  prop.table(table(loan_data$loan_status, useNA = "ifany")) * 100,
  2
)



# SECTION 32: SUMMARISE RECOVERY OUTCOMES
# ------------------------------------------------------------

# Classify each loan according to whether any recovery occurred.
recovery_outcome <- ifelse(
  loan_data$recoveries > 0,
  "Positive recovery",
  "Zero recovery"
)

# Display counts and percentages.
recovery_counts <- table(recovery_outcome)

data.frame(
  recovery_outcome = names(recovery_counts),
  loan_count = as.integer(recovery_counts),
  percentage = round(
    as.integer(recovery_counts) / nrow(loan_data) * 100,
    2
  )
)



#------------------------
#  -----------------------
  # BEGIN FEATURE SELECTION
 # -----------
 # ------------
  
  # Section 33: Inspect the first 60 variable names
  
  names(loan_data)[1:60]

# Section 34: Inspect the remaining variable names

names(loan_data)[61:120]

# Section 35: Inspect the remaining variable names

names(loan_data)[121:212]

# Section 36: Inspect the good_bad variable

table(loan_data$good_bad, useNA = "ifany")

# Section 37: Verify the good_bad distribution

unique(loan_data$good_bad)
table(loan_data$good_bad, useNA = "ifany")
sum(is.na(loan_data$good_bad))

# Section 37A: Check the data type of good_bad

class(loan_data$good_bad)
typeof(loan_data$good_bad)

# Section 38: Inspect encoded loan_status columns

loan_status_encoded <- grep(
  "^loan_status:",
  names(loan_data),
  value = TRUE
)

sapply(
  loan_data[loan_status_encoded],
  function(x) sum(x == 1, na.rm = TRUE)
)

# Section 39: Check consistency of grade indicators

grade_encoded <- grep("^grade:", names(loan_data), value = TRUE)

# Check how many grade indicators are active in each row
grade_indicator_sum <- rowSums(
  loan_data[grade_encoded] == 1,
  na.rm = TRUE
)

table(grade_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original grade column
grade_indicator_matches <- vapply(
  grade_encoded,
  function(col) {
    grade_label <- sub("^grade:", "", col)
    
    expected <- !is.na(loan_data$grade) &
      loan_data$grade == grade_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

grade_indicator_matches

# Check missing values in the original grade column
sum(is.na(loan_data$grade))



# Section 40: Check consistency of sub-grade indicators

sub_grade_encoded <- grep(
  "^sub_grade:",
  names(loan_data),
  value = TRUE
)

# Check how many sub-grade indicators are active in each row
sub_grade_indicator_sum <- rowSums(
  loan_data[sub_grade_encoded] == 1,
  na.rm = TRUE
)

table(sub_grade_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original sub_grade column
sub_grade_indicator_matches <- vapply(
  sub_grade_encoded,
  function(col) {
    sub_grade_label <- sub("^sub_grade:", "", col)
    
    expected <- !is.na(loan_data$sub_grade) &
      loan_data$sub_grade == sub_grade_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

sub_grade_indicator_matches

# Check missing values in the original sub_grade column
sum(is.na(loan_data$sub_grade))


# SECTION 41: Check home ownership indicators
#--------------------------------------------------

home_ownership_encoded <- grep(
  "^home_ownership:",
  names(loan_data),
  value = TRUE
)

# Check how many indicators are active in each row
home_ownership_indicator_sum <- rowSums(
  loan_data[home_ownership_encoded] == 1,
  na.rm = TRUE
)

table(home_ownership_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original column
home_ownership_indicator_matches <- vapply(
  home_ownership_encoded,
  function(col) {
    ownership_label <- sub("^home_ownership:", "", col)
    
    expected <- !is.na(loan_data$home_ownership) &
      loan_data$home_ownership == ownership_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

home_ownership_indicator_matches

# Check missing values in the original column
sum(is.na(loan_data$home_ownership))


# SECTION 42: Check verification status indicators
#--------------------------------------------------

verification_encoded <- grep(
  "^verification_status:",
  names(loan_data),
  value = TRUE
)

# Check how many indicators are active in each row
verification_indicator_sum <- rowSums(
  loan_data[verification_encoded] == 1,
  na.rm = TRUE
)

table(verification_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original column
verification_indicator_matches <- vapply(
  verification_encoded,
  function(col) {
    status_label <- sub("^verification_status:", "", col)
    
    expected <- !is.na(loan_data$verification_status) &
      loan_data$verification_status == status_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

verification_indicator_matches

# Check missing values in the original column
sum(is.na(loan_data$verification_status))


# SECTION 43: Check loan purpose indicators
#--------------------------------------------------

purpose_encoded <- grep(
  "^purpose:",
  names(loan_data),
  value = TRUE
)

# Check how many indicators are active in each row
purpose_indicator_sum <- rowSums(
  loan_data[purpose_encoded] == 1,
  na.rm = TRUE
)

table(purpose_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original column
purpose_indicator_matches <- vapply(
  purpose_encoded,
  function(col) {
    purpose_label <- sub("^purpose:", "", col)
    
    expected <- !is.na(loan_data$purpose) &
      loan_data$purpose == purpose_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

purpose_indicator_matches

# Check missing values in the original column
sum(is.na(loan_data$purpose))

# SECTION 44: Check address state indicators
#--------------------------------------------------

state_encoded <- grep(
  "^addr_state:",
  names(loan_data),
  value = TRUE
)

# Check how many indicators are active in each row
state_indicator_sum <- rowSums(
  loan_data[state_encoded] == 1,
  na.rm = TRUE
)

# SECTION 45: Check initial listing status indicators
#--------------------------------------------------

listing_status_encoded <- grep(
  "^initial_list_status:",
  names(loan_data),
  value = TRUE
)

# Check how many indicators are active in each row
listing_status_indicator_sum <- rowSums(
  loan_data[listing_status_encoded] == 1,
  na.rm = TRUE
)

table(listing_status_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original column
listing_status_indicator_matches <- vapply(
  listing_status_encoded,
  function(col) {
    status_label <- sub("^initial_list_status:", "", col)
    
    expected <- !is.na(loan_data$initial_list_status) &
      loan_data$initial_list_status == status_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

listing_status_indicator_matches

# Check missing values in the original column
sum(is.na(loan_data$initial_list_status))

table(state_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original column
state_indicator_matches <- vapply(
  state_encoded,
  function(col) {
    state_label <- sub("^addr_state:", "", col)
    
    expected <- !is.na(loan_data$addr_state) &
      loan_data$addr_state == state_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

state_indicator_matches

# Check missing values in the original column
sum(is.na(loan_data$addr_state))


# SECTION 45: Check initial listing status indicators
#--------------------------------------------------

listing_status_encoded <- grep(
  "^initial_list_status:",
  names(loan_data),
  value = TRUE
)

# Check how many indicators are active in each row
listing_status_indicator_sum <- rowSums(
  loan_data[listing_status_encoded] == 1,
  na.rm = TRUE
)

table(listing_status_indicator_sum, useNA = "ifany")

# Compare encoded indicators with the original column
listing_status_indicator_matches <- vapply(
  listing_status_encoded,
  function(col) {
    status_label <- sub("^initial_list_status:", "", col)
    
    expected <- !is.na(loan_data$initial_list_status) &
      loan_data$initial_list_status == status_label
    
    observed <- !is.na(loan_data[[col]]) &
      loan_data[[col]] == 1
    
    all(expected == observed) &&
      !any(is.na(loan_data[[col]]))
  },
  logical(1)
)

listing_status_indicator_matches

# Check missing values in the original column
sum(is.na(loan_data$initial_list_status))


# SECTION 46: Inspect index and identifier columns
#--------------------------------------------------

identifier_columns <- c(
  "",
  "Unnamed: 0",
  "Unnamed: 0.1",
  "id",
  "member_id"
)

# Check which columns exist
identifier_columns %in% names(loan_data)

# Summarise distinct values and missingness
identifier_summary <- data.frame(
  column = identifier_columns,
  distinct_values = sapply(
    identifier_columns,
    function(col) {
      if (col %in% names(loan_data)) {
        length(unique(loan_data[[col]]))
      } else {
        NA_integer_
      }
    }
  ),
  missing_values = sapply(
    identifier_columns,
    function(col) {
      if (col %in% names(loan_data)) {
        sum(is.na(loan_data[[col]]))
      } else {
        NA_integer_
      }
    }
  )
)

identifier_summary


# SECTION 47: Verify index-like columns
#--------------------------------------------------

# Check whether the two index-like columns are identical
identical(
  loan_data[["Unnamed: 0"]],
  loan_data[["Unnamed: 0.1"]]
)

# Inspect the first and last values of each index-like column
head(loan_data[["Unnamed: 0"]])
tail(loan_data[["Unnamed: 0"]])

head(loan_data[["Unnamed: 0.1"]])
tail(loan_data[["Unnamed: 0.1"]])

# Inspect the blank-name column
blank_column <- loan_data[[""]]

sum(is.na(blank_column))
length(unique(blank_column))
head(blank_column)

# SECTION 48: Compare loan and member identifiers
#--------------------------------------------------

# Check whether the two identifier columns are identical
identical(loan_data$id, loan_data$member_id)

# Summarise their ranges and distributions
summary(loan_data$id)
summary(loan_data$member_id)

# Check for duplicate identifiers
sum(duplicated(loan_data$id))
sum(duplicated(loan_data$member_id))


# SECTION 49: Remove confirmed redundant and unusable columns
#--------------------------------------------------

# 1. List the 17 completely empty columns
empty_columns <- c(
  "annual_inc_joint",
  "dti_joint",
  "verification_status_joint",
  "open_acc_6m",
  "open_il_6m",
  "open_il_12m",
  "open_il_24m",
  "mths_since_rcnt_il",
  "total_bal_il",
  "il_util",
  "open_rv_12m",
  "open_rv_24m",
  "max_bal_bc",
  "all_util",
  "inq_fi",
  "total_cu_tl",
  "inq_last_12m"
)

# 2. Identify all one-hot encoded columns we have audited
encoded_columns <- grep(
  "^(grade:|sub_grade:|home_ownership:|verification_status:|loan_status:|purpose:|addr_state:|initial_list_status:)",
  names(loan_data)
)

# 3. Identify the columns to exclude
drop_indices <- unique(c(
  1:3,
  which(names(loan_data) %in% empty_columns),
  which(names(loan_data) == "good_bad"),
  encoded_columns
))

# 4. Create the cleaned working dataset
loan_clean <- loan_data[, -drop_indices, drop = FALSE]

# 5. Verify the dimensions
dim(loan_data)
dim(loan_clean)

# 6. Verify that the confirmed exclusions are absent
names(loan_clean)[
  names(loan_clean) %in% c(empty_columns, "good_bad")
]

grep(
  "^(grade:|sub_grade:|home_ownership:|verification_status:|loan_status:|purpose:|addr_state:|initial_list_status:)",
  names(loan_clean),
  value = TRUE
)

# SECTION 50: Inspect the remaining categorical variables
#--------------------------------------------------

# Identify character columns in the cleaned dataset
character_columns <- names(loan_clean)[
  sapply(loan_clean, is.character)
]

character_columns

# Count distinct values in each character column
sapply(
  loan_clean[character_columns],
  function(x) length(unique(x))
)


# SECTION 51: Check constant categorical variables
#--------------------------------------------------

# Count distinct non-missing values in each character column
character_unique_counts <- sapply(
  loan_clean[character_columns],
  function(x) length(unique(x[!is.na(x)]))
)

# Display variables with only one distinct non-missing value
character_unique_counts[character_unique_counts <= 1]

# Show the actual values of these variables
constant_character_columns <- names(
  character_unique_counts[character_unique_counts <= 1]
)

lapply(
  loan_clean[constant_character_columns],
  unique
)

# SECTION 52: Inspect date-related columns
#--------------------------------------------------

date_columns <- c(
  "issue_d",
  "issue_d_date",
  "earliest_cr_line",
  "earliest_cr_line_date",
  "last_pymnt_d",
  "next_pymnt_d",
  "last_credit_pull_d"
)

# Display the first six values from each date-related column
lapply(
  date_columns,
  function(col) {
    if (col %in% names(loan_clean)) {
      head(loan_clean[[col]])
    } else {
      paste("Column not found:", col)
    }
  }
) |> setNames(date_columns)

# Display the data types
sapply(
  loan_clean[date_columns[date_columns %in% names(loan_clean)]],
  class
)

# SECTION 53: Consolidated final data audit
#--------------------------------------------------

# 1. Identify columns with no variation
unique_counts <- sapply(
  loan_clean,
  function(x) length(unique(x[!is.na(x)]))
)

constant_columns <- names(unique_counts[unique_counts <= 1])

cat("\n--- CONSTANT COLUMNS ---\n")
print(constant_columns)

# 2. Check duplicate date representations
cat("\n--- DATE PAIR CHECKS ---\n")

cat("issue_d and issue_d_date agree:",
    all(
      format(
        as.Date(loan_clean$issue_d_date),
        "%b-%y"
      ) == loan_clean$issue_d,
      na.rm = TRUE
    ), "\n")

cat("earliest_cr_line and earliest_cr_line_date agree:",
    all(
      format(
        as.Date(loan_clean$earliest_cr_line_date),
        "%b-%y"
      ) == loan_clean$earliest_cr_line,
      na.rm = TRUE
    ), "\n")

# 3. Summarize missing values, including empty character strings
missing_summary <- data.frame(
  column = names(loan_clean),
  NA_count = sapply(loan_clean, function(x) sum(is.na(x))),
  empty_string_count = sapply(
    loan_clean,
    function(x) {
      if (is.character(x)) sum(trimws(x) == "", na.rm = TRUE)
      else 0
    }
  )
)

missing_summary <- missing_summary[
  missing_summary$NA_count > 0 |
    missing_summary$empty_string_count > 0,
]

missing_summary <- missing_summary[
  order(
    -missing_summary$NA_count,
    -missing_summary$empty_string_count
  ),
]

cat("\n--- COLUMNS WITH MISSING OR EMPTY VALUES ---\n")
print(missing_summary, row.names = FALSE)

# 4. Flag variables that may contain post-origination information
potential_leakage <- c(
  "last_pymnt_d",
  "next_pymnt_d",
  "last_credit_pull_d",
  "last_pymnt_amnt",
  "total_pymnt",
  "total_pymnt_inv",
  "total_rec_prncp",
  "total_rec_int",
  "total_rec_late_fee",
  "collection_recovery_fee",
  "out_prncp",
  "out_prncp_inv",
  "CCF"
)

cat("\n--- POTENTIAL LEAKAGE / POST-ORIGINATION VARIABLES ---\n")
print(intersect(potential_leakage, names(loan_clean)))

# 5. Confirm the recovery outcome is usable
cat("\n--- RECOVERY OUTCOME CHECK ---\n")

cat("Rows:", nrow(loan_clean), "\n")
cat("Missing recovery_rate:",
    sum(is.na(loan_clean$recovery_rate)), "\n")
cat("Missing recoveries:",
    sum(is.na(loan_clean$recoveries)), "\n")
cat("Recovery-rate range:\n")
print(range(loan_clean$recovery_rate, na.rm = TRUE))

cat("\n--- CURRENT WORKING DATASET ---\n")
cat("Rows:", nrow(loan_clean),
    "| Columns:", ncol(loan_clean), "\n")


# SECTION 54: Apply confirmed removals and format dates
#--------------------------------------------------

# 1. Define confirmed columns to remove
confirmed_drop <- c(
  "pymnt_plan",
  "out_prncp",
  "out_prncp_inv",
  "policy_code",
  "application_type",
  "issue_d",
  "earliest_cr_line",
  "url"
)

# 2. Remove confirmed columns from the working dataset
confirmed_drop <- intersect(
  confirmed_drop,
  names(loan_clean)
)

loan_clean <- loan_clean[
  ,
  !names(loan_clean) %in% confirmed_drop,
  drop = FALSE
]

# 3. Convert retained date columns to Date format
loan_clean$issue_d_date <- as.Date(
  loan_clean$issue_d_date,
  format = "%Y-%m-%d"
)

loan_clean$earliest_cr_line_date <- as.Date(
  loan_clean$earliest_cr_line_date,
  format = "%Y-%m-%d"
)

# 4. Verify the changes
cat("Columns removed:\n")
print(confirmed_drop)

cat("\nUpdated dataset dimensions:\n")
print(dim(loan_clean))

cat("\nDate column classes:\n")
print(sapply(
  loan_clean[c("issue_d_date", "earliest_cr_line_date")],
  class
))

cat("\nMissing recovery outcomes:\n")
cat("recoveries:", sum(is.na(loan_clean$recoveries)), "\n")
cat("recovery_rate:", sum(is.na(loan_clean$recovery_rate)), "\n")

# SECTION 55: Consolidated audit of remaining data-preparation issues
#--------------------------------------------------

# 1. Check the supplied recovery-rate definition
raw_rate_funded <- loan_clean$recoveries / loan_clean$funded_amnt
raw_rate_loan <- loan_clean$recoveries / loan_clean$loan_amnt

cat("RECOVERY-RATE DEFINITION\n")

cat(
  "Mismatches against recoveries / funded_amnt:",
  sum(abs(loan_clean$recovery_rate - raw_rate_funded) > 1e-8),
  "\n"
)

cat(
  "Mismatches against capped funded-amnt ratio:",
  sum(abs(loan_clean$recovery_rate - pmin(raw_rate_funded, 1)) > 1e-8),
  "\n"
)

cat(
  "Mismatches against recoveries / loan_amnt:",
  sum(abs(loan_clean$recovery_rate - raw_rate_loan) > 1e-8),
  "\n"
)

cat(
  "Recoveries exceeding funded_amnt:",
  sum(loan_clean$recoveries > loan_clean$funded_amnt),
  "\n"
)

cat(
  "Recoveries exceeding loan_amnt:",
  sum(loan_clean$recoveries > loan_clean$loan_amnt),
  "\n"
)

# 2. Check consistency of derived variables
cat("\nDERIVED-VARIABLE CONSISTENCY\n")

if (all(c("term", "term_int") %in% names(loan_clean))) {
  print(table(loan_clean$term, loan_clean$term_int, useNA = "ifany"))
}

if (all(c("emp_length", "emp_length_int") %in% names(loan_clean))) {
  cat(
    "Employment-length missingness:\n"
  )
  print(c(
    emp_length_missing = sum(is.na(loan_clean$emp_length) |
                               trimws(loan_clean$emp_length) == ""),
    emp_length_int_missing = sum(is.na(loan_clean$emp_length_int))
  ))
  
  cat(
    "Distinct numeric employment-length values:\n"
  )
  print(sort(unique(loan_clean$emp_length_int)))
}

if (all(c("issue_d_date", "mths_since_issue_d") %in% names(loan_clean))) {
  cat(
    "Issue-date missing:",
    sum(is.na(loan_clean$issue_d_date)),
    "\n"
  )
  cat(
    "Months-since-issue missing:",
    sum(is.na(loan_clean$mths_since_issue_d)),
    "\n"
  )
  cat(
    "Months-since-issue range:\n"
  )
  print(range(loan_clean$mths_since_issue_d, na.rm = TRUE))
}

if (all(c("earliest_cr_line_date",
          "mths_since_earliest_cr_line") %in% names(loan_clean))) {
  cat(
    "Months-since-credit-history range:\n"
  )
  print(range(
    loan_clean$mths_since_earliest_cr_line,
    na.rm = TRUE
  ))
}

# 3. Summarise the remaining columns' types
cat("\nREMAINING COLUMN TYPES\n")
print(sort(table(sapply(loan_clean, function(x) class(x)[1])),
           decreasing = TRUE))

# 4. Check remaining missing values and empty strings
cat("\nREMAINING MISSING VALUES\n")

missing_summary <- data.frame(
  column = names(loan_clean),
  class = sapply(loan_clean, function(x) class(x)[1]),
  missing_count = sapply(loan_clean, function(x) sum(is.na(x))),
  empty_string_count = sapply(
    loan_clean,
    function(x) {
      if (is.character(x)) {
        sum(!is.na(x) & trimws(x) == "")
      } else {
        0
      }
    }
  ),
  row.names = NULL
)

print(
  missing_summary[
    missing_summary$missing_count > 0 |
      missing_summary$empty_string_count > 0,
  ],
  row.names = FALSE
)

# 5. Review remaining variables that may contain post-outcome information
potential_leakage <- intersect(
  c(
    "last_pymnt_d",
    "last_pymnt_amnt",
    "next_pymnt_d",
    "last_credit_pull_d",
    "total_pymnt",
    "total_pymnt_inv",
    "total_rec_prncp",
    "total_rec_int",
    "total_rec_late_fee",
    "collection_recovery_fee",
    "CCF",
    "mths_since_issue_d"
  ),
  names(loan_clean)
)

cat("\nPOTENTIAL LEAKAGE / TIMING-SENSITIVE VARIABLES\n")
print(potential_leakage)

# 6. Check outcome and dataset integrity
cat("\nDATASET INTEGRITY\n")
cat("Rows:", nrow(loan_clean), "\n")
cat("Columns:", ncol(loan_clean), "\n")
cat("Missing recoveries:", sum(is.na(loan_clean$recoveries)), "\n")
cat("Missing recovery_rate:", sum(is.na(loan_clean$recovery_rate)), "\n")
cat(
  "Positive recoveries:",
  sum(loan_clean$recoveries > 0),
  "\n"
)
cat(
  "Zero recoveries:",
  sum(loan_clean$recoveries == 0),
  "\n"
)
cat("Exact duplicate rows:", sum(duplicated(loan_clean)), "\n")

# SECTION 56: Standardise empty text values and reassess missingness
#--------------------------------------------------

# 1. Convert blank or whitespace-only character values to NA
character_columns <- names(loan_clean)[
  sapply(loan_clean, is.character)
]

for (col in character_columns) {
  blank_values <- !is.na(loan_clean[[col]]) &
    trimws(loan_clean[[col]]) == ""
  
  loan_clean[[col]][blank_values] <- NA
}

# 2. Recalculate missingness after standardisation
missing_summary <- data.frame(
  column = names(loan_clean),
  class = sapply(loan_clean, function(x) class(x)[1]),
  missing_count = sapply(loan_clean, function(x) sum(is.na(x))),
  missing_percent = round(
    sapply(loan_clean, function(x) mean(is.na(x)) * 100),
    2
  ),
  row.names = NULL
)

cat("COLUMNS WITH MISSING VALUES\n")

print(
  missing_summary[missing_summary$missing_count > 0, ],
  row.names = FALSE
)

# 3. Verify that no blank character values remain
remaining_blanks <- sapply(
  loan_clean[character_columns],
  function(x) sum(!is.na(x) & trimws(x) == "")
)

cat("\nRemaining blank character values:\n")
print(remaining_blanks[remaining_blanks > 0])

# 4. Confirm outcome fields remain complete
cat("\nOUTCOME COMPLETENESS\n")
cat("Missing recoveries:", sum(is.na(loan_clean$recoveries)), "\n")
cat("Missing recovery_rate:", sum(is.na(loan_clean$recovery_rate)), "\n")


# SECTION 57: Validate employment-length encoding and credit-history dates
#--------------------------------------------------

# 1. Compare original and numeric employment-length representations
cat("EMPLOYMENT-LENGTH MAPPING\n")

print(
  table(
    emp_length = loan_clean$emp_length,
    emp_length_int = loan_clean$emp_length_int,
    useNA = "ifany"
  )
)

# 2. Inspect rows where employment length is missing
cat("\nROWS WITH MISSING EMPLOYMENT LENGTH\n")

print(
  head(
    loan_clean[
      is.na(loan_clean$emp_length),
      c("emp_length", "emp_length_int", "grade", "annual_inc"),
      drop = FALSE
    ],
    10
  )
)

cat(
  "Count with missing emp_length but numeric value zero:",
  sum(
    is.na(loan_clean$emp_length) &
      loan_clean$emp_length_int == 0,
    na.rm = TRUE
  ),
  "\n"
)

# 3. Inspect rows with missing earliest credit-line dates
cat("\nMISSING EARLIEST CREDIT-LINE DATES\n")

print(
  loan_clean[
    is.na(loan_clean$earliest_cr_line_date),
    c(
      "earliest_cr_line_date",
      "issue_d_date",
      "mths_since_earliest_cr_line"
    ),
    drop = FALSE
  ]
)

# 4. Check whether missing dates align with missing derived durations
cat("\nDATE AND DERIVED-DURATION MISSINGNESS\n")

print(
  table(
    date_missing = is.na(loan_clean$earliest_cr_line_date),
    duration_missing = is.na(loan_clean$mths_since_earliest_cr_line)
  )
)

# 5. Check the relationship between the two dates and the derived duration
date_check <- !is.na(loan_clean$earliest_cr_line_date) &
  !is.na(loan_clean$issue_d_date) &
  !is.na(loan_clean$mths_since_earliest_cr_line)

calculated_months <- (
  as.integer(format(loan_clean$issue_d_date[date_check], "%Y")) -
    as.integer(format(loan_clean$earliest_cr_line_date[date_check], "%Y"))
) * 12 +
  as.integer(format(loan_clean$issue_d_date[date_check], "%m")) -
  as.integer(format(loan_clean$earliest_cr_line_date[date_check], "%m"))

cat(
  "\nRows where calculated and supplied credit-history months differ:",
  sum(
    calculated_months !=
      loan_clean$mths_since_earliest_cr_line[date_check]
  ),
  "\n"
)

# SECTION 58: Investigate credit-history duration calculation
#--------------------------------------------------

# 1. Calculate duration in months as of loan issuance
date_check <- !is.na(loan_clean$earliest_cr_line_date) &
  !is.na(loan_clean$issue_d_date) &
  !is.na(loan_clean$mths_since_earliest_cr_line)

issue_months <- (
  as.integer(format(loan_clean$issue_d_date[date_check], "%Y")) -
    as.integer(format(
      loan_clean$earliest_cr_line_date[date_check], "%Y"
    ))
) * 12 +
  as.integer(format(loan_clean$issue_d_date[date_check], "%m")) -
  as.integer(format(
    loan_clean$earliest_cr_line_date[date_check], "%m"
  ))

supplied_months <- loan_clean$mths_since_earliest_cr_line[date_check]

# 2. Summarise the supplied and recalculated durations
cat("SUPPLIED DURATION SUMMARY\n")
print(summary(supplied_months))

cat("\nDURATION CALCULATED AT LOAN ISSUANCE\n")
print(summary(issue_months))

# 3. Examine the difference between the two
duration_difference <- supplied_months - issue_months

cat("\nSUPPLIED MINUS ISSUE-DATE DURATION\n")
print(summary(duration_difference))

cat(
  "\nMinimum difference:",
  min(duration_difference),
  "\nMaximum difference:",
  max(duration_difference),
  "\nMedian difference:",
  median(duration_difference),
  "\n")

# 4. Inspect representative observations
cat("\nREPRESENTATIVE DATE AND DURATION VALUES\n")

print(
  head(
    loan_clean[
      date_check,
      c(
        "earliest_cr_line_date",
        "issue_d_date",
        "mths_since_earliest_cr_line"
      ),
      drop = FALSE
    ],
    10
  )
)

# 5. Inspect the three missing credit-line dates
cat("\nROWS WITH MISSING CREDIT-LINE DATES\n")

print(
  loan_clean[
    is.na(loan_clean$earliest_cr_line_date),
    c(
      "issue_d_date",
      "earliest_cr_line_date",
      "mths_since_earliest_cr_line"
    ),
    drop = FALSE
  ]
)

# SECTION 59: Prepare baseline modelling dataset
#--------------------------------------------------

# 1. Create a separate copy for modelling
loan_model_data <- loan_clean

# 2. Correct employment-length coding
# Missing employment length must not be treated as zero years.
loan_model_data$emp_length_model <- as.character(
  loan_model_data$emp_length
)

loan_model_data$emp_length_model[
  is.na(loan_model_data$emp_length_model)
] <- "Unknown"

loan_model_data$emp_length_model <- factor(
  loan_model_data$emp_length_model
)

# 3. Create missingness indicators before imputing
variables_with_missingness <- c(
  "tot_coll_amt",
  "tot_cur_bal"
)

for (col in variables_with_missingness) {
  loan_model_data[[paste0(col, "_missing")]] <-
    as.integer(is.na(loan_model_data[[col]]))
}

# 4. Impute selected numerical predictors using their medians
# Median imputation is robust to skewness and extreme values.
imputation_variables <- c(
  "revol_util",
  "collections_12_mths_ex_med",
  "tot_coll_amt",
  "tot_cur_bal"
)

imputation_values <- numeric(length(imputation_variables))
names(imputation_values) <- imputation_variables

for (col in imputation_variables) {
  median_value <- median(
    loan_model_data[[col]],
    na.rm = TRUE
  )
  
  imputation_values[col] <- median_value
  
  missing_rows <- is.na(loan_model_data[[col]])
  
  loan_model_data[[col]][missing_rows] <- median_value
}

# 5. Verify the preparation
cat("MODELLING DATASET DIMENSIONS\n")
print(dim(loan_model_data))

cat("\nEMPLOYMENT-LENGTH MODEL CATEGORIES\n")
print(table(loan_model_data$emp_length_model, useNA = "ifany"))

cat("\nMEDIAN IMPUTATION VALUES\n")
print(imputation_values)

cat("\nMISSINGNESS INDICATOR COUNTS\n")
print(sapply(
  loan_model_data[
    c("tot_coll_amt_missing", "tot_cur_bal_missing")
  ],
  sum
))

cat("\nMISSING VALUES AFTER IMPUTATION\n")
print(sapply(
  loan_model_data[imputation_variables],
  function(x) sum(is.na(x))
))

cat("\nORIGINAL DATASET PRESERVED\n")
cat("Original rows:", nrow(loan_clean), "\n")
cat("Modelling rows:", nrow(loan_model_data), "\n")

# SECTION 61: Check whether collection variables contain useful variation
#--------------------------------------------------

collection_variables <- c(
  "collections_12_mths_ex_med",
  "tot_coll_amt"
)

for (v in collection_variables) {
  x <- loan_clean[[v]]
  observed <- x[!is.na(x)]
  
  cat("\nVariable:", v, "\n")
  cat("Observed values:", length(observed), "\n")
  cat("Missing values:", sum(is.na(x)), "\n")
  cat("Observed zeros:", sum(observed == 0), "\n")
  cat("Observed zeros (%):",
      round(mean(observed == 0) * 100, 2), "%\n")
  cat("Observed positive values:", sum(observed > 0), "\n")
  cat("Observed positive values (%):",
      round(mean(observed > 0) * 100, 2), "%\n")
  cat("Distinct observed values:", length(unique(observed)), "\n")
}

# SECTION 62: Exclude near-constant collection-count variable
#--------------------------------------------------

# Remove the variable from the modelling dataset
loan_model_data$collections_12_mths_ex_med <- NULL

# Update the imputation-variable list
imputation_variables <- setdiff(
  imputation_variables,
  "collections_12_mths_ex_med"
)

# Confirm the removal
stopifnot(!"collections_12_mths_ex_med" %in% names(loan_model_data))
stopifnot(!"collections_12_mths_ex_med" %in% imputation_variables)

cat("Variable excluded from modelling dataset.\n")
cat("Descriptive dataset dimensions:",
    nrow(loan_clean), "rows x", ncol(loan_clean), "columns\n")
cat("Modelling dataset dimensions:",
    nrow(loan_model_data), "rows x", ncol(loan_model_data), "columns\n")
cat("Remaining imputation variables:\n")
print(imputation_variables)

# SECTION 63: Final validation and save prepared datasets
#--------------------------------------------------

# 1. Validate dimensions and row integrity
stopifnot(nrow(loan_clean) == 43236)
stopifnot(nrow(loan_model_data) == nrow(loan_clean))
stopifnot(!anyDuplicated(loan_clean))

# 2. Validate key outcomes and exposure
stopifnot(!anyNA(loan_model_data$recoveries))
stopifnot(!anyNA(loan_model_data$recovery_rate))
stopifnot(all(loan_model_data$recoveries >= 0))
stopifnot(all(loan_model_data$funded_amnt > 0))
stopifnot(all(
  loan_model_data$recovery_rate >= 0 &
    loan_model_data$recovery_rate <= 1
))

# 3. Confirm recovery-rate definition
expected_rate <- pmin(
  loan_model_data$recoveries / loan_model_data$funded_amnt,
  1
)

stopifnot(all(
  abs(loan_model_data$recovery_rate - expected_rate) < 1e-8
))

# 4. Confirm remaining imputed variables contain no missing values
stopifnot(all(
  sapply(
    imputation_variables,
    function(x) sum(is.na(loan_model_data[[x]])) == 0
  )
))

# 5. Confirm excluded variable is absent from modelling data
stopifnot(
  !"collections_12_mths_ex_med" %in% names(loan_model_data)
)

# 6. Save both prepared datasets
saveRDS(loan_clean, "data/loan_data_descriptive_prepared.rds")
saveRDS(loan_model_data, "data/loan_data_model_prepared.rds")

cat("FINAL VALIDATION PASSED\n")
cat("Descriptive dataset:", nrow(loan_clean), "rows x",
    ncol(loan_clean), "columns\n")
cat("Modelling dataset:", nrow(loan_model_data), "rows x",
    ncol(loan_model_data), "columns\n")
cat("Positive recovery rate:",
    round(mean(loan_model_data$recoveries > 0) * 100, 2), "%\n")
cat("Recovery-rate definition verified.\n")
cat("Both prepared datasets saved successfully.\n")
cat("Working directory:\n")
print(getwd())