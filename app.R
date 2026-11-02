loan_clean <- readRDS("data/loan_data_descriptive_prepared.rds")
loan_model_data <- readRDS("data/loan_data_model_prepared.rds")

stage1_dashboard <- readRDS("data/stage1_dashboard.rds")
stage2_dashboard <- readRDS("data/stage2_dashboard.rds")
lgd_dashboard <- readRDS("data/lgd_dashboard.rds")


part_d_results <- readRDS("data/part_d_results.rds")

# ============================================================
# SECTION 1: LIBRARIES AND THEME
# ============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(randomForest)

risk_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#B5121B",
  secondary = "#263746",
  bg = "#F5F7FA",
  fg = "#202B36"
)


# ============================================================
# SECTION 2: APPLICATION HEADER AND MAIN NAVIGATION
# ============================================================

ui <- page_navbar(
  title = div(
    span(
      "RISK",
      style = paste(
        "font-weight: 800;",
        "letter-spacing: 2px;",
        "color: #FFFFFF;"
      )
    ),
    span(
      " / LGD FRAMEWORK",
      style = paste(
        "font-weight: 400;",
        "font-size: 13px;",
        "letter-spacing: 1px;",
        "color: #D8DEE5;"
      )
    )
  ),
  
  id = "main_navigation",
  selected = "Part A",
  fillable = FALSE,
  collapsible = TRUE,
  inverse = TRUE,
  position = "static-top",
  theme = risk_theme,
  
  header = tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, Helvetica, sans-serif;
        background: #F5F7FA;
      }

      .navbar {
        background-color: #172B3A !important;
        border-bottom: 3px solid #B5121B;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }

      .navbar-brand {
        padding-top: 16px;
        padding-bottom: 16px;
      }

      .navbar .nav-link {
        font-size: 13px;
        font-weight: 600;
        padding: 18px 16px !important;
        color: #D8DEE5 !important;
      }

      .navbar .nav-link.active {
        color: #FFFFFF !important;
        border-bottom: 3px solid #B5121B;
      }

      .navbar .nav-link:hover {
        color: #FFFFFF !important;
        background: #263D4F;
      }

      .dashboard-heading {
        padding: 30px 34px 20px 34px;
        background: #FFFFFF;
        border-bottom: 1px solid #E2E7ED;
      }

      .dashboard-eyebrow {
        color: #B5121B;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 2px;
        text-transform: uppercase;
        margin-bottom: 8px;
      }

      .dashboard-title {
        color: #172B3A;
        font-size: 27px;
        font-weight: 700;
        margin: 0;
      }

      .dashboard-subtitle {
        color: #687786;
        font-size: 13px;
        margin-top: 8px;
      }

      .workspace {
        padding: 24px 34px;
        min-height: 65vh;
        background: #F5F7FA;
      }

      .nav-tabs {
        background: #FFFFFF;
        padding: 0 24px;
        border-bottom: 1px solid #E0E5EB;
      }

      .nav-tabs .nav-link {
        color: #647383;
        font-size: 13px;
        font-weight: 600;
        border: none;
        border-bottom: 3px solid transparent;
        border-radius: 0;
        padding: 15px 18px;
      }

      .nav-tabs .nav-link.active {
        color: #172B3A;
        background: #FFFFFF;
        border-bottom: 3px solid #B5121B;
      }

      .nav-tabs .nav-link:hover {
        color: #B5121B;
        border-color: transparent transparent #D9A0A3;
      }

      @media (max-width: 768px) {
        .dashboard-heading {
          padding: 24px 18px;
        }

        .workspace {
          padding: 18px;
        }

        .dashboard-title {
          font-size: 22px;
        }
      }
    "))
  ),
  
  
  # ==========================================================
  # SECTION 3: PART A — PORTFOLIO ANALYSIS
  # ==========================================================
  
  
  nav_panel(
    "A  Portfolio",
    value = "Part A",
    
    div(
      class = "dashboard-heading",
      div(class = "dashboard-eyebrow", "Part A / Portfolio Assessment"),
      h1(class = "dashboard-title", "Defaulted Portfolio Analysis"),
      p(
        class = "dashboard-subtitle",
        "Portfolio scale, recovery outcomes and headline risk indicators"
      )
    ),
    
    navset_tab(
      id = "part_a_navigation",
      
      # ----------------------------------------------------------
      # PAGE 1: OVERVIEW
      # ----------------------------------------------------------
      
      nav_panel(
        "Overview",
        value = "a_overview",
        
        div(
          class = "workspace",
          
          # KPI CARDS
          fluidRow(
            column(
              3,
              card(
                card_body(
                  h6("Defaulted Loans"),
                  h3(textOutput("kpi_loan_count", inline = TRUE))
                )
              )
            ),
            column(
              3,
              card(
                card_body(
                  h6("Positive Recovery Rate"),
                  h3(textOutput("kpi_positive_recovery", inline = TRUE))
                )
              )
            ),
            column(
              3,
              card(
                card_body(
                  h6("Mean Recovery Rate"),
                  h3(textOutput("kpi_mean_recovery", inline = TRUE))
                )
              )
            ),
            column(
              3,
              card(
                card_body(
                  h6("Median Recovery Rate"),
                  h3(textOutput("kpi_median_recovery", inline = TRUE))
                )
              )
            )
          ),
          
          br(),
          
          # CHARTS
          fluidRow(
            column(
              6,
              card(
                card_header("Recovery Outcomes"),
                card_body(
                  plotOutput("recovery_outcomes_plot", height = "300px")
                )
              )
            ),
            column(
              6,
              card(
                card_header("Recovery Rate Distribution"),
                card_body(
                  plotOutput("recovery_histogram", height = "300px")
                )
              )
            )
          ),
          
          br(),
          
          # DYNAMIC OBSERVATIONS
          card(
            card_header("Key Observations"),
            card_body(
              uiOutput("overview_observations")
            )
          )
        )
      ),
      
      
      nav_panel(
        "Segment Analysis",
        value = "a_segments",
        
        div(
          class = "workspace",
          
          h3("Segment Analysis"),
          p(
            "Compare recovery performance across borrower and loan characteristics."
          ),
          
          fluidRow(
            column(
              4,
              selectInput(
                "segment_variable",
                "Select segment",
                choices = c(
                  "Loan Grade" = "grade",
                  "Loan Purpose" = "purpose",
                  "Homeownership" = "home_ownership",
                  "Loan Term" = "term",
                  "Employment Length" = "emp_length"
                ),
                selected = "grade"
              )
            )
          ),
          
          br(),
          
          fluidRow(
            column(
              12,
              card(
                card_header("Mean Recovery Rate by Segment"),
                card_body(
                  plotOutput("segment_recovery_plot", height = "350px")
                )
              )
            )
          ),
          
          br(),
          
          card(
            card_header("Segment Performance Summary"),
            card_body(
              tableOutput("segment_summary_table")
            )
          ),
          
          br(),
          
          card(
            card_header("Key Findings"),
            card_body(
              uiOutput("segment_observations")
            )
          )
        )
      ),
      
      
      nav_panel(
        "Statistical Tests",
        value = "a_statistics",
        
        div(
          class = "workspace",
          
          h3("Statistical Testing Workbench"),
          p(
            "Investigate whether recovery outcomes differ across ",
            "borrower and loan segments using statistical tests."
          ),
          
          
          # 1. Variable selection
          h4("1. Select Variables"),
          
          fluidRow(
            column(
              6,
              selectInput(
                "stat_group",
                "Grouping Variable",
                choices = c(
                  "Loan Grade" = "grade",
                  "Loan Purpose" = "purpose",
                  "Homeownership" = "home_ownership",
                  "Loan Term" = "term",
                  "Employment Length" = "emp_length"
                ),
                selected = "grade",
                width = "100%"
              )
            ),
            column(
              6,
              selectInput(
                "stat_outcome",
                "Outcome Variable",
                choices = c(
                  "Recovery Rate" = "recovery_rate",
                  "Positive Recovery (Yes/No)" = "positive_recovery",
                  "Recovery Amount ($)" = "recoveries",
                  "LGD Proxy" = "lgd_proxy"
                ),
                selected = "recovery_rate",
                width = "100%"
              )
            )
          ),
          
          br(),
          
          
          # 2. Test selection
          card(
            card_header("2. Statistical Test"),
            card_body(
              radioButtons(
                "stat_mode",
                "Recommendation Method",
                choices = c(
                  "Automatic Recommendation" = "auto",
                  "Choose Test Manually" = "manual"
                ),
                selected = "auto",
                inline = TRUE
              ),
              
              conditionalPanel(
                condition = "input.stat_mode == 'manual'",
                selectInput(
                  "stat_test",
                  "Select Test",
                  choices = c(
                    "Kruskal-Wallis Test" = "kruskal",
                    "Wilcoxon Rank-Sum Test" = "wilcox",
                    "Welch's ANOVA" = "welch",
                    "Chi-Square Test of Independence" = "chisq",
                    "Fisher's Exact Test" = "fisher"
                  ),
                  selected = "kruskal"
                )
              ),
              
              selectInput(
                "stat_alpha",
                "Significance Level",
                choices = c(
                  "1% (0.01)" = "0.01",
                  "5% (0.05)" = "0.05",
                  "10% (0.10)" = "0.10"
                ),
                selected = "0.05"
              ),
              
              p(
                class = "text-muted",
                "Automatic recommendations will consider the selected ",
                "outcome and grouping variable. Manual selection is ",
                "intended for users who want to investigate a specific test."
              )
            )
          ),
          
          br(),
          
          # 3. Recommendation
          card(
            card_header("3. Test Recommendation"),
            card_body(
              uiOutput("stat_recommendation")
            )
          ),
          
          br(),
          
          # 4. Results
          card(
            card_header("4. Statistical Results"),
            card_body(
              uiOutput("stat_test_results")
            )
          ),
          
          br(),
          
          # 5. Group summaries
          card(
            card_header("5. Group-Level Summary"),
            card_body(
              tableOutput("stat_group_summary")
            )
          ),
          
          br(),
          
          # 6. Interpretation
          card(
            card_header("6. Interpretation and Limitations"),
            card_body(
              uiOutput("stat_interpretation")
            )
          )
        )
      ),
      
      nav_panel(
        "Relationships & Quality",
        value = "a_quality",
        
        div(
          class = "workspace",
          
          h3("Relationships & Data Quality"),
          p("Explore relationships between numeric loan and borrower characteristics using correlation analysis."),
          
          h4("1. Correlation Analysis"),
          
          fluidRow(
            column(
              12,
              checkboxGroupInput(
                "corr_variables",
                "Select variables for the heatmap",
                choices = c(
                  "Loan Amount" = "loan_amnt",
                  "Funded Amount" = "funded_amnt",
                  "Interest Rate" = "int_rate",
                  "Installment" = "installment",
                  "Annual Income" = "annual_inc",
                  "Debt-to-Income Ratio" = "dti",
                  "Delinquencies (Last 2 Years)" = "delinq_2yrs",
                  "Open Accounts" = "open_acc",
                  "Revolving Balance" = "revol_bal",
                  "Revolving Utilisation" = "revol_util",
                  "Recovery Amount" = "recoveries",
                  "Recovery Rate" = "recovery_rate"
                ),
                selected = c(
                  "loan_amnt",
                  "int_rate",
                  "annual_inc",
                  "dti",
                  "revol_util",
                  "recovery_rate"
                ),
                inline = TRUE
              )
            )
          ),
          
          br(),
          
          card(
            card_header("Correlation Heatmap"),
            card_body(
              plotOutput("correlation_heatmap", height = "500px")
            )
          ),
          
          br(),
          
          card(
            card_header("Highly Correlated Variable Pairs"),
            card_body(
              p("Variable pairs with an absolute Pearson correlation of 0.70 or higher."),
              tableOutput("high_correlation_table")
            )
          ),
          
          br(),
          
          card(
            card_header("Interpretation"),
            card_body(
              uiOutput("correlation_observations")
            )
          ),
          
          br(),
          
          card(
            card_header("Data Quality Summary"),
            card_body(
              tableOutput("data_quality_table")
            )
          )
        )
      ),
    ),
    
    div(class = "workspace")
  ),
  
  
  # ==========================================================
  # SECTION 4: PART B — RECOVERY DRIVERS
  # ==========================================================
  
  nav_panel(
    "B  Drivers",
    value = "Part B",
    
    div(
      class = "dashboard-heading",
      div(class = "dashboard-eyebrow", "Part B / Risk Drivers"),
      h1(class = "dashboard-title", "Recovery Drivers"),
      p(
        class = "dashboard-subtitle",
        "Borrower characteristics and internal grade differentiation"
      )
    ),
    
    navset_tab(
      id = "part_b_navigation",
      nav_panel("Recovery Drivers", value = "b_drivers"),
      
      nav_panel(
        "Grade Validation",
        value = "b_grades",
        
        h3("Grade Validation"),
        p(
          "Evaluate whether the institution's internal grades ",
          "differentiate recovery outcomes among defaulted borrowers."
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header("Positive Recovery Rate by Grade"),
              card_body(
                plotOutput("grade_positive_plot", height = "350px")
              )
            )
          ),
          column(
            6,
            card(
              card_header("Recovery Rate by Grade"),
              card_body(
                plotOutput("grade_severity_plot", height = "350px")
              )
            )
          )
        ),
        
        br(),
        
        card(
          card_header("Grade-Level Performance Summary"),
          card_body(
            tableOutput("grade_validation_table")
          )
        ),
        
        br(),
        
        card(
          card_header("Statistical Validation"),
          card_body(
            tableOutput("grade_validation_tests")
          )
        ),
        
        br(),
        
        card(
          card_header("Interpretation"),
          card_body(
            uiOutput("grade_validation_observations")
          )
        )
      )
      
    ),
    
    
    div(
      class = "workspace",
      
      conditionalPanel(
        condition = "input.part_b_navigation == 'b_drivers'",
        
        h3("Recovery Drivers"),
        p(
          "Examine borrower and loan characteristics associated with ",
          "the likelihood of obtaining recoveries after default."
        ),
        
        fluidRow(
          column(
            4,
            selectInput(
              "driver_variable",
              "Select characteristic",
              choices = c(
                "Loan Grade" = "grade",
                "Loan Purpose" = "purpose",
                "Homeownership" = "home_ownership",
                "Loan Term" = "term",
                "Employment Length" = "emp_length",
                "Loan Amount" = "loan_amnt",
                "Interest Rate" = "int_rate",
                "Annual Income" = "annual_inc",
                "Debt-to-Income Ratio" = "dti",
                "Revolving Utilisation" = "revol_util",
                "Recent Delinquencies" = "delinq_2yrs"
              ),
              selected = "grade",
              width = "100%"
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            12,
            card(
              card_header("Positive Recovery Rate by Characteristic"),
              card_body(
                plotOutput("driver_recovery_plot", height = "380px")
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            12,
            card(
              card_header("Recovery Driver Summary"),
              card_body(
                tableOutput("driver_summary_table")
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            12,
            card(
              card_header("Credit Risk Interpretation"),
              card_body(
                uiOutput("driver_observations")
              )
            )
          )
        )
      ),
      
      
    )
    
  ),
  
  
  # ==========================================================
  # SECTION 5: PART C — LGD MODELLING
  # ==========================================================
  
  nav_panel(
    "C  Models",
    value = "Part C",
    
    div(
      class = "dashboard-heading",
      div(class = "dashboard-eyebrow", "Part C / Model Development"),
      h1(class = "dashboard-title", "Recovery & LGD Models"),
      p(
        class = "dashboard-subtitle",
        "Two-stage recovery modelling, LGD estimation, and validation"
      )
    ),
    
    
    navset_tab(
      id = "part_c_navigation",
      
      nav_panel(
        "Recovery Probability",
        value = "c_probability",
        
        
        h3("Positive Recovery Probability"),
        p("Performance of the model predicting whether a defaulted loan receives a positive recovery."),
        
        div(class = "metric-grid",
            uiOutput("c_prob_metrics")
        ),
        
        h4("Model Comparison"),
        tableOutput("c_prob_comparison"),
        
        h4("Calibration"),
        plotOutput("c_prob_calibration", height = "350px")
      ),
      
      nav_panel(
        "Conditional Recovery",
        value = "c_recovery",
        
        h3("Conditional Recovery Rate"),
        p("Model performance among loans with positive recoveries."),
        
        div(class = "metric-grid",
            uiOutput("c_recovery_metrics")
        ),
        
        
        h4("Model Comparison"),
        tableOutput("c_recovery_comparison"),
        h4("Variable Importance"),
        plotOutput("c_recovery_importance", height = "400px")
        
      ),
      
      nav_panel(
        "LGD Estimates",
        value = "c_lgd",
        
        h3("Two-Stage LGD Estimation"),
        p("Estimated LGD combines the probability of positive recovery with the conditional recovery rate."),
        
        div(class = "metric-grid",
            uiOutput("c_lgd_metrics")
        ),
        
        h4("Predicted vs Observed LGD"),
        plotOutput("c_lgd_calibration", height = "350px"),
        
        h4("LGD Distribution"),
        plotOutput("c_lgd_distribution", height = "350px")
      ),
      
      nav_panel(
        "Model Performance",
        value = "c_performance",
        
        h3("Overall Model Validation"),
        
        h4("LGD Model vs Benchmark"),
        tableOutput("c_lgd_benchmark"),
        
        h4("LGD Distribution Diagnostics"),
        tableOutput("c_lgd_diagnostics"),
        
        h4("Interpretation and Limitations"),
        uiOutput("c_model_interpretation")
      ),
      
      nav_panel(
        "LGD Simulator",
        value = "c_simulator",
        
        div(
          class = "workspace",
          
          h3("Loan-Level LGD Simulator"),
          p(
            "Enter the characteristics of a defaulted loan to estimate ",
            "its probability of positive recovery, conditional recovery ",
            "rate, and expected loss given default."
          ),
          
          p(
            class = "text-muted",
            "This simulator uses the saved Stage 1 and Stage 2 models. ",
            "It does not estimate probability of default."
          ),
          
          hr(),
          h4("Loan Characteristics"),
          
          fluidRow(
            column(4, numericInput("sim_funded_amnt", "Funded Amount", 10000, min = 0)),
            column(4, selectInput("sim_term", "Loan Term",
                                  choices = c("36 months" = " 36 months",
                                              "60 months" = " 60 months"),
                                  selected = " 36 months")),
            column(4, numericInput("sim_int_rate", "Interest Rate (%)", 12, min = 0, max = 100, step = 0.1))
          ),
          
          fluidRow(
            column(4, selectInput("sim_grade", "Loan Grade",
                                  choices = c("A", "B", "C", "D", "E", "F", "G"),
                                  selected = "C")),
            column(4, selectInput("sim_purpose", "Loan Purpose",
                                  choices = c("car", "credit_card", "debt_consolidation",
                                              "educational", "home_improvement", "house",
                                              "major_purchase", "medical", "moving", "other",
                                              "renewable_energy", "small_business", "vacation",
                                              "wedding"),
                                  selected = "debt_consolidation")),
            column(4, selectInput("sim_home_ownership", "Home Ownership",
                                  choices = c("MORTGAGE", "NONE", "OTHER", "OWN", "RENT"),
                                  selected = "MORTGAGE"))
          ),
          
          fluidRow(
            column(4, selectInput("sim_verification_status", "Verification Status",
                                  choices = c("Not Verified", "Source Verified", "Verified"),
                                  selected = "Not Verified")),
            column(4, selectInput("sim_emp_length", "Employment Length",
                                  choices = c("< 1 year", "1 year", "2 years", "3 years",
                                              "4 years", "5 years", "6 years", "7 years",
                                              "8 years", "9 years", "10+ years", "Unknown"),
                                  selected = "10+ years")),
            column(4, numericInput("sim_annual_inc", "Annual Income", 60000, min = 0))
          ),
          
          fluidRow(
            column(4, numericInput("sim_dti", "Debt-to-Income Ratio", 15, min = 0)),
            column(4, numericInput("sim_delinq", "Delinquencies (2 Years)", 0, min = 0)),
            column(4, numericInput("sim_inq", "Recent Credit Inquiries", 1, min = 0))
          ),
          
          fluidRow(
            column(4, numericInput("sim_open_acc", "Open Credit Accounts", 8, min = 0)),
            column(4, numericInput("sim_pub_rec", "Public Records", 0, min = 0)),
            column(4, numericInput("sim_revol_bal", "Revolving Balance", 5000, min = 0))
          ),
          
          fluidRow(
            column(4, numericInput("sim_revol_util", "Revolving Utilisation (%)", 40, min = 0, max = 100)),
            column(4, numericInput("sim_total_acc", "Total Credit Accounts", 15, min = 0)),
            column(4, numericInput("sim_credit_history", "Credit History (Months)", 180, min = 0))
          ),
          
          fluidRow(
            column(4, numericInput("sim_acc_now_delinq", "Currently Delinquent Accounts", 0, min = 0))
          ),
          
          actionButton("sim_run", "Estimate LGD", class = "btn-primary"),
          
          br(),
          br(),
          
          h4("Simulation Results"),
          
          uiOutput("sim_lgd_highlight"),
          
          fluidRow(
            column(
              4,
              wellPanel(
                h5("Positive Recovery Probability"),
                uiOutput("sim_probability_card")
              )
            ),
            column(
              4,
              wellPanel(
                h5("Conditional Recovery Rate"),
                uiOutput("sim_conditional_card")
              )
            ),
            column(
              4,
              wellPanel(
                h5("Expected Recovery Rate"),
                uiOutput("sim_recovery_card")
              )
            )
          ),
          div(style = "margin-top: 24px;"),
          tableOutput("sim_results"),
          
          p(
            class = "text-muted",
            "These are model estimates for a loan that has already defaulted. ",
            "They are not probability-of-default estimates or guarantees ",
            "of actual recovery."
          )
          
        )
      )
      
    ),
    
    
    div(class = "workspace")
  ),
  
  
  # ==========================================================
  # SECTION 6: PART D — ROBUSTNESS AND MODEL RISK
  # ==========================================================
  
  nav_panel(
    "D  Validation",
    value = "Part D",
    
    div(
      class = "dashboard-heading",
      div(class = "dashboard-eyebrow", "Part D / Model Validation"),
      h1(class = "dashboard-title", "Robustness & Model Risk"),
      p(
        class = "dashboard-subtitle",
        "Sensitivity analysis, reliability, limitations, and assumptions"
      )
    ),
    
    navset_tab(
      id = "part_d_navigation",
      nav_panel(
        "Robustness",
        value = "d_robustness",
        uiOutput("d_robustness_content")
      ),
      
      nav_panel(
        "Portfolio Sensitivity",
        value = "d_sensitivity",
        uiOutput("d_sensitivity_content")
      ),
      
      nav_panel(
        "Model Risk",
        value = "d_risk",
        uiOutput("d_risk_content")
      )
    ),
    
    div(class = "workspace")
  ),
  
  
  # ==========================================================
  # SECTION 7: PART E — INTERACTIVE DASHBOARD
  # ==========================================================
  
  nav_panel(
    "E  Documentation",
    value = "Part E",
    
    div(
      class = "dashboard-heading",
      div(class = "dashboard-eyebrow", "Part E / Navigation Support"),
      h1(class = "dashboard-title", "Design Guide & Reports"),
      p(
        class = "dashboard-subtitle",
        "Comprehensive documentation"
      )
    ),
    
    
    navset_tab(
      id = "part_e_navigation",
      
      nav_panel(
        "Read First",
        value = "read_first",
        tags$iframe(
          src = "read_first.pdf",
          width = "100%",
          height = "750px",
          style = "border: none;"
        )
      ),
      
      nav_panel(
        "Business Report",
        value = "business_report",
        tags$iframe(
          src = "business_report.pdf",
          width = "100%",
          height = "750px",
          style = "border: none;"
        )
      ),
      
      nav_panel(
        "Detailed Report",
        value = "detailed_report",
        tags$iframe(
          src = "detailed_report.pdf",
          width = "100%",
          height = "750px",
          style = "border: none;"
        )
      )
    ),
    
    
    div(class = "workspace")
  )
)


# ============================================================
# SECTION 8: SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # DATA SOURCE
  # ----------------------------------------------------------
  
  overview_data <- reactive({
    req(exists("loan_clean", envir = .GlobalEnv))
    
    data <- get("loan_clean", envir = .GlobalEnv)
    
    req(
      all(c("recoveries", "recovery_rate") %in% names(data)),
      nrow(data) > 0
    )
    
    data
  })
  
  
  # Statistical Tests: prepare the selected outcome and grouping variable
  stat_data <- reactive({
    req(input$stat_group, input$stat_outcome)
    
    data <- loan_clean
    
    data$Segment <- as.character(data[[input$stat_group]])
    
    data$Segment[
      is.na(data$Segment) |
        trimws(data$Segment) == ""
    ] <- "Missing / Unknown"
    
    if (input$stat_outcome == "positive_recovery") {
      data$Outcome <- as.integer(data$recoveries > 0)
    } else if (input$stat_outcome == "lgd_proxy") {
      data$Outcome <- 1 - data$recovery_rate
    } else {
      data$Outcome <- data[[input$stat_outcome]]
    }
    
    data |>
      dplyr::filter(
        !is.na(Segment),
        !is.na(Outcome),
        is.finite(Outcome)
      )
  })
  
  # Recommend a test based on outcome type and number of groups
  stat_recommended_test <- reactive({
    data <- stat_data()
    
    group_count <- dplyr::n_distinct(data$Segment)
    
    if (input$stat_outcome == "positive_recovery") {
      return(list(
        test = "chisq",
        name = "Chi-square test of independence",
        reason = paste(
          "The outcome indicates whether a loan had any recovery.",
          "The test evaluates the association between the selected",
          "segment and positive recovery status."
        ),
        caveat = paste(
          "Expected cell counts must be checked.",
          "Fisher's exact test may be preferable for sparse tables."
        )
      ))
    }
    
    if (group_count < 2) {
      return(list(
        test = NA_character_,
        name = "Insufficient groups",
        reason = "At least two groups with usable observations are required.",
        caveat = "Select another grouping variable."
      ))
    }
    
    if (group_count == 2) {
      return(list(
        test = "wilcox",
        name = "Wilcoxon rank-sum test",
        reason = paste(
          "The selected numeric outcome is compared across",
          "two independent groups without requiring normality."
        ),
        caveat = paste(
          "The test compares rank distributions; it does not",
          "automatically test equality of medians."
        )
      ))
    }
    
    list(
      test = "kruskal",
      name = "Kruskal-Wallis test",
      reason = paste(
        "The selected numeric outcome is compared across",
        "three or more independent groups. This is a useful",
        "initial choice for skewed recovery data."
      ),
      caveat = paste(
        "A significant result indicates that at least one group",
        "differs in rank distribution. Follow-up comparisons",
        "may be needed."
      )
    )
  })
  
  # Display the recommendation or manual test selection
  stat_selected_test <- reactive({
    if (input$stat_mode == "manual") {
      manual_names <- c(
        kruskal = "Kruskal-Wallis test",
        wilcox = "Wilcoxon rank-sum test",
        welch = "Welch's ANOVA",
        chisq = "Chi-square test of independence",
        fisher = "Fisher's exact test"
      )
      
      return(list(
        test = input$stat_test,
        name = unname(manual_names[input$stat_test]),
        reason = "The test was selected manually.",
        caveat = paste(
          "Verify that the selected test is appropriate",
          "for the outcome type and its assumptions."
        )
      ))
    }
    
    stat_recommended_test()
  })
  
  output$stat_recommendation <- renderUI({
    recommendation <- stat_recommended_test()
    
    tagList(
      h4(recommendation$name),
      p(recommendation$reason),
      p(
        strong("Important considerations: "),
        recommendation$caveat
      )
    )
  })
  
  
  # Calculate statistical test results
  stat_test_result <- reactive({
    data <- stat_data()
    selected <- stat_selected_test()
    test_name <- selected$test
    
    alpha <- as.numeric(input$stat_alpha)
    
    validate(
      need(
        dplyr::n_distinct(data$Segment) >= 2,
        "At least two groups with usable observations are required."
      )
    )
    
    is_binary <- input$stat_outcome == "positive_recovery"
    
    # Reject incompatible tests
    if (is_binary && !test_name %in% c("chisq", "fisher")) {
      return(list(
        valid = FALSE,
        message = paste(
          "For a positive-recovery outcome, select the",
          "Chi-square or Fisher's exact test."
        )
      ))
    }
    
    if (!is_binary && test_name %in% c("chisq", "fisher")) {
      return(list(
        valid = FALSE,
        message = paste(
          "Chi-square and Fisher's exact tests require a",
          "categorical outcome. Select a numeric outcome instead."
        )
      ))
    }
    
    # Remove unused segment levels and create the test result
    data$Segment <- droplevels(factor(data$Segment))
    
    result <- tryCatch({
      
      if (test_name %in% c("chisq", "fisher")) {
        contingency_table <- table(
          data$Segment,
          factor(data$Outcome, levels = c(0, 1))
        )
        
        if (nrow(contingency_table) < 2 ||
            ncol(contingency_table) < 2) {
          stop("The contingency table needs at least two groups and two outcome categories.")
        }
        
        if (test_name == "fisher") {
          test <- fisher.test(contingency_table)
          method <- "Fisher's exact test"
        } else {
          test <- suppressWarnings(
            chisq.test(contingency_table, correct = FALSE)
          )
          
          if (any(test$expected < 5)) {
            stop(
              paste(
                "Some expected cell counts are below 5.",
                "Use Fisher's exact test or combine sparse categories",
                "where substantively justified."
              )
            )
          }
          
          method <- "Chi-square test of independence"
        }
        
        statistic <- unname(test$statistic)
        p_value <- test$p.value
        df <- if (is.null(test$parameter)) NA_real_ else
          unname(test$parameter)
        
      } else {
        
        group_sizes <- table(data$Segment)
        
        if (any(group_sizes < 2)) {
          stop(
            "At least one group has fewer than two observations. Review the selected grouping variable."
          )
        }
        
        if (test_name == "welch") {
          if (length(group_sizes) < 2) {
            stop("Welch's ANOVA requires at least two groups.")
          }
          
          test <- oneway.test(
            Outcome ~ Segment,
            data = data,
            var.equal = FALSE
          )
          
          method <- "Welch's ANOVA"
          statistic <- unname(test$statistic)
          p_value <- test$p.value
          df <- NA_real_
          
        } else if (test_name == "wilcox") {
          
          if (length(group_sizes) != 2) {
            stop(
              "The Wilcoxon rank-sum test requires exactly two groups."
            )
          }
          
          test <- wilcox.test(
            Outcome ~ Segment,
            data = data,
            exact = FALSE
          )
          
          method <- "Wilcoxon rank-sum test"
          statistic <- unname(test$statistic)
          p_value <- test$p.value
          df <- NA_real_
          
        } else if (test_name == "kruskal") {
          
          test <- kruskal.test(
            Outcome ~ Segment,
            data = data
          )
          
          method <- "Kruskal-Wallis test"
          statistic <- unname(test$statistic)
          p_value <- test$p.value
          df <- unname(test$parameter)
          
        } else {
          stop("Unsupported statistical test selected.")
        }
      }
      
      list(
        valid = TRUE,
        method = method,
        statistic = statistic,
        df = df,
        p_value = p_value,
        alpha = alpha,
        decision = if (p_value < alpha) {
          "Reject the null hypothesis"
        } else {
          "Fail to reject the null hypothesis"
        }
      )
      
    }, error = function(e) {
      list(
        valid = FALSE,
        message = conditionMessage(e)
      )
    })
    
    result
  })
  
  # Display statistical test results
  output$stat_test_results <- renderUI({
    result <- stat_test_result()
    
    if (!result$valid) {
      return(
        div(
          class = "alert alert-warning",
          strong("Test could not be completed"),
          p(result$message)
        )
      )
    }
    
    tagList(
      h4(result$method),
      
      fluidRow(
        column(
          4,
          h5("Test statistic"),
          strong(round(result$statistic, 4))
        ),
        column(
          4,
          h5("P-value"),
          strong(format.pval(result$p_value, digits = 4))
        ),
        column(
          4,
          h5("Significance level"),
          strong(result$alpha)
        )
      ),
      
      if (!is.na(result$df)) {
        p(strong("Degrees of freedom: "), result$df)
      },
      
      hr(),
      
      p(
        strong("Decision: "),
        result$decision
      ),
      
      p(
        if (result$p_value < result$alpha) {
          paste(
            "The result is statistically significant at the",
            "selected significance level. There is evidence",
            "of a difference or association between groups."
          )
        } else {
          paste(
            "The result is not statistically significant at",
            "the selected significance level. The analysis",
            "does not provide sufficient evidence of a",
            "difference or association between groups."
          )
        }
      )
    )
  })
  
  # Summarise the selected outcome by group
  output$stat_group_summary <- renderTable({
    data <- stat_data()
    
    data |>
      dplyr::group_by(Segment) |>
      dplyr::summarise(
        Loan_Count = dplyr::n(),
        Mean_Outcome = mean(Outcome, na.rm = TRUE),
        Median_Outcome = median(Outcome, na.rm = TRUE),
        SD_Outcome = sd(Outcome, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Mean_Outcome = round(Mean_Outcome, 4),
        Median_Outcome = round(Median_Outcome, 4),
        SD_Outcome = round(SD_Outcome, 4)
      ) |>
      dplyr::arrange(dplyr::desc(Mean_Outcome))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # Explain the result and its limitations
  output$stat_interpretation <- renderUI({
    result <- stat_test_result()
    
    if (!result$valid) {
      return(p("Interpretation is unavailable until the test can be completed."))
    }
    
    tagList(
      p(
        strong("Research question: "),
        if (input$stat_outcome == "positive_recovery") {
          "Is positive recovery associated with the selected segment?"
        } else {
          "Does the selected numeric outcome differ across the selected segments?"
        }
      ),
      
      p(
        strong("Null hypothesis (H0): "),
        if (input$stat_outcome == "positive_recovery") {
          "Positive recovery status is independent of the selected segment."
        } else {
          "The selected outcome does not differ across groups under the assumptions of the chosen test."
        }
      ),
      
      p(
        strong("Alternative hypothesis (H1): "),
        if (input$stat_outcome == "positive_recovery") {
          "Positive recovery status is associated with the selected segment."
        } else {
          "At least one group differs in the outcome distribution or mean, depending on the test."
        }
      ),
      
      p(
        strong("Limitation: "),
        "Statistical significance does not establish causation or practical importance. These are unadjusted group comparisons and do not control for other borrower or loan characteristics."
      )
    )
  })
  
  # Relationships & Quality: correlation analysis
  
  correlation_data <- reactive({
    req(input$corr_variables)
    
    data <- overview_data()
    
    selected_data <- data[, input$corr_variables, drop = FALSE]
    
    # Retain only numeric variables
    numeric_vars <- names(selected_data)[
      vapply(selected_data, is.numeric, logical(1))
    ]
    
    validate(
      need(
        length(numeric_vars) >= 2,
        "Select at least two numeric variables to calculate correlations."
      )
    )
    
    selected_data <- selected_data[, numeric_vars, drop = FALSE]
    
    # Remove variables with no variation
    variable_sd <- vapply(
      selected_data,
      function(x) stats::sd(x, na.rm = TRUE),
      numeric(1)
    )
    
    valid_vars <- names(variable_sd)[
      is.finite(variable_sd) & variable_sd > 0
    ]
    
    validate(
      need(
        length(valid_vars) >= 2,
        "At least two selected variables must have non-zero variation."
      )
    )
    
    selected_data <- selected_data[, valid_vars, drop = FALSE]
    
    # Calculate pairwise Pearson correlations
    correlation_matrix <- stats::cor(
      selected_data,
      use = "pairwise.complete.obs",
      method = "pearson"
    )
    
    correlation_matrix
  })
  
  
  output$correlation_heatmap <- renderPlot({
    correlation_matrix <- correlation_data()
    
    correlation_long <- as.data.frame(
      as.table(correlation_matrix)
    )
    
    names(correlation_long) <- c("Variable1", "Variable2", "Correlation")
    
    ggplot(
      correlation_long,
      aes(
        x = Variable1,
        y = Variable2,
        fill = Correlation
      )
    ) +
      geom_tile(color = "white", linewidth = 0.4) +
      geom_text(
        aes(label = sprintf("%.2f", Correlation)),
        size = 3.5
      ) +
      scale_fill_gradient2(
        low = "#B5121B",
        mid = "white",
        high = "#263746",
        midpoint = 0,
        limits = c(-1, 1),
        name = "Correlation"
      ) +
      coord_equal() +
      labs(
        x = NULL,
        y = NULL,
        title = "Pearson Correlation Matrix"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
  
  
  output$correlation_observations <- renderUI({
    correlation_matrix <- correlation_data()
    
    upper_matrix <- correlation_matrix
    upper_matrix[lower.tri(upper_matrix, diag = TRUE)] <- NA
    
    correlation_pairs <- as.data.frame(
      as.table(upper_matrix)
    )
    
    names(correlation_pairs) <- c(
      "Variable1",
      "Variable2",
      "Correlation"
    )
    
    correlation_pairs <- correlation_pairs[
      is.finite(correlation_pairs$Correlation),
      ,
      drop = FALSE
    ]
    
    if (nrow(correlation_pairs) == 0) {
      return(
        p("No variable pairs are available for interpretation.")
      )
    }
    
    correlation_pairs$AbsCorrelation <- abs(
      correlation_pairs$Correlation
    )
    
    strongest_pair <- correlation_pairs[
      which.max(correlation_pairs$AbsCorrelation),
      ,
      drop = FALSE
    ]
    
    p(
      paste0(
        "The strongest observed correlation among the selected variables is between ",
        strongest_pair$Variable1,
        " and ",
        strongest_pair$Variable2,
        ", with a Pearson correlation of ",
        sprintf("%.3f", strongest_pair$Correlation),
        "."
      )
    )
  })
  
  output$high_correlation_table <- renderTable({
    correlation_matrix <- correlation_data()
    
    correlation_pairs <- as.data.frame(
      as.table(correlation_matrix)
    )
    
    names(correlation_pairs) <- c(
      "Variable 1",
      "Variable 2",
      "Correlation"
    )
    
    # Keep each pair once and exclude self-correlations
    correlation_pairs <- correlation_pairs[
      as.character(correlation_pairs[["Variable 1"]]) <
        as.character(correlation_pairs[["Variable 2"]]),
      ,
      drop = FALSE
    ]
    
    # Retain strong positive or negative correlations
    correlation_pairs <- correlation_pairs[
      abs(correlation_pairs$Correlation) >= 0.70,
      ,
      drop = FALSE
    ]
    
    if (nrow(correlation_pairs) == 0) {
      return(data.frame(
        Message = "No variable pairs have an absolute correlation of 0.70 or higher."
      ))
    }
    
    correlation_pairs <- correlation_pairs[
      order(-abs(correlation_pairs$Correlation)),
      ,
      drop = FALSE
    ]
    
    correlation_pairs$Correlation <- round(
      correlation_pairs$Correlation,
      3
    )
    
    correlation_pairs
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Data Quality Summary
  
  output$data_quality_table <- renderTable({
    data <- overview_data()
    
    missing_count <- sum(is.na(data))
    total_cells <- nrow(data) * ncol(data)
    
    # Count fully duplicated rows
    duplicate_count <- sum(duplicated(data))
    
    # Check selected fields for unusual values
    negative_recoveries <- if ("recoveries" %in% names(data)) {
      sum(data$recoveries < 0, na.rm = TRUE)
    } else {
      NA_integer_
    }
    
    invalid_recovery_rates <- if ("recovery_rate" %in% names(data)) {
      sum(
        data$recovery_rate < 0 | data$recovery_rate > 1,
        na.rm = TRUE
      )
    } else {
      NA_integer_
    }
    
    data.frame(
      Check = c(
        "Total records",
        "Total variables",
        "Missing cells",
        "Missing cells (%)",
        "Duplicate rows",
        "Negative recovery amounts",
        "Recovery rates outside [0, 1]"
      ),
      Result = c(
        format(nrow(data), big.mark = ","),
        ncol(data),
        format(missing_count, big.mark = ","),
        sprintf("%.2f%%", 100 * missing_count / total_cells),
        format(duplicate_count, big.mark = ","),
        format(negative_recoveries, big.mark = ","),
        format(invalid_recovery_rates, big.mark = ",")
      )
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  #---------
  # PART B SERVER BEGINS
  
  
  # ==========================================================
  # PART B: RECOVERY DRIVERS
  # ==========================================================
  
  driver_data <- reactive({
    data <- overview_data()
    
    variable <- input$driver_variable
    req(variable, variable %in% names(data))
    
    data$Segment <- as.character(data[[variable]])
    data$Segment[
      is.na(data$Segment) | trimws(data$Segment) == ""
    ] <- "Missing / Unknown"
    
    data$PositiveRecovery <- data$recoveries > 0
    
    data$RecoveryRate <- data$recovery_rate
    
    data$Segment <- factor(data$Segment)
    data
  })
  
  
  driver_summary <- reactive({
    data <- driver_data()
    
    result <- lapply(
      split(data, data$Segment, drop = TRUE),
      function(group) {
        valid_rates <- group$RecoveryRate[
          is.finite(group$RecoveryRate)
        ]
        
        data.frame(
          Segment = as.character(group$Segment[1]),
          LoanCount = nrow(group),
          PositiveRecoveryPct = 100 * mean(
            group$PositiveRecovery, na.rm = TRUE
          ),
          MeanRecoveryRate = if (length(valid_rates) > 0) {
            100 * mean(valid_rates)
          } else {
            NA_real_
          },
          MedianRecoveryRate = if (length(valid_rates) > 0) {
            100 * median(valid_rates)
          } else {
            NA_real_
          }
        )
      }
    )
    
    result <- do.call(rbind, result)
    rownames(result) <- NULL
    
    result[order(-result$PositiveRecoveryPct), ]
  })
  
  
  output$driver_recovery_plot <- renderPlot({
    summary_data <- driver_summary()
    
    ggplot(
      summary_data,
      aes(
        x = reorder(Segment, PositiveRecoveryPct),
        y = PositiveRecoveryPct
      )
    ) +
      geom_col(fill = "#263746", width = 0.7) +
      geom_text(
        aes(label = sprintf("%.1f%%", PositiveRecoveryPct)),
        hjust = -0.15,
        size = 3.5
      ) +
      coord_flip() +
      scale_y_continuous(
        limits = c(
          0,
          max(summary_data$PositiveRecoveryPct, na.rm = TRUE) * 1.15
        ),
        labels = function(x) paste0(x, "%")
      ) +
      labs(
        x = NULL,
        y = "Loans with Positive Recovery",
        title = "Positive Recovery Incidence by Segment"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
  
  
  output$driver_summary_table <- renderTable({
    summary_data <- driver_summary()
    
    summary_data$PositiveRecoveryPct <- round(
      summary_data$PositiveRecoveryPct, 2
    )
    summary_data$MeanRecoveryRate <- round(
      summary_data$MeanRecoveryRate, 2
    )
    summary_data$MedianRecoveryRate <- round(
      summary_data$MedianRecoveryRate, 2
    )
    
    names(summary_data) <- c(
      "Segment",
      "Loan Count",
      "Positive Recovery (%)",
      "Mean Recovery Rate (%)",
      "Median Recovery Rate (%)"
    )
    
    summary_data
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 2)
  
  
  output$driver_observations <- renderUI({
    summary_data <- driver_summary()
    
    validate(
      need(nrow(summary_data) >= 2,
           "At least two segments are required for comparison.")
    )
    
    highest <- summary_data[
      which.max(summary_data$PositiveRecoveryPct), ,
      drop = FALSE
    ]
    
    lowest <- summary_data[
      which.min(summary_data$PositiveRecoveryPct), ,
      drop = FALSE
    ]
    
    tagList(
      p(
        strong("Highest positive-recovery incidence: "),
        highest$Segment,
        " (",
        sprintf("%.2f%%", highest$PositiveRecoveryPct),
        "; n = ",
        format(highest$LoanCount, big.mark = ","),
        ")."
      ),
      p(
        strong("Lowest positive-recovery incidence: "),
        lowest$Segment,
        " (",
        sprintf("%.2f%%", lowest$PositiveRecoveryPct),
        "; n = ",
        format(lowest$LoanCount, big.mark = ","),
        ")."
      ),
      p(
        "These are descriptive associations within a portfolio of defaulted ",
        "loans. They do not establish causality or indicate the probability ",
        "of default among all originated loans. Differences in recovery ",
        "incidence may reflect collateral, borrower resources, collection ",
        "processes, loan characteristics, or differences between segments."
      )
    )
  })
  
  
  # ==========================================================
  # PART B: GRADE VALIDATION
  # ==========================================================
  
  grade_validation_data <- reactive({
    data <- overview_data()
    
    data$Grade <- factor(
      as.character(data$grade),
      levels = c("A", "B", "C", "D", "E", "F", "G"),
      ordered = TRUE
    )
    
    data$PositiveRecovery <- data$recoveries > 0
    data$RecoveryRate <- data$recovery_rate
    
    data
  })
  
  
  grade_validation_summary <- reactive({
    data <- grade_validation_data()
    data <- data[!is.na(data$Grade), , drop = FALSE]
    
    result <- lapply(
      split(data, data$Grade, drop = TRUE),
      function(group) {
        valid_rates <- group$RecoveryRate[
          is.finite(group$RecoveryRate)
        ]
        
        data.frame(
          Grade = as.character(group$Grade[1]),
          LoanCount = nrow(group),
          PositiveRecoveryPct = 100 * mean(
            group$PositiveRecovery, na.rm = TRUE
          ),
          MeanRecoveryRate = if (length(valid_rates) > 0) {
            100 * mean(valid_rates)
          } else {
            NA_real_
          },
          MedianRecoveryRate = if (length(valid_rates) > 0) {
            100 * median(valid_rates)
          } else {
            NA_real_
          },
          MeanLGD = if (length(valid_rates) > 0) {
            100 * mean(1 - valid_rates)
          } else {
            NA_real_
          }
        )
      }
    )
    
    result <- do.call(rbind, result)
    rownames(result) <- NULL
    result
  })
  
  
  output$grade_positive_plot <- renderPlot({
    summary_data <- grade_validation_summary()
    
    ggplot(
      summary_data,
      aes(x = Grade, y = PositiveRecoveryPct, group = 1)
    ) +
      geom_line(color = "#263746", linewidth = 0.8) +
      geom_point(color = "#B5121B", size = 3) +
      geom_text(
        aes(label = sprintf("%.1f%%", PositiveRecoveryPct)),
        vjust = -0.8,
        size = 3.5
      ) +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0.02, 0.15))
      ) +
      labs(
        x = "Internal Grade",
        y = "Loans with Positive Recovery",
        title = "Positive Recovery Incidence Across Grades"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
  
  
  output$grade_severity_plot <- renderPlot({
    summary_data <- grade_validation_summary()
    
    ggplot(
      summary_data,
      aes(x = Grade, y = MeanRecoveryRate)
    ) +
      geom_col(fill = "#263746", width = 0.65) +
      geom_point(
        aes(y = MedianRecoveryRate),
        color = "#B5121B",
        size = 3
      ) +
      geom_text(
        aes(label = sprintf("%.2f%%", MeanRecoveryRate)),
        vjust = -0.6,
        size = 3.2
      ) +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.18))
      ) +
      labs(
        x = "Internal Grade",
        y = "Recovery Rate",
        title = "Mean Recovery Rate (Bars) and Median (Red Points)",
        caption = "Recovery rates are capped at 100% of funded amount."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
  
  
  output$grade_validation_table <- renderTable({
    summary_data <- grade_validation_summary()
    
    summary_data$PositiveRecoveryPct <- round(
      summary_data$PositiveRecoveryPct, 2
    )
    summary_data$MeanRecoveryRate <- round(
      summary_data$MeanRecoveryRate, 2
    )
    summary_data$MedianRecoveryRate <- round(
      summary_data$MedianRecoveryRate, 2
    )
    summary_data$MeanLGD <- round(summary_data$MeanLGD, 2)
    
    names(summary_data) <- c(
      "Grade",
      "Loan Count",
      "Positive Recovery (%)",
      "Mean Recovery Rate (%)",
      "Median Recovery Rate (%)",
      "Mean LGD Proxy (%)"
    )
    
    summary_data
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 2)
  
  
  grade_validation_test_results <- reactive({
    data <- grade_validation_data()
    data <- data[!is.na(data$Grade), , drop = FALSE]
    
    # Test 1: Association between grade and positive recovery.
    positive_table <- table(data$Grade, data$PositiveRecovery)
    
    chi_result <- tryCatch(
      chisq.test(positive_table, correct = FALSE),
      error = function(e) NULL
    )
    
    # Test 2: Differences in recovery-rate distributions across grades.
    kw_result <- tryCatch(
      kruskal.test(RecoveryRate ~ Grade, data = data),
      error = function(e) NULL
    )
    
    chi_row <- if (!is.null(chi_result)) {
      data.frame(
        Outcome = "Positive recovery incidence",
        Test = "Chi-square test of independence",
        Statistic = unname(chi_result$statistic),
        DF = unname(chi_result$parameter),
        PValue = chi_result$p.value
      )
    } else {
      data.frame(
        Outcome = "Positive recovery incidence",
        Test = "Chi-square test of independence",
        Statistic = NA_real_,
        DF = NA_real_,
        PValue = NA_real_
      )
    }
    
    kw_row <- if (!is.null(kw_result)) {
      data.frame(
        Outcome = "Recovery-rate distribution",
        Test = "Kruskal-Wallis test",
        Statistic = unname(kw_result$statistic),
        DF = unname(kw_result$parameter),
        PValue = kw_result$p.value
      )
    } else {
      data.frame(
        Outcome = "Recovery-rate distribution",
        Test = "Kruskal-Wallis test",
        Statistic = NA_real_,
        DF = NA_real_,
        PValue = NA_real_
      )
    }
    
    result <- rbind(chi_row, kw_row)
    
    result$Decision <- ifelse(
      is.na(result$PValue),
      "Test unavailable",
      ifelse(
        result$PValue < 0.05,
        "Reject H0 at 5%",
        "Do not reject H0 at 5%"
      )
    )
    
    result
  })
  
  
  output$grade_validation_tests <- renderTable({
    result <- grade_validation_test_results()
    
    result$Statistic <- round(result$Statistic, 3)
    result$PValue <- ifelse(
      is.na(result$PValue),
      NA_character_,
      ifelse(
        result$PValue < 0.001,
        "< 0.001",
        formatC(result$PValue, format = "f", digits = 4)
      )
    )
    
    names(result) <- c(
      "Outcome",
      "Statistical Test",
      "Test Statistic",
      "Degrees of Freedom",
      "P-value",
      "Decision"
    )
    
    result
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  output$grade_validation_observations <- renderUI({
    summary_data <- grade_validation_summary()
    test_results <- grade_validation_test_results()
    
    validate(
      need(nrow(summary_data) >= 2,
           "At least two grades are required for validation.")
    )
    
    highest <- summary_data[
      which.max(summary_data$MeanRecoveryRate), ,
      drop = FALSE
    ]
    
    lowest <- summary_data[
      which.min(summary_data$MeanRecoveryRate), ,
      drop = FALSE
    ]
    
    chi_p <- test_results$PValue[
      test_results$Outcome == "Positive recovery incidence"
    ]
    
    kw_p <- test_results$PValue[
      test_results$Outcome == "Recovery-rate distribution"
    ]
    
    tagList(
      p(
        strong("Highest mean recovery rate: "),
        "Grade ", highest$Grade,
        " (", sprintf("%.2f%%", highest$MeanRecoveryRate), ")."
      ),
      p(
        strong("Lowest mean recovery rate: "),
        "Grade ", lowest$Grade,
        " (", sprintf("%.2f%%", lowest$MeanRecoveryRate), ")."
      ),
      p(
        strong("Positive-recovery incidence: "),
        if (length(chi_p) > 0 && !is.na(chi_p[1]) && chi_p[1] < 0.05) {
          "The chi-square test indicates a statistically significant association between grade and positive-recovery incidence at the 5% level."
        } else {
          "The chi-square test does not establish a statistically significant association at the 5% level, or the test was unavailable."
        }
      ),
      p(
        strong("Recovery-rate severity: "),
        if (length(kw_p) > 0 && !is.na(kw_p[1]) && kw_p[1] < 0.05) {
          "The Kruskal-Wallis test indicates that recovery-rate distributions differ across grades at the 5% level."
        } else {
          "The Kruskal-Wallis test does not establish statistically significant differences at the 5% level, or the test was unavailable."
        }
      ),
      p(
        "These tests assess association, not causality. Statistical significance alone does not establish that the grading system is well calibrated or that its ordering is optimal. The dataset contains defaulted loans only, so this page validates recovery differentiation conditional on default, not default-risk ranking across the full originated portfolio."
      )
    )
  })
  
  
  # ==========================================================
  # PART C1: RECOVERY PROBABILITY
  # ==========================================================
  
  # Key Stage 1 performance metrics
  output$c_prob_metrics <- renderUI({
    
    metrics <- stage1_dashboard$metrics
    
    fluidRow(
      column(
        3,
        card(
          card_body(
            h5("Test AUC"),
            h3(sprintf("%.4f", metrics$auc)),
            p("Discrimination")
          )
        )
      ),
      column(
        3,
        card(
          card_body(
            h5("Brier Score"),
            h3(sprintf("%.4f", metrics$brier)),
            p("Lower is better")
          )
        )
      ),
      column(
        3,
        card(
          card_body(
            h5("Recall"),
            h3(sprintf("%.2f%%", 100 * metrics$recall)),
            p("Positive recovery")
          )
        )
      ),
      column(
        3,
        card(
          card_body(
            h5("Specificity"),
            h3(sprintf("%.2f%%", 100 * metrics$specificity)),
            p("Zero recovery")
          )
        )
      )
    )
  })
  
  
  # Compare baseline logistic regression and tuned random forest
  output$c_prob_comparison <- renderTable({
    
    data.frame(
      Model = c(
        "Logistic Regression",
        "Tuned Random Forest"
      ),
      AUC = c(0.6061, 0.6252),
      Brier_Score = c(0.2378, 0.2344),
      Log_Loss = c(0.6682, 0.6608),
      Accuracy = c(0.5850, 0.6025),
      Precision = c(0.5978, 0.6124),
      Recall = c(0.8066, 0.8031),
      Specificity = c(0.2987, 0.3432),
      F1_Score = c(0.6866, 0.6949),
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 4)
  
  
  # Stage 1 calibration by predicted-probability decile
  output$c_prob_calibration <- renderPlot({
    
    calibration_data <- data.frame(
      Decile = factor(1:10, levels = 1:10),
      Mean_Predicted = c(
        0.3877, 0.4575, 0.4949, 0.5240, 0.5515,
        0.5782, 0.6044, 0.6337, 0.6697, 0.7404
      ),
      Observed_Rate = c(
        0.3942, 0.4139, 0.5006, 0.5283, 0.5260,
        0.5711, 0.6324, 0.6497, 0.6794, 0.7419
      )
    )
    
    ggplot(
      calibration_data,
      aes(x = Mean_Predicted, y = Observed_Rate)
    ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "#7B8794"
      ) +
      geom_line(
        color = "#263746",
        linewidth = 0.8
      ) +
      geom_point(
        color = "#B5121B",
        size = 3
      ) +
      geom_text(
        aes(label = Decile),
        nudge_y = 0.012,
        size = 3
      ) +
      coord_equal(xlim = c(0.35, 0.80), ylim = c(0.35, 0.80)) +
      scale_x_continuous(
        labels = scales::label_percent(accuracy = 1)
      ) +
      scale_y_continuous(
        labels = scales::label_percent(accuracy = 1)
      ) +
      labs(
        title = "Predicted vs Observed Positive-Recovery Probability",
        subtitle = "Each point represents one decile; the dashed line indicates perfect calibration",
        x = "Mean predicted probability",
        y = "Observed positive-recovery rate"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
    
  })
  
  
  # ==========================================================
  # PART C2: CONDITIONAL RECOVERY RATE
  # ==========================================================
  
  # Key Stage 2 performance metrics
  output$c_recovery_metrics <- renderUI({
    
    metrics <- stage2_dashboard$metrics
    
    fluidRow(
      column(
        6,
        card(
          card_body(
            h5("Test MAE"),
            h3(sprintf("%.4f", metrics$mae)),
            p("Mean absolute error; lower is better")
          )
        )
      ),
      column(
        6,
        card(
          card_body(
            h5("Test RMSE"),
            h3(sprintf("%.4f", metrics$rmse)),
            p("Root mean squared error; lower is better")
          )
        )
      )
    )
  })
  
  
  # Compare linear regression with tuned random forest
  output$c_recovery_comparison <- renderTable({
    
    data.frame(
      Model = c(
        "Linear Regression",
        "Baseline Random Forest",
        "Tuned Random Forest"
      ),
      Test_MAE = c(0.0521, 0.0515, 0.0515),
      Test_RMSE = c(0.0947, 0.0940, 0.0938),
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 4)
  
  
  # Stage 2 variable importance
  output$c_recovery_importance <- renderPlot({
    
    importance_data <- data.frame(
      Variable = names(stage2_dashboard$variable_importance),
      Importance = as.numeric(stage2_dashboard$variable_importance)
    )
    
    importance_data <- importance_data[
      order(importance_data$Importance),
    ]
    
    importance_data$Variable <- factor(
      importance_data$Variable,
      levels = importance_data$Variable
    )
    
    ggplot(
      importance_data,
      aes(x = Importance, y = Variable)
    ) +
      geom_col(fill = "#263746", width = 0.7) +
      labs(
        title = "Stage 2 Random Forest Variable Importance",
        subtitle = "Importance scores from the saved tuned model results",
        x = "Variable importance",
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
    
  })
  
  
  # ==========================================================
  # PART C3: LGD ESTIMATES
  # ==========================================================
  
  # Key combined LGD metrics
  output$c_lgd_metrics <- renderUI({
    
    metrics <- lgd_dashboard$metrics
    
    fluidRow(
      column(
        3,
        card(
          card_body(
            h5("Test MAE"),
            h3(sprintf("%.4f", metrics$mae)),
            p("Lower is better")
          )
        )
      ),
      column(
        3,
        card(
          card_body(
            h5("Test RMSE"),
            h3(sprintf("%.4f", metrics$rmse)),
            p("Lower is better")
          )
        )
      ),
      column(
        3,
        card(
          card_body(
            h5("Mean Predicted LGD"),
            h3(sprintf("%.2f%%", 100 * metrics$mean_predicted_lgd)),
            p("Test sample")
          )
        )
      ),
      column(
        3,
        card(
          card_body(
            h5("Mean Observed LGD"),
            h3(sprintf("%.2f%%", 100 * metrics$mean_observed_lgd)),
            p("Test sample")
          )
        )
      )
    )
  })
  
  
  # Predicted versus observed LGD by predicted-LGD decile
  output$c_lgd_calibration <- renderPlot({
    
    calibration_data <- lgd_dashboard$calibration
    
    ggplot(
      calibration_data,
      aes(x = PredictedLGDDecile)
    ) +
      geom_line(
        aes(y = MeanPredictedLGD, colour = "Predicted LGD"),
        linewidth = 1
      ) +
      geom_point(
        aes(y = MeanPredictedLGD, colour = "Predicted LGD"),
        size = 2.5
      ) +
      geom_line(
        aes(y = MeanObservedLGD, colour = "Observed LGD"),
        linewidth = 1
      ) +
      geom_point(
        aes(y = MeanObservedLGD, colour = "Observed LGD"),
        size = 2.5
      ) +
      scale_colour_manual(
        values = c(
          "Predicted LGD" = "#263746",
          "Observed LGD" = "#B5121B"
        )
      ) +
      scale_x_continuous(breaks = 1:10) +
      scale_y_continuous(
        labels = scales::label_percent(accuracy = 1)
      ) +
      labs(
        title = "Predicted vs Observed LGD by Decile",
        subtitle = "Loans grouped from lowest to highest predicted LGD",
        x = "Predicted-LGD decile",
        y = "Mean LGD",
        colour = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
  })
  
  
  # Compare predicted and observed LGD distributions
  output$c_lgd_distribution <- renderPlot({
    
    distribution_data <- lgd_dashboard$distribution_data
    
    ggplot(
      distribution_data,
      aes(x = LGD, colour = Type)
    ) +
      geom_density(linewidth = 1) +
      scale_colour_manual(
        values = c(
          "Predicted LGD" = "#263746",
          "Observed LGD" = "#B5121B"
        )
      ) +
      scale_x_continuous(
        labels = scales::label_percent(accuracy = 1)
      ) +
      labs(
        title = "Distribution of Predicted and Observed LGD",
        x = "LGD",
        y = "Density",
        colour = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
  })
  
  
  # ==========================================================
  # PART C4: MODEL PERFORMANCE
  # ==========================================================
  
  # Compare the two-stage model against the constant-mean benchmark
  output$c_lgd_benchmark <- renderTable({
    
    lgd_dashboard$model_comparison
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 4)
  
  
  # Display LGD prediction diagnostics
  output$c_lgd_diagnostics <- renderTable({
    
    lgd_dashboard$diagnostics
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 4)
  
  
  # Interpretation of the validation results
  output$c_model_interpretation <- renderUI({
    
    tagList(
      h4("Key Findings"),
      
      tags$ul(
        tags$li(
          tags$strong("Aggregate performance: "),
          "Mean predicted LGD is approximately 93.77%, compared with ",
          "93.94% observed LGD in the test sample."
        ),
        tags$li(
          tags$strong("Benchmark comparison: "),
          "The two-stage model achieves MAE of 0.0622 and RMSE of 0.0889, ",
          "compared with 0.0633 and 0.0895 for the training-mean benchmark. ",
          "The improvement is modest."
        ),
        tags$li(
          tags$strong("Limited loan-level discrimination: "),
          "The correlation between predicted and observed LGD is approximately ",
          "0.1272, indicating a weak linear relationship."
        ),
        tags$li(
          tags$strong("Compressed predictions: "),
          "Predicted LGD varies much less than observed LGD, so the model ",
          "does not fully capture the variation in individual loan outcomes."
        ),
        tags$li(
          tags$strong("Decile calibration: "),
          "Mean predicted and observed LGD are relatively close across ",
          "predicted-LGD deciles, although good group-level calibration ",
          "does not guarantee accurate predictions for individual loans."
        )
      ),
      
      h4("Limitations"),
      
      tags$ul(
        tags$li(
          "The observed LGD is a proxy calculated as one minus the recovery ",
          "rate based on funded amount. It does not incorporate recovery timing, ",
          "discounting, or collection costs."
        ),
        tags$li(
          "The dataset contains defaulted loans only. The framework estimates ",
          "recovery and LGD conditional on default; it does not estimate ",
          "probability of default for the full originated portfolio."
        ),
        tags$li(
          "The test results reflect the particular train-test split and should ",
          "be validated on additional samples or a later time period before ",
          "operational use."
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # KPI CARDS
  # ----------------------------------------------------------
  
  output$kpi_loan_count <- renderText({
    format(nrow(overview_data()), big.mark = ",")
  })
  
  output$kpi_positive_recovery <- renderText({
    data <- overview_data()
    positive <- sum(data$recoveries > 0, na.rm = TRUE)
    
    sprintf("%.2f%%", 100 * positive / nrow(data))
  })
  
  output$kpi_mean_recovery <- renderText({
    sprintf(
      "%.2f%%",
      100 * mean(overview_data()$recovery_rate, na.rm = TRUE)
    )
  })
  
  output$kpi_median_recovery <- renderText({
    sprintf(
      "%.2f%%",
      100 * median(overview_data()$recovery_rate, na.rm = TRUE)
    )
  })
  
  
  # ----------------------------------------------------------
  # CHART 1: RECOVERY OUTCOMES
  # ----------------------------------------------------------
  
  output$recovery_outcomes_plot <- renderPlot({
    
    data <- overview_data()
    
    outcomes <- data.frame(
      Outcome = c("Positive recovery", "Zero recovery"),
      Loans = c(
        sum(data$recoveries > 0, na.rm = TRUE),
        sum(data$recoveries == 0, na.rm = TRUE)
      )
    )
    
    outcomes$Percentage <- 100 * outcomes$Loans / sum(outcomes$Loans)
    
    ggplot(outcomes, aes(x = Outcome, y = Loans, fill = Outcome)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(
        aes(
          label = sprintf(
            "%s loans (%.1f%%)",
            format(Loans, big.mark = ","),
            Percentage
          )
        ),
        vjust = -0.5,
        size = 3.5
      ) +
      scale_fill_manual(
        values = c(
          "Positive recovery" = "#B5121B",
          "Zero recovery" = "#AEBAC6"
        )
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.18))
      ) +
      labs(x = NULL, y = "Number of loans") +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(color = "#263746")
      )
  })
  
  
  # ----------------------------------------------------------
  # CHART 2: RECOVERY RATE HISTOGRAM
  # ----------------------------------------------------------
  
  output$recovery_histogram <- renderPlot({
    
    data <- overview_data()
    
    recovery_rates <- data$recovery_rate
    
    recovery_rates <- recovery_rates[
      !is.na(recovery_rates) &
        is.finite(recovery_rates)
    ]
    
    histogram_data <- data.frame(
      recovery_rate = recovery_rates
    )
    
    ggplot(
      histogram_data,
      aes(x = recovery_rate)
    ) +
      geom_histogram(
        binwidth = 0.05,
        boundary = 0,
        fill = "#B5121B",
        color = "white"
      ) +
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.2),
        labels = scales::label_percent(accuracy = 1),
        limits = c(0, 1),
        expand = c(0, 0)
      ) +
      labs(
        x = "Recovery rate",
        y = "Number of loans"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  })
  
  
  # ----------------------------------------------------------
  # DYNAMIC KEY OBSERVATIONS
  # ----------------------------------------------------------
  
  output$overview_observations <- renderUI({
    
    data <- overview_data()
    
    recovery_rate <- data$recovery_rate
    recovery_rate <- recovery_rate[is.finite(recovery_rate)]
    
    positive_pct <- 100 * mean(data$recoveries > 0, na.rm = TRUE)
    mean_rate <- mean(recovery_rate)
    median_rate <- median(recovery_rate)
    
    skew_text <- if (mean_rate > median_rate) {
      "The mean exceeds the median, indicating a right-skewed recovery-rate distribution."
    } else if (mean_rate < median_rate) {
      "The median exceeds the mean, indicating a distribution with a longer lower tail."
    } else {
      "The mean and median recovery rates are equal."
    }
    
    tagList(
      tags$p(
        tags$strong("Recovery incidence: "),
        sprintf(
          "%.2f%% of loans generated positive recoveries.",
          positive_pct
        )
      ),
      
      tags$p(
        tags$strong("Distribution: "),
        skew_text
      ),
      
      tags$p(
        tags$strong("Scope: "),
        paste(
          "The dataset contains defaulted loans.",
          "These results describe recovery performance conditional on default;",
          "they do not estimate the probability of default."
        )
      )
    )
  })
  
  # ============================================================
  # PART A: SEGMENT ANALYSIS
  # ============================================================
  
  # Prepare data for the selected segment
  segment_data <- reactive({
    
    data <- overview_data()
    segment_var <- input$segment_variable
    
    req(segment_var %in% names(data))
    req(
      all(c("recoveries", "recovery_rate") %in% names(data))
    )
    
    segment <- as.character(data[[segment_var]])
    
    # Retain missing values as an explicit category
    segment[is.na(segment) | trimws(segment) == ""] <-
      "Missing / Unknown"
    
    data.frame(
      Segment = segment,
      Recoveries = data$recoveries,
      RecoveryRate = data$recovery_rate
    )
  })
  
  
  # Calculate segment-level summary statistics
  segment_summary <- reactive({
    
    data <- segment_data()
    
    # Split the portfolio into the selected groups
    groups <- split(data, data$Segment, drop = TRUE)
    
    summary_data <- do.call(
      rbind,
      lapply(names(groups), function(group_name) {
        
        group_data <- groups[[group_name]]
        
        valid_rates <- group_data$RecoveryRate[
          is.finite(group_data$RecoveryRate)
        ]
        
        positive_pct <- if (nrow(group_data) > 0) {
          100 * mean(group_data$Recoveries > 0, na.rm = TRUE)
        } else {
          NA_real_
        }
        
        data.frame(
          Segment = group_name,
          LoanCount = nrow(group_data),
          
          MeanRecoveryAmount = mean(
            group_data$Recoveries,
            na.rm = TRUE
          ),
          
          PositiveRecoveryPct = positive_pct,
          
          MeanRecoveryRate = if (length(valid_rates) > 0) {
            mean(valid_rates)
          } else {
            NA_real_
          },
          MedianRecoveryRate = if (length(valid_rates) > 0) {
            median(valid_rates)
          } else {
            NA_real_
          },
          MeanLGD = if (length(valid_rates) > 0) {
            mean(1 - valid_rates)
          } else {
            NA_real_
          }
        )
      })
    )
    
    rownames(summary_data) <- NULL
    
    summary_data[
      order(summary_data$MeanRecoveryRate, decreasing = TRUE,
            na.last = TRUE),
    ]
  })
  
  
  # CHART: MEAN RECOVERY RATE BY SEGMENT
  output$segment_recovery_plot <- renderPlot({
    
    summary_data <- segment_summary()
    
    plot_data <- summary_data[
      is.finite(summary_data$MeanRecoveryRate),
    ]
    
    validate(
      need(nrow(plot_data) > 0, "No valid recovery rates to display.")
    )
    
    ggplot(
      plot_data,
      aes(
        x = reorder(Segment, MeanRecoveryRate),
        y = MeanRecoveryRate
      )
    ) +
      geom_col(
        fill = "#B5121B",
        width = 0.7
      ) +
      geom_text(
        aes(
          label = scales::percent(
            MeanRecoveryRate,
            accuracy = 0.1
          )
        ),
        hjust = -0.15,
        size = 3.5
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = scales::label_percent(accuracy = 1),
        expand = expansion(mult = c(0, 0.18))
      ) +
      labs(
        x = NULL,
        y = "Mean recovery rate"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "#263746")
      )
  })
  
  
  # TABLE: SEGMENT PERFORMANCE
  output$segment_summary_table <- renderTable({
    
    summary_data <- segment_summary()
    
    display_data <- data.frame(
      Segment = summary_data$Segment,
      `Loan Count` = format(
        summary_data$LoanCount,
        big.mark = ",",
        scientific = FALSE
      ),
      `Positive Recovery (%)` = round(
        summary_data$PositiveRecoveryPct, 2
      ),
      `Mean Recovery Rate (%)` = round(
        100 * summary_data$MeanRecoveryRate, 2
      ),
      `Median Recovery Rate (%)` = round(
        100 * summary_data$MedianRecoveryRate, 2
      ),
      `Mean LGD Proxy (%)` = round(
        100 * summary_data$MeanLGD, 2
      ),
      `Mean Recovery Amount ($)` = round(
        summary_data$MeanRecoveryAmount,
        2
      ),
      check.names = FALSE
    )
    
    display_data
  },
  striped = TRUE,
  bordered = FALSE,
  hover = TRUE,
  spacing = "s",
  na = "—"
  )
  
  
  # DYNAMIC SEGMENT FINDINGS
  output$segment_observations <- renderUI({
    
    summary_data <- segment_summary()
    
    valid_data <- summary_data[
      is.finite(summary_data$MeanRecoveryRate),
    ]
    
    validate(
      need(nrow(valid_data) > 0, "No segment results are available.")
    )
    
    highest <- valid_data[
      which.max(valid_data$MeanRecoveryRate),
    ]
    
    lowest <- valid_data[
      which.min(valid_data$MeanRecoveryRate),
    ]
    
    tagList(
      
      tags$p(
        tags$strong("Highest mean recovery: "),
        sprintf(
          "%s (%.2f%%), based on %s loans.",
          highest$Segment,
          100 * highest$MeanRecoveryRate,
          format(highest$LoanCount, big.mark = ",")
        )
      ),
      
      tags$p(
        tags$strong("Lowest mean recovery: "),
        sprintf(
          "%s (%.2f%%), based on %s loans.",
          lowest$Segment,
          100 * lowest$MeanRecoveryRate,
          format(lowest$LoanCount, big.mark = ",")
        )
      ),
      
      tags$p(
        tags$strong("Interpretation: "),
        paste(
          "These are descriptive differences in recovery performance",
          "among defaulted loans. They do not establish causation;",
          "differences may reflect borrower characteristics, loan terms,",
          "or other factors."
        )
      )
    )
  })
  
  # ============================================================
  # SECTION 8: LGD SIMULATOR
  # ============================================================
  
  observeEvent(input$sim_run, {
    
    # Step 1: Collect the 19 predictor values
    new_loan <- data.frame(
      funded_amnt = input$sim_funded_amnt,
      term = input$sim_term,
      int_rate = input$sim_int_rate,
      grade = input$sim_grade,
      purpose = input$sim_purpose,
      home_ownership = input$sim_home_ownership,
      annual_inc = input$sim_annual_inc,
      verification_status = input$sim_verification_status,
      emp_length_model = input$sim_emp_length,
      dti = input$sim_dti,
      delinq_2yrs = input$sim_delinq,
      inq_last_6mths = input$sim_inq,
      open_acc = input$sim_open_acc,
      pub_rec = input$sim_pub_rec,
      revol_bal = input$sim_revol_bal,
      revol_util = input$sim_revol_util,
      total_acc = input$sim_total_acc,
      mths_since_earliest_cr_line = input$sim_credit_history,
      acc_now_delinq = input$sim_acc_now_delinq,
      stringsAsFactors = FALSE
    )
    
    # Step 2: Match categorical levels used when training the models
    factor_levels <- list(
      term = c(" 36 months", " 60 months"),
      grade = c("A", "B", "C", "D", "E", "F", "G"),
      purpose = c(
        "car", "credit_card", "debt_consolidation", "educational",
        "home_improvement", "house", "major_purchase", "medical",
        "moving", "other", "renewable_energy", "small_business",
        "vacation", "wedding"
      ),
      home_ownership = c("MORTGAGE", "NONE", "OTHER", "OWN", "RENT"),
      verification_status = c(
        "Not Verified", "Source Verified", "Verified"
      ),
      emp_length_model = c(
        "< 1 year", "1 year", "10+ years", "2 years", "3 years",
        "4 years", "5 years", "6 years", "7 years", "8 years",
        "9 years", "Unknown"
      )
    )
    
    for (variable in names(factor_levels)) {
      new_loan[[variable]] <- factor(
        new_loan[[variable]],
        levels = factor_levels[[variable]]
      )
    }
    
    # Stop if any categorical input does not match the model levels
    invalid_factors <- names(factor_levels)[
      vapply(
        names(factor_levels),
        function(variable) is.na(new_loan[[variable]][1]),
        logical(1)
      )
    ]
    
    if (length(invalid_factors) > 0) {
      showNotification(
        paste(
          "Invalid categorical input:",
          paste(invalid_factors, collapse = ", ")
        ),
        type = "error"
      )
      return()
    }
    
    # Step 3: Estimate probability of positive recovery (Stage 1)
    recovery_probability <- tryCatch(
      {
        stage1_probability <- predict(
          stage1_dashboard$model,
          newdata = new_loan,
          type = "prob"
        )
        
        as.numeric(stage1_probability[1, "1"])
      },
      error = function(e) {
        showNotification(
          paste("Stage 1 prediction failed:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        return(NA_real_)
      }
    )
    
    if (!is.finite(recovery_probability)) {
      return()
    }
    
    # Step 4: Estimate recovery rate conditional on positive recovery
    conditional_recovery_rate <- tryCatch(
      {
        as.numeric(
          predict(
            stage2_dashboard$model,
            newdata = new_loan
          )[1]
        )
      },
      error = function(e) {
        showNotification(
          paste("Stage 2 prediction failed:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        return(NA_real_)
      }
    )
    
    if (!is.finite(conditional_recovery_rate)) {
      return()
    }
    
    # Step 5: Calculate expected recovery rate and LGD
    recovery_probability <- max(0, min(1, recovery_probability))
    conditional_recovery_rate <- max(
      0, min(1, conditional_recovery_rate)
    )
    
    expected_recovery_rate <-
      recovery_probability * conditional_recovery_rate
    
    estimated_lgd <- 1 - expected_recovery_rate
    
    
    # Display the main LGD estimate prominently
    output$sim_lgd_highlight <- renderUI({
      div(
        style = paste(
          "padding: 22px;",
          "margin-bottom: 18px;",
          "border-radius: 10px;",
          "background-color: #f0f4f8;",
          "text-align: center;"
        ),
        h4("Estimated Loss Given Default"),
        tags$h1(
          style = "font-size: 38px; font-weight: 700; margin: 8px 0;",
          sprintf("%.2f%%", 100 * estimated_lgd)
        ),
        p("Estimated proportion of exposure lost, conditional on default.")
      )
    })
    
    # Supporting metric: probability of positive recovery
    output$sim_probability_card <- renderUI({
      tags$h3(
        style = "text-align: center; margin: 10px 0;",
        sprintf("%.2f%%", 100 * recovery_probability)
      )
    })
    
    # Supporting metric: recovery rate conditional on positive recovery
    output$sim_conditional_card <- renderUI({
      tags$h3(
        style = "text-align: center; margin: 10px 0;",
        sprintf("%.2f%%", 100 * conditional_recovery_rate)
      )
    })
    
    # Supporting metric: expected recovery rate
    output$sim_recovery_card <- renderUI({
      tags$h3(
        style = "text-align: center; margin: 10px 0;",
        sprintf("%.2f%%", 100 * expected_recovery_rate)
      )
    })
    
    
    # Step 6: Display the estimates
    output$sim_results <- renderTable({
      data.frame(
        Metric = c(
          "Probability of Positive Recovery",
          "Conditional Recovery Rate",
          "Expected Recovery Rate",
          "Estimated Loss Given Default (LGD)"
        ),
        Estimate = sprintf(
          "%.2f%%",
          100 * c(
            recovery_probability,
            conditional_recovery_rate,
            expected_recovery_rate,
            estimated_lgd
          )
        ),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "m")
    
  })
  
  
  # ==========================================================
  # PART D.1: ROBUSTNESS AND RELIABILITY
  # ==========================================================
  
  output$d_robustness_content <- renderUI({
    
    req(part_d_results)
    
    tagList(
      
      h3("Robustness and Reliability"),
      
      p(
        "The following results assess test-set uncertainty, ",
        "prediction errors across credit grades, and the ",
        "variability captured by the LGD framework."
      ),
      
      h4("Bootstrap Uncertainty"),
      
      p(
        "The 95% percentile intervals below quantify sampling ",
        "uncertainty conditional on the existing test predictions."
      ),
      
      tableOutput("d_bootstrap_table"),
      
      hr(),
      
      h4("Prediction Error by Credit Grade"),
      
      p(
        "MAE measures average absolute error. RMSE penalises ",
        "larger errors more heavily. Mean error is predicted ",
        "LGD minus observed LGD."
      ),
      
      tableOutput("d_grade_performance_table"),
      
      hr(),
      
      h4("Predicted versus Observed LGD Variability"),
      
      p(
        "Comparing standard deviations indicates whether the ",
        "predictions capture the variability observed in actual LGD."
      ),
      
      tableOutput("d_grade_variability_table")
    )
  })
  
  
  # Bootstrap confidence intervals
  output$d_bootstrap_table <- renderTable({
    
    req(part_d_results$bootstrap_summary)
    
    part_d_results$bootstrap_summary
    
  }, digits = 4, striped = TRUE, bordered = TRUE)
  
  
  # Grade-level performance
  output$d_grade_performance_table <- renderTable({
    
    req(part_d_results$grade_robustness)
    
    x <- part_d_results$grade_robustness
    
    x[, c("Grade", "Loans", "MAE", "RMSE", "Mean_Error")]
    
  }, digits = 4, striped = TRUE, bordered = TRUE)
  
  
  # Predicted versus observed variability by grade
  output$d_grade_variability_table <- renderTable({
    
    req(part_d_results$grade_robustness)
    
    x <- part_d_results$grade_robustness
    
    x[, c(
      "Grade",
      "Predicted_LGD_SD",
      "Observed_LGD_SD"
    )]
    
  }, digits = 4, striped = TRUE, bordered = TRUE)
  
  
  # ==========================================================
  # PART D.2: PORTFOLIO SENSITIVITY
  # ==========================================================
  
  output$d_sensitivity_content <- renderUI({
    
    req(part_d_results)
    
    tagList(
      
      h3("Portfolio Composition Sensitivity"),
      
      p(
        "This analysis illustrates how changes in the distribution ",
        "of defaulted loans across credit grades affect estimated LGD ",
        "and implied recovery rates. Grade-level outcomes are held ",
        "fixed to isolate the effect of portfolio composition."
      ),
      
      h4("Portfolio Composition by Grade"),
      
      tableOutput("d_composition_table"),
      
      hr(),
      
      h4("Scenario Results"),
      
      tableOutput("d_scenario_results_table"),
      
      hr(),
      
      p(
        "Interpretation: these are illustrative scenarios based on ",
        "the existing test set, not forecasts of future portfolio ",
        "losses. The grade labels should not be interpreted as a ",
        "universal ranking of recovery risk."
      )
    )
  })
  
  
  # Portfolio grade composition
  output$d_composition_table <- renderTable({
    
    req(part_d_results$composition_table)
    
    part_d_results$composition_table
    
  }, digits = 4, striped = TRUE, bordered = TRUE)
  
  
  # Portfolio sensitivity results
  output$d_scenario_results_table <- renderTable({
    
    req(part_d_results$scenario_results)
    
    x <- part_d_results$scenario_results
    
    x[, c(
      "Scenario",
      "Predicted_LGD",
      "Implied_Recovery_Rate",
      "Observed_LGD_Reference",
      "Prediction_Error",
      "Change_vs_Baseline_pp"
    )]
    
  }, digits = 4, striped = TRUE, bordered = TRUE)
  
  
  # ==========================================================
  # PART D.3: MODEL RISK AND RECOMMENDED CONTROLS
  # ==========================================================
  
  output$d_risk_content <- renderUI({
    
    tagList(
      
      h3("Model Risk Assessment"),
      
      p(
        "The following risks are based on the observed test-set ",
        "performance, grade-level diagnostics, and assumptions ",
        "underlying the portfolio sensitivity analysis."
      ),
      
      h4("Key Model Risks"),
      
      tableOutput("d_model_risk_table"),
      
      hr(),
      
      h4("Recommended Controls"),
      
      tags$ul(
        
        tags$li(
          tags$strong("Performance monitoring: "),
          "Track MAE, RMSE, and prediction bias over time and ",
          "across credit grades."
        ),
        
        tags$li(
          tags$strong("Stage 1 validation: "),
          "Monitor discrimination and probability calibration, ",
          "including the trade-off between recall and specificity."
        ),
        
        tags$li(
          tags$strong("Out-of-time validation: "),
          "Evaluate the framework on a later, independent sample ",
          "to assess stability under changing portfolio conditions."
        ),
        
        tags$li(
          tags$strong("Data and predictor controls: "),
          "Confirm that every predictor is available at the ",
          "intended prediction date and exclude information ",
          "that would only become available afterwards."
        ),
        
        tags$li(
          tags$strong("Benchmarking and review: "),
          "Compare performance with a simple benchmark and ",
          "investigate material deterioration or systematic ",
          "errors in individual segments."
        ),
        
        tags$li(
          tags$strong("Scenario interpretation: "),
          "Treat portfolio-composition scenarios as illustrative ",
          "sensitivity analyses, not forecasts or causal estimates."
        )
      ),
      
      hr(),
      
      h4("Overall Assessment"),
      
      p(
        "The framework modestly improves upon the benchmark ",
        "on the test set, but limited Stage 1 discrimination, ",
        "compressed LGD predictions, and variation in grade-level ",
        "errors restrict confidence in individual loan estimates. ",
        "The model should therefore be subject to ongoing ",
        "monitoring and independent validation before operational use."
      )
    )
  })
  
  
  # Evidence-based model risk table
  output$d_model_risk_table <- renderTable({
    
    data.frame(
      Risk = c(
        "Limited Stage 1 discrimination",
        "Prediction compression",
        "Uneven performance by grade",
        "Validation uncertainty",
        "Portfolio scenario assumptions",
        "Predictor availability"
      ),
      
      Evidence = c(
        "Test AUC = 0.6252; specificity = 0.3432",
        "Predicted LGD SD = 0.0145; observed LGD SD = 0.0896",
        "Grade-level MAE ranges from 0.0531 to 0.0752",
        "Bootstrap intervals condition on existing fitted models",
        "Within-grade outcomes remain fixed in scenarios",
        "Availability at the intended prediction date must be verified"
      ),
      
      Implication = c(
        "Recovery probabilities may not separate outcomes reliably",
        "Individual outcomes may vary more than predictions suggest",
        "Portfolio-wide metrics can conceal segment-level weaknesses",
        "Future model and data uncertainty is not fully captured",
        "Scenario results are illustrative, not forecasts",
        "Post-outcome information could introduce leakage if used"
      ),
      
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
}




# ============================================================
# SECTION 9: RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)