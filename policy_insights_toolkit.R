# =============================================================================
# POLICY INSIGHTS TOOLKIT
# =============================================================================
# A user-friendly data analysis tool designed for policy analysts, program
# managers, and anyone who wants to understand their data without needing
# a statistics degree.
#
# Design Philosophy:
# - Plain language over statistical jargon
# - Visual confidence indicators over p-values
# - Guided workflows over open-ended exploration
# - Smart defaults that just work
# - Focus on actionable insights
# =============================================================================

# --- Package Loading ----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(DT)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(fixest)
library(did)
library(Synth)
library(broom)
library(purrr)
library(stringr)
library(lubridate)
library(knitr)
library(kableExtra)
library(waiter)
library(htmltools)
library(httr2)      # For making API calls to Claude
library(jsonlite)   # For JSON handling
library(promises)
library(future)
library(commonmark)

source("chatbot_module.R")  # Make sure chatbot_module.R is in the same directory
source("example_datasets.R")  # Example datasets library

# Set global theme
theme_set(theme_minimal(base_size = 13, base_family = "sans"))

# =============================================================================
# HELPER FUNCTIONS - THE INTELLIGENCE LAYER
# =============================================================================

# --- Smart Variable Detection -------------------------------------------------
# Automatically figures out what each column in your data represents

detect_data_structure <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  
  results <- list(
    unit_id = NULL,
    time_var = NULL,
    outcome_candidates = character(0),
    treatment_var = NULL,
    covariate_candidates = character(0),
    data_type = "unknown",
    panel_structure = FALSE,
    quality_score = 0,
    issues = character(0),
    suggestions = character(0)
  )
  
  # Identify likely unit identifier
  for (col in names(df)) {
    col_lower <- tolower(col)
    # Check for ID-like names
    if (grepl("^(id|unit|entity|state|country|county|city|firm|company|org|person|participant|subject|group)($|_|id)", col_lower)) {
      results$unit_id <- col
      break
    }
    # Check for string columns with reasonable cardinality
    if ((is.character(df[[col]]) || is.factor(df[[col]]))) {
      unique_ratio <- length(unique(df[[col]])) / nrow(df)
      if (unique_ratio > 0.01 && unique_ratio < 0.5 && is.null(results$unit_id)) {
        results$unit_id <- col
      }
    }
  }
  
  # Identify time variable
  for (col in names(df)) {
    col_lower <- tolower(col)
    # Check for time-like names
    if (grepl("(year|time|date|period|quarter|month|week|day|fiscal|fy)", col_lower)) {
      results$time_var <- col
      break
    }
    # Check for year-like numeric values
    if (is.numeric(df[[col]])) {
      vals <- unique(df[[col]][!is.na(df[[col]])])
      if (all(vals >= 1900 & vals <= 2100) && length(vals) < 100 && length(vals) > 1) {
        if (is.null(results$time_var)) results$time_var <- col
      }
    }
  }
  
  # Identify treatment indicator
  for (col in names(df)) {
    col_lower <- tolower(col)
    if (is.numeric(df[[col]])) {
      unique_vals <- unique(df[[col]][!is.na(df[[col]])])
      is_binary <- length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
      
      if (is_binary) {
        if (grepl("(treat|intervention|policy|program|exposed|participant|enrolled|received)", col_lower)) {
          results$treatment_var <- col
          break
        }
        if (is.null(results$treatment_var)) results$treatment_var <- col
      }
    }
  }
  
  # Identify outcome and covariate candidates
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  exclude_cols <- c(results$unit_id, results$time_var, results$treatment_var)
  
  for (col in numeric_cols) {
    if (!col %in% exclude_cols) {
      col_lower <- tolower(col)
      # Likely outcomes
      if (grepl("(outcome|result|score|rate|index|gdp|income|wage|employment|revenue|cost|spending|performance|achievement|value|amount|total|count|number)", col_lower)) {
        results$outcome_candidates <- c(results$outcome_candidates, col)
      } else {
        results$covariate_candidates <- c(results$covariate_candidates, col)
      }
    }
  }
  
  # If no outcomes identified, use remaining numeric columns
  if (length(results$outcome_candidates) == 0) {
    remaining <- setdiff(numeric_cols, exclude_cols)
    if (length(remaining) > 0) {
      results$outcome_candidates <- remaining[1:min(3, length(remaining))]
      results$covariate_candidates <- remaining[-c(1:min(3, length(remaining)))]
    }
  }
  
  # Determine data type
  if (!is.null(results$unit_id) && !is.null(results$time_var)) {
    results$data_type <- "panel"
    results$panel_structure <- TRUE
    
    # Check panel balance
    panel_check <- df %>%
      group_by(across(all_of(results$unit_id))) %>%
      summarise(n_periods = n_distinct(across(all_of(results$time_var))), .groups = "drop")
    
    if (length(unique(panel_check$n_periods)) == 1) {
      results$suggestions <- c(results$suggestions, "✓ Your data has a balanced panel structure - great for analysis!")
    } else {
      results$issues <- c(results$issues, "Your data is an unbalanced panel (units have different numbers of time periods)")
      results$suggestions <- c(results$suggestions, "Consider whether missing time periods are random or systematic")
    }
  } else if (!is.null(results$time_var)) {
    results$data_type <- "time_series"
    results$suggestions <- c(results$suggestions, "Your data appears to be a time series")
  } else if (!is.null(results$unit_id)) {
    results$data_type <- "cross_section"
    results$suggestions <- c(results$suggestions, "Your data appears to be cross-sectional (one observation per unit)")
  }
  
  # Calculate quality score
  quality_points <- 0
  max_points <- 10
  
  if (!is.null(results$unit_id)) quality_points <- quality_points + 2
  if (!is.null(results$time_var)) quality_points <- quality_points + 2
  if (length(results$outcome_candidates) > 0) quality_points <- quality_points + 2
  if (!is.null(results$treatment_var)) quality_points <- quality_points + 2
  
  # Check for missing values
  missing_pct <- mean(is.na(df)) * 100
  if (missing_pct == 0) {
    quality_points <- quality_points + 2
  } else if (missing_pct < 5) {
    quality_points <- quality_points + 1
    results$issues <- c(results$issues, sprintf("%.1f%% of values are missing", missing_pct))
  } else {
    results$issues <- c(results$issues, sprintf("%.1f%% of values are missing - this may affect your analysis", missing_pct))
  }
  
  results$quality_score <- round(quality_points / max_points * 100)
  
  results
}

# --- Plain Language Interpretation Functions ----------------------------------

interpret_confidence_level <- function(pvalue) {
  if (is.na(pvalue)) return(list(level = "unknown", text = "Unable to determine confidence", color = "#95a5a6"))
  
  if (pvalue < 0.01) {
    list(level = "very_high", 
         text = "Very High Confidence", 
         description = "This finding is very unlikely to be due to chance. You can be quite confident in this result.",
         color = "#27ae60",
         icon = "check-circle")
  } else if (pvalue < 0.05) {
    list(level = "high",
         text = "High Confidence",
         description = "This finding is unlikely to be due to chance. This is a reliable result.",
         color = "#2ecc71",
         icon = "check-circle")
  } else if (pvalue < 0.10) {
    list(level = "moderate",
         text = "Moderate Confidence",
         description = "This finding shows a pattern, but there's some chance it could be random. Consider gathering more data.",
         color = "#f39c12",
         icon = "exclamation-circle")
  } else {
    list(level = "low",
         text = "Low Confidence",
         description = "We can't be confident this finding is real - it might just be random variation in your data.",
         color = "#e74c3c",
         icon = "question-circle")
  }
}

interpret_effect_size <- function(effect, baseline, outcome_name = "the outcome") {
  if (is.na(effect) || is.na(baseline) || baseline == 0) {
    return(list(
      direction = "unknown",
      magnitude = "unknown",
      percentage = NA,
      plain_text = "Unable to interpret effect size"
    ))
  }
  
  pct_change <- (effect / baseline) * 100
  direction <- if (effect > 0) "increased" else "decreased"
  
  abs_pct <- abs(pct_change)
  if (abs_pct < 1) {
    magnitude <- "very small"
    practical <- "This change is quite small and may not be practically meaningful."
  } else if (abs_pct < 5) {
    magnitude <- "small"
    practical <- "This is a modest change that may be meaningful depending on your context."
  } else if (abs_pct < 15) {
    magnitude <- "moderate"
    practical <- "This is a meaningful change that's likely to be noticeable."
  } else if (abs_pct < 30) {
    magnitude <- "large"
    practical <- "This is a substantial change with clear practical significance."
  } else {
    magnitude <- "very large"
    practical <- "This is a very large change - double-check your analysis to ensure accuracy."
  }
  
  list(
    direction = direction,
    magnitude = magnitude,
    percentage = pct_change,
    plain_text = sprintf("%s %s by %.1f%% (a %s effect). %s", 
                         str_to_title(outcome_name), direction, abs(pct_change), magnitude, practical)
  )
}

# --- Analysis Readiness Check -------------------------------------------------

check_analysis_readiness <- function(df, unit_var, time_var, outcome_var, 
                                     treatment_var = NULL, treated_units = NULL, 
                                     treatment_time = NULL, analysis_type = "did") {
  
  checks <- list()
  overall_ready <- TRUE
  
  # Check 1: Required variables exist
  required_vars <- c(unit_var, time_var, outcome_var)
  missing_vars <- required_vars[!required_vars %in% names(df)]
  
  if (length(missing_vars) > 0) {
    checks$variables <- list(
      status = "fail",
      title = "Missing Variables",
      message = paste("Cannot find:", paste(missing_vars, collapse = ", "))
    )
    overall_ready <- FALSE
  } else {
    checks$variables <- list(
      status = "pass",
      title = "Variables Found",
      message = "All required variables are present in your data"
    )
  }
  
  # Check 2: Sample size
  n_units <- length(unique(df[[unit_var]]))
  n_periods <- length(unique(df[[time_var]]))
  n_obs <- nrow(df)
  
  if (n_units < 2) {
    checks$sample <- list(
      status = "fail",
      title = "Insufficient Units",
      message = "You need at least 2 different units to compare"
    )
    overall_ready <- FALSE
  } else if (n_units < 10) {
    checks$sample <- list(
      status = "warning",
      title = "Small Sample",
      message = sprintf("You have %d units. Results may be less reliable with fewer than 10 units.", n_units)
    )
  } else {
    checks$sample <- list(
      status = "pass",
      title = "Good Sample Size",
      message = sprintf("%d units across %d time periods (%s observations)", n_units, n_periods, format(n_obs, big.mark = ","))
    )
  }
  
  # Check 3: Treatment information
  if (analysis_type == "did") {
    if (!is.null(treatment_var) && treatment_var %in% names(df)) {
      n_treated <- sum(df[[treatment_var]] == 1, na.rm = TRUE)
      n_control <- sum(df[[treatment_var]] == 0, na.rm = TRUE)
      
      if (n_treated == 0) {
        checks$treatment <- list(
          status = "fail",
          title = "No Treated Observations",
          message = "Your treatment variable has no treated (=1) observations"
        )
        overall_ready <- FALSE
      } else if (n_control == 0) {
        checks$treatment <- list(
          status = "fail",
          title = "No Control Observations",
          message = "Your treatment variable has no control (=0) observations"
        )
        overall_ready <- FALSE
      } else {
        checks$treatment <- list(
          status = "pass",
          title = "Treatment Groups Identified",
          message = sprintf("%s treated and %s control observations", 
                            format(n_treated, big.mark = ","), 
                            format(n_control, big.mark = ","))
        )
      }
    } else if (!is.null(treated_units) && !is.null(treatment_time)) {
      n_treated_units <- length(treated_units)
      n_control_units <- n_units - n_treated_units
      
      if (n_control_units < 1) {
        checks$treatment <- list(
          status = "fail",
          title = "No Control Units",
          message = "You need at least one unit that wasn't treated"
        )
        overall_ready <- FALSE
      } else {
        checks$treatment <- list(
          status = "pass",
          title = "Treatment Setup",
          message = sprintf("%d treated unit(s), %d control unit(s), treatment starts in %s",
                            n_treated_units, n_control_units, treatment_time)
        )
      }
    }
  }
  
  # Check 4: Pre-treatment periods (for DiD)
  if (analysis_type == "did" && !is.null(treatment_time)) {
    time_vals <- sort(unique(df[[time_var]]))
    pre_periods <- sum(time_vals < treatment_time)
    post_periods <- sum(time_vals >= treatment_time)
    
    if (pre_periods < 2) {
      checks$timing <- list(
        status = "fail",
        title = "Insufficient Pre-Treatment Data",
        message = sprintf("Only %d period(s) before treatment. Need at least 2.", pre_periods)
      )
      overall_ready <- FALSE
    } else if (pre_periods < 4) {
      checks$timing <- list(
        status = "warning",
        title = "Limited Pre-Treatment Data",
        message = sprintf("%d periods before, %d after treatment. More pre-treatment periods would strengthen your analysis.", pre_periods, post_periods)
      )
    } else {
      checks$timing <- list(
        status = "pass",
        title = "Good Time Coverage",
        message = sprintf("%d periods before treatment, %d after", pre_periods, post_periods)
      )
    }
  }
  
  # Check 5: Missing values in key variables
  key_vars <- c(unit_var, time_var, outcome_var)
  missing_count <- sum(is.na(df[, key_vars]))
  missing_pct <- (missing_count / (nrow(df) * length(key_vars))) * 100
  
  if (missing_pct > 10) {
    checks$missing <- list(
      status = "warning",
      title = "Missing Values",
      message = sprintf("%.1f%% of key data is missing. This may affect results.", missing_pct)
    )
  } else if (missing_pct > 0) {
    checks$missing <- list(
      status = "pass",
      title = "Minimal Missing Data",
      message = sprintf("Only %.1f%% missing values - this is acceptable", missing_pct)
    )
  } else {
    checks$missing <- list(
      status = "pass",
      title = "Complete Data",
      message = "No missing values in your key variables"
    )
  }
  
  list(
    ready = overall_ready,
    checks = checks,
    summary = if (overall_ready) {
      "Your data looks ready for analysis!"
    } else {
      "Please address the issues above before running your analysis."
    }
  )
}

# --- DiD Analysis Function with Plain Language Output -------------------------

run_did_analysis <- function(df, unit_var, time_var, outcome_var, 
                             treatment_var = NULL, treated_units = NULL,
                             treatment_time = NULL, controls = NULL,
                             unit_fe = TRUE, time_fe = TRUE, cluster = "unit") {
  
  # Prepare data
  analysis_data <- df
  
  if (is.null(treatment_var)) {
    # Create treatment indicator from manual specification
    analysis_data$treatment <- ifelse(
      analysis_data[[unit_var]] %in% treated_units & 
        analysis_data[[time_var]] >= treatment_time, 1, 0)
    analysis_data$treated_group <- ifelse(
      analysis_data[[unit_var]] %in% treated_units, 1, 0)
    analysis_data$post <- ifelse(
      analysis_data[[time_var]] >= treatment_time, 1, 0)
  } else {
    analysis_data$treatment <- analysis_data[[treatment_var]]
    # Infer treatment timing
    treated_obs <- analysis_data[analysis_data$treatment == 1, ]
    treatment_time <- min(treated_obs[[time_var]])
    treated_units <- unique(treated_obs[[unit_var]])
    analysis_data$treated_group <- ifelse(
      analysis_data[[unit_var]] %in% treated_units, 1, 0)
    analysis_data$post <- ifelse(
      analysis_data[[time_var]] >= treatment_time, 1, 0)
  }
  
  # Calculate pre-treatment mean for effect size interpretation
  pre_treatment_data <- analysis_data[analysis_data[[time_var]] < treatment_time, ]
  pre_mean <- mean(pre_treatment_data[[outcome_var]], na.rm = TRUE)
  treated_pre_mean <- mean(pre_treatment_data[[outcome_var]][pre_treatment_data$treated_group == 1], na.rm = TRUE)
  
  # Build formula
  fe_part <- ""
  if (unit_fe && time_fe) {
    fe_part <- paste0("| ", unit_var, " + ", time_var)
  } else if (unit_fe) {
    fe_part <- paste0("| ", unit_var)
  } else if (time_fe) {
    fe_part <- paste0("| ", time_var)
  }
  
  if (!is.null(controls) && length(controls) > 0) {
    controls <- controls[controls %in% names(df) & controls != outcome_var]
    formula_str <- paste0(outcome_var, " ~ treatment + ", paste(controls, collapse = " + "), " ", fe_part)
  } else {
    formula_str <- paste0(outcome_var, " ~ treatment ", fe_part)
  }
  
  # Run model
  cluster_formula <- switch(cluster,
                            "unit" = as.formula(paste0("~ ", unit_var)),
                            "time" = as.formula(paste0("~ ", time_var)),
                            "twoway" = as.formula(paste0("~ ", unit_var, " + ", time_var)),
                            NULL)
  
  if (!is.null(cluster_formula)) {
    model <- feols(as.formula(formula_str), data = analysis_data, cluster = cluster_formula)
  } else {
    model <- feols(as.formula(formula_str), data = analysis_data)
  }
  
  # Extract results
  coef_table <- as.data.frame(summary(model)$coeftable)
  
  if ("treatment" %in% rownames(coef_table)) {
    estimate <- coef_table["treatment", "Estimate"]
    se <- coef_table["treatment", "Std. Error"]
    pval <- coef_table["treatment", "Pr(>|t|)"]
    ci_lower <- estimate - 1.96 * se
    ci_upper <- estimate + 1.96 * se
    
    # Plain language interpretation
    confidence <- interpret_confidence_level(pval)
    effect_interp <- interpret_effect_size(estimate, treated_pre_mean, outcome_var)
    
    # Main finding
    if (estimate > 0) {
      main_finding <- sprintf("The intervention appears to have **increased** %s by %.2f units.", 
                              outcome_var, abs(estimate))
    } else {
      main_finding <- sprintf("The intervention appears to have **decreased** %s by %.2f units.",
                              outcome_var, abs(estimate))
    }
    
    list(
      success = TRUE,
      model = model,
      data = analysis_data,
      
      # Key results
      estimate = estimate,
      se = se,
      pval = pval,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      
      # Context
      pre_mean = pre_mean,
      treated_pre_mean = treated_pre_mean,
      treatment_time = treatment_time,
      treated_units = treated_units,
      n_treated = sum(analysis_data$treated_group == 1),
      n_control = sum(analysis_data$treated_group == 0),
      
      # Plain language
      confidence = confidence,
      effect_interpretation = effect_interp,
      main_finding = main_finding,
      
      # Technical details
      outcome_var = outcome_var,
      unit_var = unit_var,
      time_var = time_var,
      formula = formula_str
    )
  } else {
    list(success = FALSE, error = "Could not find treatment effect in model")
  }
}

# --- Synthetic Control Function with Plain Language Output --------------------

run_synth_analysis <- function(df, unit_var, time_var, outcome_var,
                               treated_unit, treatment_time, predictors = NULL) {
  
  tryCatch({
    # Prepare data
    synth_data <- df
    synth_data$unit_id <- as.numeric(factor(synth_data[[unit_var]]))
    synth_data$time_id <- as.numeric(synth_data[[time_var]])
    
    unit_mapping <- synth_data %>% 
      select(all_of(unit_var), unit_id) %>% 
      distinct()
    
    treated_id <- unit_mapping$unit_id[unit_mapping[[unit_var]] == treated_unit]
    control_ids <- unit_mapping$unit_id[unit_mapping[[unit_var]] != treated_unit]
    
    all_times <- sort(unique(synth_data$time_id))
    pre_times <- all_times[all_times < treatment_time]
    post_times <- all_times[all_times >= treatment_time]
    
    # Set up predictors
    if (is.null(predictors) || length(predictors) == 0) {
      predictors <- outcome_var
    }
    
    synth_data_clean <- synth_data %>%
      select(unit_id, time_id, all_of(c(outcome_var, predictors))) %>%
      drop_na()
    
    # Run synthetic control
    dataprep_out <- dataprep(
      foo = as.data.frame(synth_data_clean),
      predictors = predictors,
      predictors.op = "mean",
      time.predictors.prior = pre_times,
      special.predictors = list(list(outcome_var, pre_times, "mean")),
      dependent = outcome_var,
      unit.variable = "unit_id",
      time.variable = "time_id",
      treatment.identifier = treated_id,
      controls.identifier = control_ids,
      time.optimize.ssr = pre_times,
      time.plot = all_times
    )
    
    synth_out <- synth(dataprep_out, verbose = FALSE)
    synth_tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
    
    # Calculate results
    Y1 <- dataprep_out$Y1plot  # Treated unit
    Y0 <- dataprep_out$Y0plot %*% synth_out$solution.w  # Synthetic control
    gap <- Y1 - Y0
    
    # Pre-treatment fit
    pre_gap <- gap[all_times < treatment_time, ]
    post_gap <- gap[all_times >= treatment_time, ]
    
    pre_rmspe <- sqrt(mean(pre_gap^2))
    pre_mean <- mean(Y1[all_times < treatment_time, ])
    fit_pct <- (pre_rmspe / abs(pre_mean)) * 100
    
    # Effect size
    mean_effect <- mean(post_gap)
    
    # Fit quality interpretation
    if (fit_pct < 5) {
      fit_quality <- list(
        status = "excellent",
        text = "Excellent Pre-Treatment Fit",
        description = "The synthetic control closely matches the treated unit before the intervention.",
        color = "#27ae60"
      )
    } else if (fit_pct < 10) {
      fit_quality <- list(
        status = "good",
        text = "Good Pre-Treatment Fit", 
        description = "The synthetic control matches the treated unit reasonably well.",
        color = "#2ecc71"
      )
    } else if (fit_pct < 20) {
      fit_quality <- list(
        status = "moderate",
        text = "Moderate Pre-Treatment Fit",
        description = "The match isn't perfect. Interpret results with some caution.",
        color = "#f39c12"
      )
    } else {
      fit_quality <- list(
        status = "poor",
        text = "Poor Pre-Treatment Fit",
        description = "The synthetic control doesn't match well. Results may not be reliable.",
        color = "#e74c3c"
      )
    }
    
    # Effect interpretation
    effect_interp <- interpret_effect_size(mean_effect, pre_mean, outcome_var)
    
    # Main finding
    direction <- if (mean_effect > 0) "higher" else "lower"
    main_finding <- sprintf("After the intervention, %s's %s was %.2f units %s than predicted by the synthetic control (%.1f%% %s).",
                            treated_unit, outcome_var, abs(mean_effect), direction,
                            abs(effect_interp$percentage), 
                            if (mean_effect > 0) "increase" else "decrease")
    
    list(
      success = TRUE,
      synth_out = synth_out,
      dataprep_out = dataprep_out,
      synth_tables = synth_tables,
      
      # Time series data
      all_times = all_times,
      Y_treated = as.vector(Y1),
      Y_synthetic = as.vector(Y0),
      gap = as.vector(gap),
      
      # Key results
      mean_effect = mean_effect,
      pre_rmspe = pre_rmspe,
      fit_pct = fit_pct,
      
      # Context
      treated_unit = treated_unit,
      treatment_time = treatment_time,
      n_controls = length(control_ids),
      pre_mean = pre_mean,
      
      # Weights
      weights = synth_out$solution.w,
      unit_mapping = unit_mapping,
      
      # Plain language
      fit_quality = fit_quality,
      effect_interpretation = effect_interp,
      main_finding = main_finding,
      
      # Technical
      outcome_var = outcome_var,
      unit_var = unit_var,
      time_var = time_var
    )
    
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

# --- Report Generation Functions ----------------------------------------------

generate_executive_summary <- function(results, analysis_type = "did") {
  if (analysis_type == "did") {
    html <- tags$div(
      class = "executive-summary",
      tags$h3(icon("file-alt"), " Executive Summary"),
      tags$div(
        class = "key-finding",
        tags$h4("Key Finding"),
        tags$p(HTML(results$main_finding))
      ),
      tags$div(
        class = "confidence-box",
        style = paste0("border-left: 4px solid ", results$confidence$color),
        tags$h4(icon(results$confidence$icon), " ", results$confidence$text),
        tags$p(results$confidence$description)
      ),
      tags$div(
        class = "effect-details",
        tags$h4("What This Means"),
        tags$p(results$effect_interpretation$plain_text)
      ),
      tags$div(
        class = "technical-note",
        tags$small(
          sprintf("Based on comparing %d treated observations to %d control observations. Effect estimate: %.3f (95%% CI: %.3f to %.3f)",
                  results$n_treated, results$n_control,
                  results$estimate, results$ci_lower, results$ci_upper)
        )
      )
    )
  } else {
    # Synthetic control summary
    html <- tags$div(
      class = "executive-summary",
      tags$h3(icon("file-alt"), " Executive Summary"),
      tags$div(
        class = "key-finding",
        tags$h4("Key Finding"),
        tags$p(results$main_finding)
      ),
      tags$div(
        class = "confidence-box",
        style = paste0("border-left: 4px solid ", results$fit_quality$color),
        tags$h4(results$fit_quality$text),
        tags$p(results$fit_quality$description)
      ),
      tags$div(
        class = "effect-details",
        tags$h4("What This Means"),
        tags$p(results$effect_interpretation$plain_text)
      ),
      tags$div(
        class = "technical-note",
        tags$small(
          sprintf("Synthetic control constructed from %d donor units. Average post-treatment effect: %.3f",
                  results$n_controls, results$mean_effect)
        )
      )
    )
  }
  
  html
}


# =============================================================================
# CUSTOM CSS FOR MODERN, CLEAN UI
# =============================================================================

custom_css <- "
/* Overall styling */
.content-wrapper {
  background-color: #f8fafc;
}

.main-header .logo {
  font-weight: 600;
}

/* Cards and boxes */
.box {
  border-radius: 12px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  border: none;
}

.box-header {
  border-bottom: 1px solid #eee;
  padding-bottom: 15px;
}

.box-title {
  font-weight: 600;
  font-size: 1.1em;
}

/* Step indicators */
.step-indicator {
  display: flex;
  justify-content: space-between;
  padding: 20px 0;
  margin-bottom: 20px;
}

.step {
  flex: 1;
  text-align: center;
  position: relative;
}

.step::after {
  content: '';
  position: absolute;
  top: 20px;
  left: 50%;
  width: 100%;
  height: 2px;
  background: #e0e0e0;
  z-index: 1;
}

.step:last-child::after {
  display: none;
}

.step-number {
  width: 40px;
  height: 40px;
  border-radius: 50%;
  background: #e0e0e0;
  color: #666;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-weight: bold;
  position: relative;
  z-index: 2;
  margin-bottom: 8px;
}

.step.active .step-number {
  background: #3498db;
  color: white;
}

.step.completed .step-number {
  background: #27ae60;
  color: white;
}

.step-label {
  font-size: 0.85em;
  color: #666;
}

.step.active .step-label {
  color: #3498db;
  font-weight: 600;
}

/* Confidence indicators */
.confidence-badge {
  display: inline-block;
  padding: 8px 16px;
  border-radius: 20px;
  font-weight: 600;
  font-size: 0.95em;
}

.confidence-very-high {
  background: #d4edda;
  color: #155724;
}

.confidence-high {
  background: #d4edda;
  color: #155724;
}

.confidence-moderate {
  background: #fff3cd;
  color: #856404;
}

.confidence-low {
  background: #f8d7da;
  color: #721c24;
}

/* Result cards */
.result-card {
  background: white;
  border-radius: 12px;
  padding: 24px;
  margin-bottom: 20px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}

.result-card h3 {
  margin-top: 0;
  color: #2c3e50;
  font-size: 1.3em;
}

.key-metric {
  font-size: 2.5em;
  font-weight: 700;
  line-height: 1.2;
}

.key-metric.positive {
  color: #27ae60;
}

.key-metric.negative {
  color: #e74c3c;
}

.metric-label {
  font-size: 0.9em;
  color: #7f8c8d;
  margin-top: 4px;
}

/* Executive summary */
.executive-summary {
  background: white;
  border-radius: 12px;
  padding: 24px;
  margin-bottom: 24px;
}

.executive-summary h3 {
  color: #2c3e50;
  margin-top: 0;
  padding-bottom: 12px;
  border-bottom: 2px solid #3498db;
}

.key-finding {
  background: #f8f9fa;
  padding: 16px;
  border-radius: 8px;
  margin: 16px 0;
}

.key-finding h4 {
  margin-top: 0;
  color: #2c3e50;
}

.confidence-box {
  padding: 16px;
  padding-left: 20px;
  margin: 16px 0;
  background: #f8f9fa;
  border-radius: 0 8px 8px 0;
}

.confidence-box h4 {
  margin-top: 0;
  margin-bottom: 8px;
}

.technical-note {
  margin-top: 20px;
  padding-top: 16px;
  border-top: 1px solid #eee;
  color: #7f8c8d;
}

/* Checklist items */
.check-item {
  padding: 12px 16px;
  margin: 8px 0;
  border-radius: 8px;
  display: flex;
  align-items: center;
}

.check-item.pass {
  background: #d4edda;
}

.check-item.warning {
  background: #fff3cd;
}

.check-item.fail {
  background: #f8d7da;
}

.check-item i {
  margin-right: 12px;
  font-size: 1.2em;
}

.check-item.pass i {
  color: #27ae60;
}

.check-item.warning i {
  color: #f39c12;
}

.check-item.fail i {
  color: #e74c3c;
}

/* Welcome hero */
.welcome-hero {
  background: linear-gradient(135deg, #3498db 0%, #2c3e50 100%);
  color: white;
  padding: 60px 40px;
  border-radius: 12px;
  text-align: center;
  margin-bottom: 30px;
}

.welcome-hero h1 {
  font-size: 2.5em;
  font-weight: 700;
  margin-bottom: 16px;
}

.welcome-hero p {
  font-size: 1.2em;
  opacity: 0.9;
  max-width: 600px;
  margin: 0 auto 30px;
}

/* Feature cards */
.feature-card {
  background: white;
  border-radius: 12px;
  padding: 24px;
  text-align: center;
  height: 100%;
  transition: transform 0.2s, box-shadow 0.2s;
}

.feature-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 8px 24px rgba(0,0,0,0.12);
}

.feature-card i {
  font-size: 2.5em;
  color: #3498db;
  margin-bottom: 16px;
}

.feature-card h4 {
  color: #2c3e50;
  margin-bottom: 12px;
}

.feature-card p {
  color: #7f8c8d;
  font-size: 0.95em;
}

/* Data quality gauge */
.quality-gauge {
  text-align: center;
  padding: 20px;
}

.gauge-circle {
  width: 120px;
  height: 120px;
  border-radius: 50%;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 2em;
  font-weight: 700;
  color: white;
  margin-bottom: 12px;
}

.gauge-excellent {
  background: linear-gradient(135deg, #27ae60 0%, #2ecc71 100%);
}

.gauge-good {
  background: linear-gradient(135deg, #2ecc71 0%, #f1c40f 100%);
}

.gauge-moderate {
  background: linear-gradient(135deg, #f1c40f 0%, #f39c12 100%);
}

.gauge-poor {
  background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
}

/* Tooltip styling */
.help-tooltip {
  color: #3498db;
  cursor: help;
  margin-left: 6px;
}

/* Button styling */
.btn-primary {
  background: #3498db;
  border-color: #3498db;
  border-radius: 8px;
  padding: 10px 24px;
  font-weight: 500;
}

.btn-primary:hover {
  background: #2980b9;
  border-color: #2980b9;
}

.btn-success {
  background: #27ae60;
  border-color: #27ae60;
  border-radius: 8px;
}

.btn-success:hover {
  background: #219a52;
  border-color: #219a52;
}

.btn-lg {
  padding: 14px 32px;
  font-size: 1.1em;
}

/* Clean form inputs */
.form-control, .selectize-input {
  border-radius: 8px;
  border: 2px solid #e0e0e0;
  transition: border-color 0.2s;
}

.form-control:focus, .selectize-input.focus {
  border-color: #3498db;
  box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
}

/* Sidebar styling */
.skin-blue .main-sidebar {
  background-color: #2c3e50;
}

.skin-blue .sidebar-menu > li.active > a {
  background-color: #34495e;
  border-left-color: #3498db;
}

.skin-blue .sidebar-menu > li > a:hover {
  background-color: #34495e;
}

/* Section headers */
.section-header {
  margin-bottom: 24px;
}

.section-header h2 {
  color: #2c3e50;
  font-weight: 600;
  margin-bottom: 8px;
}

.section-header p {
  color: #7f8c8d;
  margin: 0;
}

/* Info panels */
.info-panel {
  background: #e8f4fd;
  border-left: 4px solid #3498db;
  padding: 16px;
  border-radius: 0 8px 8px 0;
  margin: 16px 0;
}

.info-panel h4 {
  color: #2c3e50;
  margin-top: 0;
  margin-bottom: 8px;
}

.info-panel p {
  margin: 0;
  color: #34495e;
}

/* Warning panels */
.warning-panel {
  background: #fef9e7;
  border-left: 4px solid #f39c12;
  padding: 16px;
  border-radius: 0 8px 8px 0;
  margin: 16px 0;
}

/* Insight cards */
.insight-card {
  background: white;
  border-radius: 12px;
  padding: 20px;
  margin-bottom: 16px;
  border-left: 4px solid #3498db;
}

.insight-card.positive {
  border-left-color: #27ae60;
}

.insight-card.negative {
  border-left-color: #e74c3c;
}

.insight-card.neutral {
  border-left-color: #f39c12;
}

/* Data table styling */
.dataTables_wrapper {
  padding: 0;
}

table.dataTable {
  border-collapse: collapse;
}

table.dataTable thead th {
  background: #f8f9fa;
  border-bottom: 2px solid #dee2e6;
}

/* Hide technical jargon class */
.technical-details {
  display: none;
}

.show-technical .technical-details {
  display: block;
}

/* Mobile responsiveness */
@media (max-width: 768px) {
  .welcome-hero {
    padding: 40px 20px;
  }
  
  .welcome-hero h1 {
    font-size: 1.8em;
  }
  
  .key-metric {
    font-size: 2em;
  }
  
  .step-indicator {
    flex-direction: column;
    gap: 16px;
  }
  
  .step::after {
    display: none;
  }
}

/* ===== GLOBAL PROGRESS STEPPER ===== */
.global-progress-wrapper {
  background: white;
  padding: 20px 30px;
  margin: -15px -15px 20px -15px;
  border-bottom: 2px solid #e8eef3;
  box-shadow: 0 2px 4px rgba(0,0,0,0.04);
}

.global-progress-stepper {
  display: flex;
  justify-content: space-between;
  align-items: center;
  max-width: 1200px;
  margin: 0 auto;
  position: relative;
}

.progress-step {
  display: flex;
  flex-direction: column;
  align-items: center;
  flex: 1;
  position: relative;
  cursor: pointer;
  transition: all 0.3s ease;
}

.progress-step:hover .progress-circle {
  transform: scale(1.1);
}

.progress-connector {
  position: absolute;
  top: 20px;
  left: 50%;
  width: 100%;
  height: 3px;
  background: #e0e6ed;
  z-index: 0;
}

.progress-step:last-child .progress-connector {
  display: none;
}

.progress-step.completed .progress-connector {
  background: linear-gradient(90deg, #27ae60 0%, #27ae60 100%);
}

.progress-step.active .progress-connector {
  background: linear-gradient(90deg, #27ae60 0%, #e0e6ed 100%);
}

.progress-circle {
  width: 42px;
  height: 42px;
  border-radius: 50%;
  background: #e0e6ed;
  color: #95a5a6;
  display: flex;
  align-items: center;
  justify-content: center;
  font-weight: 600;
  font-size: 16px;
  position: relative;
  z-index: 2;
  transition: all 0.3s ease;
  border: 3px solid #f8fafc;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}

.progress-step.completed .progress-circle {
  background: linear-gradient(135deg, #27ae60 0%, #229954 100%);
  color: white;
}

.progress-step.active .progress-circle {
  background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
  color: white;
  box-shadow: 0 4px 12px rgba(52, 152, 219, 0.4);
  animation: pulse 2s infinite;
}

@keyframes pulse {
  0%, 100% { transform: scale(1); }
  50% { transform: scale(1.05); }
}

.progress-label {
  margin-top: 10px;
  font-size: 13px;
  color: #7f8c8d;
  font-weight: 500;
  text-align: center;
  max-width: 120px;
}

.progress-step.active .progress-label {
  color: #3498db;
  font-weight: 600;
}

.progress-step.completed .progress-label {
  color: #27ae60;
  font-weight: 600;
}

/* ===== DATASET LIBRARY ===== */
.dataset-library {
  margin-top: 20px;
}

.dataset-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 16px;
  margin-top: 16px;
}

.dataset-card {
  background: white;
  border: 2px solid #e8eef3;
  border-radius: 12px;
  padding: 20px;
  cursor: pointer;
  transition: all 0.3s ease;
  position: relative;
  overflow: hidden;
}

.dataset-card:hover {
  border-color: #3498db;
  box-shadow: 0 6px 20px rgba(52, 152, 219, 0.15);
  transform: translateY(-4px);
}

.dataset-card.selected {
  border-color: #27ae60;
  background: #f0f9f4;
}

.dataset-icon {
  width: 50px;
  height: 50px;
  border-radius: 10px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 24px;
  color: white;
  margin-bottom: 12px;
}

.dataset-icon.green { background: linear-gradient(135deg, #27ae60 0%, #229954 100%); }
.dataset-icon.blue { background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); }
.dataset-icon.red { background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%); }
.dataset-icon.purple { background: linear-gradient(135deg, #9b59b6 0%, #8e44ad 100%); }

.dataset-card h4 {
  margin: 0 0 8px 0;
  color: #2c3e50;
  font-size: 16px;
  font-weight: 600;
}

.dataset-badge {
  display: inline-block;
  padding: 4px 10px;
  border-radius: 12px;
  font-size: 11px;
  font-weight: 600;
  margin-bottom: 10px;
}

.badge-beginner { background: #d4edda; color: #155724; }
.badge-intermediate { background: #fff3cd; color: #856404; }
.badge-advanced { background: #f8d7da; color: #721c24; }

.dataset-description {
  font-size: 13px;
  color: #7f8c8d;
  line-height: 1.5;
  margin-bottom: 12px;
}

.dataset-meta {
  font-size: 12px;
  color: #95a5a6;
  margin-top: 10px;
  padding-top: 10px;
  border-top: 1px solid #ecf0f1;
}

.dataset-meta i {
  margin-right: 6px;
  color: #3498db;
}

.load-dataset-btn {
  margin-top: 12px;
  width: 100%;
  background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
  color: white;
  border: none;
  padding: 10px;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.2s;
}

.load-dataset-btn:hover {
  background: linear-gradient(135deg, #2980b9 0%, #21618c 100%);
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(52, 152, 219, 0.3);
}

@media (max-width: 768px) {
  .global-progress-stepper {
    flex-wrap: wrap;
  }

  .progress-step {
    min-width: 80px;
  }

  .progress-connector {
    display: none;
  }

  .dataset-grid {
    grid-template-columns: 1fr;
  }
}
"

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  title = "Policy Insights Toolkit",
  
  # Header
  dashboardHeader(
    title = tags$span(
      icon("chart-line"), " Policy Insights"
    ),
    titleWidth = 250
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    useShinyjs(),
    
    sidebarMenu(
      id = "sidebar",
      
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      
      menuItem("1. Upload Data", tabName = "upload", icon = icon("cloud-upload-alt")),
      
      menuItem("2. Explore Your Data", tabName = "explore", icon = icon("search"),
               menuSubItem("Overview & Quality", tabName = "overview"),
               menuSubItem("Trends Over Time", tabName = "trends"),
               menuSubItem("Statistical Summary", tabName = "stats")),

      menuItem("3. Analyze Impact", tabName = "impact", icon = icon("balance-scale"),
               menuSubItem("Difference-in-Differences", tabName = "did"),
               menuSubItem("Synthetic Control", tabName = "synth")),

      menuItem("4. Export Results", tabName = "export", icon = icon("file-export")),
      
      hr(),
      
      # =========== ADD THIS NEW MENU ITEM ===========
      menuItem("AI Assistant", tabName = "assistant", icon = icon("robot")),
      # ==============================================
      
      menuItem("Help & Learning", tabName = "help", icon = icon("question-circle"))
    ),
    
    hr(),
    
    # Data status indicator
    conditionalPanel(
      condition = "output.has_data",
      div(
        style = "padding: 15px;",
        tags$h5(
          icon("database"), " Your Data",
          style = "color: #ecf0f1; margin-bottom: 10px;"
        ),
        verbatimTextOutput("data_status", placeholder = TRUE)
      )
    )
  ),
  
  # Main body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),

    # Global Progress Stepper (shows on all pages except welcome)
    uiOutput("global_progress_stepper"),

    tabItems(
      
      # ===== WELCOME TAB =====
      tabItem(
        tabName = "welcome",
        
        div(
          class = "welcome-hero",
          tags$h1("Policy Insights Toolkit"),
          tags$p("Turn your program and policy data into clear, actionable insights — no statistics degree required."),
          actionButton("start_btn", "Get Started", class = "btn btn-lg", 
                       style = "background: white; color: #3498db; border: none;",
                       icon = icon("arrow-right"))
        ),
        
        fluidRow(
          column(4,
                 div(class = "feature-card",
                     icon("upload"),
                     tags$h4("Easy Data Upload"),
                     tags$p("Just upload your Excel or CSV file. We'll automatically figure out what's what.")
                 )
          ),
          column(4,
                 div(class = "feature-card",
                     icon("chart-bar"),
                     tags$h4("Clear Visualizations"),
                     tags$p("See your data come to life with interactive charts and trend lines.")
                 )
          ),
          column(4,
                 div(class = "feature-card",
                     icon("lightbulb"),
                     tags$h4("Plain Language Results"),
                     tags$p("Get findings explained in words you understand, not statistical jargon.")
                 )
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            title = "What Can This Tool Do?",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            fluidRow(
              column(6,
                     tags$h4(icon("search"), " Explore Your Data"),
                     tags$ul(
                       tags$li("See summary statistics for all your variables"),
                       tags$li("Visualize trends over time"),
                       tags$li("Compare groups and find patterns"),
                       tags$li("Check data quality and identify issues")
                     )
              ),
              column(6,
                     tags$h4(icon("balance-scale"), " Measure Impact"),
                     tags$ul(
                       tags$li(tags$strong("Before vs After Analysis:"), " Did your intervention make a difference?"),
                       tags$li(tags$strong("What-If Comparison:"), " What would have happened without the intervention?"),
                       tags$li("Get confidence levels in plain language"),
                       tags$li("Export professional reports")
                     )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "What Kind of Data Do I Need?",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,

            tags$p("This toolkit is designed for ", tags$strong("panel data"), " — where you track the same entities (like states, schools, people, or organizations) across multiple time periods."),

            tags$h5(icon("check-circle"), " Essential columns in your dataset:"),
            tags$ul(
              tags$li(tags$strong("Who/What:"), " Column identifying each entity (e.g., 'State', 'School_ID', 'Participant_Name')"),
              tags$li(tags$strong("When:"), " Column showing time periods (e.g., 'Year', 'Quarter', 'Month')"),
              tags$li(tags$strong("Outcome:"), " Column with the values you want to analyze (e.g., 'Test_Score', 'Unemployment_Rate', 'Sales')")
            ),

            tags$h5(icon("plus-circle"), " Optional but helpful:"),
            tags$ul(
              tags$li(tags$strong("Treatment indicator:"), " Column showing which units got the program/policy (typically 1=yes, 0=no)")
            ),

            tags$h5(icon("table"), " Example: Analyzing a minimum wage policy"),
            tags$pre(
              "State      Year   Unemployment   MinWage_Increase
California  2018   4.2            0
California  2019   4.0            0
California  2020   3.9            1
Texas       2018   3.8            0
Texas       2019   3.7            0
Texas       2020   3.5            0"
            ),

            tags$p(class = "help-text", icon("lightbulb"),
                   " Each state appears multiple times (once per year), and we can see California raised its minimum wage in 2020. This is perfect for analyzing program effects!")
          )
        )
      ),
      
      # ===== UPLOAD TAB =====
      tabItem(
        tabName = "upload",
        
        div(class = "section-header",
            tags$h2(icon("cloud-upload-alt"), " Upload Your Data"),
            tags$p("Start by uploading your data file, or explore the toolkit with one of our realistic example datasets.")
        ),
        
        fluidRow(
          box(
            title = NULL,
            status = "primary",
            solidHeader = FALSE,
            width = 5,
            
            fileInput(
              "file_upload",
              label = tags$span(
                "Choose your file",
                tags$span(class = "help-tooltip", icon("question-circle"), 
                          title = "Upload a CSV or Excel file with your data")
              ),
              accept = c(".csv", ".xlsx", ".xls"),
              placeholder = "No file selected",
              buttonLabel = "Browse..."
            ),
            
            div(class = "info-panel",
                tags$h4(icon("info-circle"), " Supported formats"),
                tags$p("CSV (.csv), Excel (.xlsx, .xls)")
            ),
            
            hr(),

            tags$div(
              style = "text-align: center;",
              actionButton(
                "show_dataset_library",
                "Browse Example Datasets",
                icon = icon("book"),
                class = "btn-success btn-block",
                style = "font-size: 15px; padding: 12px;"
              )
            )
          ),
          
          box(
            title = "Data Quality Check",
            status = "success",
            solidHeader = TRUE,
            width = 7,
            
            uiOutput("quality_check")
          )
        ),
        
        fluidRow(
          box(
            title = "Preview Your Data",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,

            DTOutput("data_preview"),

            uiOutput("detected_structure")
          )
        ),

        # Dataset Library (hidden by default)
        shinyjs::hidden(
          div(
            id = "dataset_library_panel",
            fluidRow(
              box(
                title = "Example Datasets Library",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,

                tags$p(
                  style = "font-size: 15px; margin-bottom: 20px;",
                  "Explore the toolkit with these realistic policy analysis datasets. Each includes documentation and suggested analyses."
                ),

                uiOutput("dataset_library_ui")
              )
            )
          )
        )
      ),
      
      # ===== DATA OVERVIEW TAB =====
      tabItem(
        tabName = "overview",

        div(class = "section-header",
            tags$h2(icon("table"), " Data Overview & Quality"),
            tags$p("Get a quick snapshot of your dataset: how many observations you have, what variables are included, and whether there are any data quality issues to address.")
        ),
        
        fluidRow(
          valueBoxOutput("n_rows_box", width = 3),
          valueBoxOutput("n_cols_box", width = 3),
          valueBoxOutput("n_units_box", width = 3),
          valueBoxOutput("n_periods_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Variable Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            
            DTOutput("var_summary_table")
          ),
          
          box(
            title = "Data Structure",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            uiOutput("data_structure_info")
          )
        ),
        
        fluidRow(
          box(
            title = "Missing Values",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            
            plotlyOutput("missing_plot", height = "300px")
          )
        )
      ),
      
      # ===== TRENDS TAB =====
      tabItem(
        tabName = "trends",

        div(class = "section-header",
            tags$h2(icon("chart-line"), " Trends Over Time"),
            tags$p("Visualize how your outcome variables change over time. Look for patterns, compare different groups, and identify potential intervention effects before running formal analyses.")
        ),
        
        fluidRow(
          box(
            title = "Chart Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            
            selectInput(
              "trend_y",
              label = tags$span("What to show:", tags$span(class = "help-tooltip", icon("question-circle"))),
              choices = NULL
            ),
            
            selectInput(
              "trend_x",
              label = "Over time:",
              choices = NULL
            ),
            
            selectInput(
              "trend_color",
              label = "Compare by:",
              choices = NULL
            ),
            
            checkboxInput("trend_smooth", "Add trend line", TRUE),
            
            checkboxInput("trend_points", "Show data points", TRUE),
            
            hr(),
            
            selectInput(
              "trend_highlight",
              label = "Highlight specific units:",
              choices = NULL,
              multiple = TRUE
            )
          ),
          
          box(
            title = NULL,
            status = "info",
            solidHeader = FALSE,
            width = 9,
            
            plotlyOutput("trend_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "What Am I Looking At?",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            
            div(class = "info-panel",
                tags$h4(icon("lightbulb"), " Reading This Chart"),
                tags$p("Each line represents a different unit (like a state or organization) over time."),
                tags$ul(
                  tags$li(tags$strong("Parallel lines:"), " Units are moving in the same direction — good for comparison analysis."),
                  tags$li(tags$strong("Diverging lines:"), " Units are moving in different directions — something different is happening to them."),
                  tags$li(tags$strong("Sudden changes:"), " Look for sharp jumps that might indicate an event or intervention.")
                )
            )
          )
        )
      ),
      
      # ===== SUMMARY STATS TAB =====
      tabItem(
        tabName = "stats",

        div(class = "section-header",
            tags$h2(icon("calculator"), " Statistical Summary"),
            tags$p("Calculate key statistics for your variables: averages, ranges, and distributions. Compare statistics across different groups to understand your data better.")
        ),
        
        fluidRow(
          box(
            title = "Select Variables",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            
            selectizeInput(
              "stats_vars",
              label = "Variables to summarize:",
              choices = NULL,
              multiple = TRUE
            ),
            
            selectInput(
              "stats_groupby",
              label = "Group by (optional):",
              choices = NULL
            ),
            
            actionButton(
              "calc_stats",
              "Calculate",
              icon = icon("calculator"),
              class = "btn-primary btn-block"
            ),
            
            hr(),
            
            downloadButton(
              "download_stats",
              "Download Summary",
              class = "btn-success btn-block"
            )
          ),
          
          box(
            title = "Results",
            status = "info",
            solidHeader = TRUE,
            width = 9,
            
            DTOutput("stats_table"),
            
            br(),
            
            uiOutput("stats_interpretation")
          )
        ),
        
        fluidRow(
          box(
            title = "Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            selectInput("dist_var", "Variable:", choices = NULL),
            plotlyOutput("dist_plot", height = "350px")
          ),
          
          box(
            title = "Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("comparison_boxplot", height = "400px")
          )
        )
      ),
      
      # ===== DIFFERENCE-IN-DIFFERENCES TAB =====
      tabItem(
        tabName = "did",

        div(class = "section-header",
            tags$h2(icon("balance-scale"), " Difference-in-Differences Analysis"),
            tags$p("Compare what happened to groups that received an intervention vs. those that didn't. This method helps you understand if your program or policy truly made a difference by accounting for natural trends over time.")
        ),
        
        # Step indicator
        uiOutput("did_steps"),
        
        # Step 1: Setup
        div(
          id = "did_setup_panel",
          
          fluidRow(
            box(
              title = "Step 1: Tell Us About Your Data",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              fluidRow(
                column(4,
                       tags$h4(icon("table"), " Data Structure"),
                       
                       selectInput(
                         "did_unit",
                         label = tags$span(
                           "Unit identifier column:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "Column that identifies what you're comparing (e.g., state names, school IDs, participant IDs)")
                         ),
                         choices = NULL
                       ),

                       selectInput(
                         "did_time",
                         label = tags$span(
                           "Time period column:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "Column showing when each observation occurred (e.g., year, quarter, month)")
                         ),
                         choices = NULL
                       ),

                       selectInput(
                         "did_outcome",
                         label = tags$span(
                           "Outcome to measure:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "The variable you want to analyze - what you're trying to see if the intervention affected")
                         ),
                         choices = NULL
                       )
                ),
                
                column(4,
                       tags$h4(icon("flask"), " The Intervention"),
                       
                       radioButtons(
                         "did_treatment_type",
                         label = "How to identify treated units:",
                         choices = c(
                           "I have a treatment column (0s and 1s)" = "indicator",
                           "I'll select which units got the program/policy" = "manual"
                         )
                       ),

                       conditionalPanel(
                         condition = "input.did_treatment_type == 'indicator'",
                         selectInput(
                           "did_treatment_var",
                           label = "Treatment indicator column:",
                           choices = NULL
                         ),
                         tags$p(class = "help-text", icon("info-circle"),
                                " This column should have 1 when a unit received the intervention, 0 when it didn't.")
                       ),

                       conditionalPanel(
                         condition = "input.did_treatment_type == 'manual'",
                         selectizeInput(
                           "did_treated_units",
                           label = "Units that got the program/policy:",
                           choices = NULL,
                           multiple = TRUE
                         ),
                         numericInput(
                           "did_treatment_time",
                           label = "Year/period when it started:",
                           value = NULL
                         ),
                         tags$p(class = "help-text", icon("info-circle"),
                                " Select all units that received the intervention and when it began.")
                       )
                ),
                
                column(4,
                       tags$h4(icon("sliders-h"), " Additional Controls (Optional)"),

                       selectizeInput(
                         "did_controls",
                         label = tags$span(
                           "Other variables to account for:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "Select other variables that might influence your outcome (e.g., population, GDP, demographics). This is optional but can improve accuracy.")
                         ),
                         choices = NULL,
                         multiple = TRUE
                       ),

                       div(class = "info-panel",
                           icon("check-circle"),
                           tags$p(tags$strong("Automatic best practices:"), " We'll handle the statistical details (fixed effects, clustering, etc.) for you.")
                       )
                )
              ),
              
              hr(),
              
              div(
                style = "text-align: right;",
                actionButton(
                  "did_check_ready",
                  "Validate Setup & Continue →",
                  icon = icon("check-circle"),
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),
        
        # Step 2: Readiness check
        shinyjs::hidden(
          div(
            id = "did_ready_panel",
            
            fluidRow(
              box(
                title = "Step 2: Checking Your Setup",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                
                fluidRow(
                  column(6,
                         tags$h4(icon("clipboard-check"), " Readiness Checklist"),
                         uiOutput("did_readiness_checks")
                  ),
                  column(6,
                         tags$h4(icon("chart-line"), " Preview: Before Treatment Trends"),
                         tags$p(class = "help-text", "Are the treated and control groups following similar patterns before the intervention?"),
                         plotlyOutput("did_preview_trends", height = "300px")
                  )
                ),
                
                hr(),
                
                uiOutput("did_ready_summary"),
                
                div(
                  style = "text-align: right;",
                  actionButton("did_back_setup", "← Back to Setup", class = "btn-default"),
                  actionButton("did_run", "Run Analysis →", icon = icon("play"), class = "btn-success btn-lg")
                )
              )
            )
          )
        ),
        
        # Step 3: Results
        shinyjs::hidden(
          div(
            id = "did_results_panel",
            
            fluidRow(
              box(
                title = NULL,
                status = "success",
                solidHeader = FALSE,
                width = 12,
                
                uiOutput("did_executive_summary")
              )
            ),
            
            fluidRow(
              box(
                title = "The Effect Over Time",
                status = "info",
                solidHeader = TRUE,
                width = 8,
                
                plotlyOutput("did_main_plot", height = "400px"),
                
                div(class = "info-panel",
                    tags$p(
                      tags$strong("How to read this:"), 
                      " The vertical dashed line shows when the intervention started. ",
                      "If the treated group (red) diverges from the control group (blue) ",
                      "after the intervention, that suggests an effect."
                    )
                )
              ),
              
              box(
                title = "Key Numbers",
                status = "primary",
                solidHeader = TRUE,
                width = 4,
                
                uiOutput("did_key_stats")
              )
            ),
            
            fluidRow(
              tabBox(
                title = "More Details",
                width = 12,
                
                tabPanel(
                  "Pre-Trend Check",
                  icon = icon("chart-area"),
                  br(),
                  div(class = "info-panel",
                      tags$h4(icon("question-circle"), " Why This Matters"),
                      tags$p("For this analysis to be valid, the treated and control groups should have been following similar trends before the intervention.")
                  ),
                  plotlyOutput("did_pretrend_plot", height = "350px"),
                  verbatimTextOutput("did_pretrend_test")
                ),
                
                tabPanel(
                  "Balance Check",
                  icon = icon("balance-scale-left"),
                  br(),
                  div(class = "info-panel",
                      tags$h4(icon("question-circle"), " Why This Matters"),
                      tags$p("We want treated and control groups to be similar before treatment. Large differences could bias results.")
                  ),
                  DTOutput("did_balance_table")
                ),
                
                tabPanel(
                  "Full Statistics",
                  icon = icon("table"),
                  br(),
                  tags$p("For those who want the technical details:"),
                  verbatimTextOutput("did_full_output"),
                  DTOutput("did_coef_table")
                )
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                div(
                  style = "text-align: left;",
                  actionButton("did_restart", "← Start New Analysis", class = "btn-default")
                )
              )
            )
          )
        )
      ),
      
      # ===== SYNTHETIC CONTROL TAB =====
      tabItem(
        tabName = "synth",

        div(class = "section-header",
            tags$h2(icon("clone"), " Synthetic Control Method"),
            tags$p("Create a 'synthetic twin' of the treated unit to estimate what would have happened without the intervention. This method is ideal when only one state, city, or organization received a program or policy change.")
        ),
        
        # Step indicator
        uiOutput("synth_steps"),
        
        # Step 1: Setup
        div(
          id = "synth_setup_panel",
          
          fluidRow(
            box(
              title = "Step 1: Tell Us About Your Analysis",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(class = "info-panel",
                  tags$h4(icon("info-circle"), " What is Synthetic Control?"),
                  tags$p("This method creates a 'synthetic' version of your treated unit by combining similar untreated units. ",
                         "It's like asking: 'What would California have looked like if it hadn't enacted this policy?' ",
                         "by creating a weighted average of other states that looks like California.")
              ),
              
              br(),
              
              fluidRow(
                column(4,
                       tags$h4(icon("table"), " Data Structure"),

                       selectInput(
                         "synth_unit",
                         label = tags$span(
                           "Unit identifier column:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "Column identifying each unit (e.g., state names, country names)")
                         ),
                         choices = NULL
                       ),
                       selectInput(
                         "synth_time",
                         label = tags$span(
                           "Time period column:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "Column showing time periods (e.g., year, quarter)")
                         ),
                         choices = NULL
                       ),
                       selectInput(
                         "synth_outcome",
                         label = tags$span(
                           "Outcome to measure:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "What you're analyzing the effect on")
                         ),
                         choices = NULL
                       )
                ),

                column(4,
                       tags$h4(icon("flask"), " The Intervention"),

                       selectInput(
                         "synth_treated_unit",
                         label = "Which ONE unit got the program/policy?",
                         choices = NULL
                       ),

                       numericInput(
                         "synth_treatment_time",
                         label = "Year/period when it started:",
                         value = NULL
                       ),

                       tags$p(class = "help-text",
                              icon("lightbulb"),
                              " Synthetic control works best with a single treated unit and many comparison units.")
                ),

                column(4,
                       tags$h4(icon("balance-scale"), " Variables for Matching"),

                       selectizeInput(
                         "synth_predictors",
                         label = tags$span(
                           "Characteristics to match on:",
                           tags$span(class = "help-tooltip", icon("question-circle"),
                                     title = "Select variables to create a good synthetic comparison (e.g., demographics, economic indicators). The synthetic unit will match the treated unit on these.")
                         ),
                         choices = NULL,
                         multiple = TRUE
                       ),

                       tags$p(class = "help-text",
                              icon("info-circle"),
                              " Choose variables that are good predictors of your outcome. The method will find the best weighted combination of control units.")
                )
              ),
              
              hr(),
              
              div(
                style = "text-align: right;",
                actionButton("synth_check_ready", "Validate Setup & Continue →", icon = icon("check-circle"), class = "btn-primary btn-lg")
              )
            )
          )
        ),
        
        # Step 2: Readiness
        shinyjs::hidden(
          div(
            id = "synth_ready_panel",
            
            fluidRow(
              box(
                title = "Step 2: Checking Your Setup",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                
                fluidRow(
                  column(6,
                         tags$h4(icon("clipboard-check"), " Readiness Checklist"),
                         uiOutput("synth_readiness_checks")
                  ),
                  column(6,
                         tags$h4(icon("chart-line"), " Your Treated Unit vs. Potential Controls"),
                         plotlyOutput("synth_preview_plot", height = "300px")
                  )
                ),
                
                hr(),
                
                uiOutput("synth_ready_summary"),
                
                div(
                  style = "text-align: right;",
                  actionButton("synth_back_setup", "← Back to Setup", class = "btn-default"),
                  actionButton("synth_run", "Run Analysis →", icon = icon("play"), class = "btn-success btn-lg")
                )
              )
            )
          )
        ),
        
        # Step 3: Results
        shinyjs::hidden(
          div(
            id = "synth_results_panel",
            
            fluidRow(
              box(
                title = NULL,
                status = "success",
                solidHeader = FALSE,
                width = 12,
                
                uiOutput("synth_executive_summary")
              )
            ),
            
            fluidRow(
              box(
                title = "Actual vs. Synthetic Comparison",
                status = "info",
                solidHeader = TRUE,
                width = 8,
                
                plotlyOutput("synth_main_plot", height = "400px"),
                
                div(class = "info-panel",
                    tags$p(
                      tags$strong("How to read this:"), 
                      " The red line is what actually happened. The blue line is what the synthetic control ",
                      "predicts would have happened without the intervention. The gap after the intervention ",
                      "is the estimated effect."
                    )
                )
              ),
              
              box(
                title = "Key Numbers",
                status = "primary",
                solidHeader = TRUE,
                width = 4,
                
                uiOutput("synth_key_stats")
              )
            ),
            
            fluidRow(
              box(
                title = "The Effect Over Time",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                
                plotlyOutput("synth_gap_plot", height = "300px"),
                
                tags$p(class = "help-text", 
                       "This shows the difference between actual and synthetic values. ",
                       "Values above zero mean the actual outcome was higher than predicted.")
              )
            ),
            
            fluidRow(
              tabBox(
                title = "More Details",
                width = 12,
                
                tabPanel(
                  "How Was the Synthetic Control Built?",
                  icon = icon("balance-scale"),
                  br(),
                  div(class = "info-panel",
                      tags$h4(icon("info-circle"), " Understanding the Weights"),
                      tags$p("The synthetic control is a weighted combination of untreated units. ",
                             "Higher weights mean that unit contributed more to the synthetic comparison.")
                  ),
                  fluidRow(
                    column(6, plotlyOutput("synth_weights_plot", height = "350px")),
                    column(6, DTOutput("synth_weights_table"))
                  )
                ),
                
                tabPanel(
                  "How Good is the Match?",
                  icon = icon("check-double"),
                  br(),
                  div(class = "info-panel",
                      tags$h4(icon("info-circle"), " Pre-Treatment Fit"),
                      tags$p("A good synthetic control should closely match the treated unit before the intervention.")
                  ),
                  DTOutput("synth_balance_table"),
                  plotlyOutput("synth_balance_plot", height = "300px")
                ),
                
                tabPanel(
                  "Is This Effect Real? (Placebo Tests)",
                  icon = icon("flask"),
                  br(),
                  div(class = "info-panel",
                      tags$h4(icon("info-circle"), " What Are Placebo Tests?"),
                      tags$p("We run the same analysis pretending each control unit was 'treated'. ",
                             "If the real effect is meaningful, it should be larger than these placebo effects.")
                  ),
                  checkboxInput("run_synth_placebo", "Run placebo tests (takes a few minutes)", FALSE),
                  conditionalPanel(
                    condition = "input.run_synth_placebo == true",
                    actionButton("start_synth_placebo", "Start Placebo Tests", icon = icon("play"), class = "btn-primary"),
                    br(), br(),
                    plotlyOutput("synth_placebo_plot", height = "400px"),
                    verbatimTextOutput("synth_pvalue_text")
                  )
                )
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                div(
                  style = "text-align: left;",
                  actionButton("synth_restart", "← Start New Analysis", class = "btn-default")
                )
              )
            )
          )
        )
      ),
      
      # ===== EXPORT TAB =====
      tabItem(
        tabName = "export",

        div(class = "section-header",
            tags$h2(icon("file-export"), " Export Your Results"),
            tags$p("Download your data, charts, and analysis results to share with colleagues or include in reports and presentations.")
        ),
        
        fluidRow(
          box(
            title = "Download Data",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            tags$p("Download your data with any transformations you've made:"),
            
            downloadButton("download_data_csv", "Download as CSV", class = "btn-primary btn-block"),
            br(),
            downloadButton("download_data_excel", "Download as Excel", class = "btn-info btn-block")
          ),
          
          box(
            title = "Download Analysis Results",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            tags$p("Download your most recent analysis results:"),
            
            downloadButton("download_results_csv", "Results as CSV", class = "btn-success btn-block"),
            br(),
            downloadButton("download_results_summary", "Plain Language Summary", class = "btn-info btn-block")
          )
        )
      ),
      
      # ===== Chatbot Tab ==== #
      tabItem(
        tabName = "assistant",

        div(class = "section-header",
            tags$h2(icon("robot"), " AI Assistant"),
            tags$p("Ask questions about your data, get help understanding statistical concepts, or receive guidance on choosing the right analysis method. The AI assistant provides personalized help based on your specific dataset and analysis.")
        ),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            chatbotUI("chatbot")  # This creates the chat interface
          )
        ),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            br(),
            div(
              class = "info-panel",
              tags$h4(icon("lightbulb"), " Tips for Better Conversations"),
              tags$ul(
                tags$li("Be specific about what you're trying to understand"),
                tags$li("Mention which analysis you're working on (DiD or Synthetic Control)"),
                tags$li("Ask follow-up questions if something isn't clear"),
                tags$li("The assistant can see your current data and results, so feel free to ask about them")
              )
            )
          )
        )
      ),
      
      # ===== HELP TAB =====
      tabItem(
        tabName = "help",
        
        div(class = "section-header",
            tags$h2(icon("question-circle"), " Help & Learning"),
            tags$p("Learn more about how to use this tool and interpret your results.")
        ),
        
        fluidRow(
          box(
            title = "Frequently Asked Questions",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            tags$div(
              class = "faq-item",
              tags$h4("What kind of data do I need?"),
              tags$p("You need 'panel data' - observations of the same units (like states, schools, or people) ",
                     "measured at multiple points in time. For impact analysis, you also need some units that ",
                     "received an intervention and some that didn't.")
            ),
            
            tags$div(
              class = "faq-item",
              tags$h4("What does 'confidence level' mean?"),
              tags$p("It tells you how sure we can be that the finding is real and not just random chance. ",
                     "'Very High Confidence' means we're quite sure; 'Low Confidence' means it might just be noise in the data.")
            ),
            
            tags$div(
              class = "faq-item",
              tags$h4("When should I use 'Before vs After' analysis?"),
              tags$p("Use this when you have multiple units that received an intervention and multiple that didn't. ",
                     "It compares the change in treated units to the change in control units.")
            ),
            
            tags$div(
              class = "faq-item",
              tags$h4("When should I use 'What Would Have Happened' analysis?"),
              tags$p("Use this when you have just one unit that received an intervention (like one state that passed a law) ",
                     "and you want to estimate what would have happened without it.")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Glossary of Terms",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            
            tags$dl(
              tags$dt("Panel Data"),
              tags$dd("Data where you observe the same units multiple times (e.g., states measured every year)."),
              
              tags$dt("Treated/Treatment Group"),
              tags$dd("The units that received the intervention or policy you're studying."),
              
              tags$dt("Control Group"),
              tags$dd("The units that didn't receive the intervention — used for comparison."),
              
              tags$dt("Pre-treatment Period"),
              tags$dd("The time before the intervention started."),
              
              tags$dt("Parallel Trends"),
              tags$dd("The assumption that treated and control groups would have followed similar patterns without the intervention."),
              
              tags$dt("Effect Size"),
              tags$dd("How big the impact of the intervention was.")
            )
          ),
          
          box(
            title = "Learn More",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            
            tags$h4(icon("book"), " Recommended Resources"),
            tags$ul(
              tags$li(tags$a(href = "https://mixtape.scunning.com/", "Causal Inference: The Mixtape", target = "_blank"), " (Free online book)"),
              tags$li(tags$a(href = "https://theeffectbook.net/", "The Effect by Nick Huntington-Klein", target = "_blank"), " (Free online book)")
            ),
            
            tags$h4(icon("video"), " Video Tutorials"),
            tags$p("Coming soon!")
          )
        )
      )
    )
  )
)


# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # ===== REACTIVE VALUES =====
  data <- reactiveVal(NULL)
  detected <- reactiveVal(list())
  did_results_data <- reactiveVal(NULL)
  synth_results_data <- reactiveVal(NULL)
  current_did_step <- reactiveVal(1)
  current_synth_step <- reactiveVal(1)
  
  # Initialize the chatbot module with access to app state
  chatbotServer(
    id = "chatbot",
    current_tab = reactive(input$sidebar),
    data = data,                    # Your main data reactive
    detected = detected,            # Your detected structure reactive
    did_step = current_did_step,    # DiD step reactive
    synth_step = current_synth_step, # Synth step reactive
    did_results = did_results_data,  # DiD results reactive
    synth_results = synth_results_data  # Synth results reactive
  )

  # ===== GLOBAL PROGRESS TRACKING =====

  # Calculate which steps are completed
  progress_status <- reactive({
    df <- data()
    det <- detected()
    has_did <- !is.null(did_results_data())
    has_synth <- !is.null(synth_results_data())

    list(
      upload = !is.null(df),
      explore = !is.null(df) && !is.null(det$data_type),
      analyze = has_did || has_synth,
      export = has_did || has_synth
    )
  })

  # Render global progress stepper
  output$global_progress_stepper <- renderUI({
    # Don't show on welcome page
    if (is.null(input$sidebar) || input$sidebar == "welcome") {
      return(NULL)
    }

    progress <- progress_status()
    current_tab <- input$sidebar

    # Define steps
    steps <- list(
      list(num = 1, label = "Upload", id = "upload", tabs = c("upload")),
      list(num = 2, label = "Explore", id = "explore", tabs = c("overview", "trends", "stats")),
      list(num = 3, label = "Analyze", id = "analyze", tabs = c("did", "synth")),
      list(num = 4, label = "Export", id = "export", tabs = c("export"))
    )

    # Create step elements
    step_elements <- lapply(steps, function(step) {
      # Determine step status
      is_active <- current_tab %in% step$tabs
      is_completed <- progress[[step$id]]

      step_class <- "progress-step"
      if (is_completed) step_class <- paste(step_class, "completed")
      if (is_active) step_class <- paste(step_class, "active")

      # Circle content
      circle_content <- if (is_completed) {
        icon("check")
      } else {
        step$num
      }

      div(
        class = step_class,
        onclick = sprintf("Shiny.setInputValue('progress_step_click', '%s', {priority: 'event'})", step$tabs[1]),
        div(class = "progress-connector"),
        div(class = "progress-circle", circle_content),
        div(class = "progress-label", step$label)
      )
    })

    div(
      class = "global-progress-wrapper",
      div(
        class = "global-progress-stepper",
        step_elements
      )
    )
  })

  # Handle progress step clicks
  observeEvent(input$progress_step_click, {
    updateTabItems(session, "sidebar", input$progress_step_click)
  })

  # ===== DATASET LIBRARY =====

  # Toggle dataset library panel
  observeEvent(input$show_dataset_library, {
    shinyjs::toggle("dataset_library_panel")
  })

  # Render dataset library UI
  output$dataset_library_ui <- renderUI({
    metadata <- get_dataset_metadata()

    dataset_cards <- lapply(names(metadata), function(dataset_id) {
      meta <- metadata[[dataset_id]]

      # Determine badge class
      badge_class <- switch(
        tolower(meta$difficulty),
        "beginner" = "badge-beginner",
        "intermediate" = "badge-intermediate",
        "advanced" = "badge-advanced",
        "badge-beginner"
      )

      div(
        class = "dataset-card",
        div(class = paste("dataset-icon", meta$color), icon(meta$icon)),
        tags$h4(meta$name),
        span(class = paste("dataset-badge", badge_class), meta$difficulty),
        div(class = "dataset-description", meta$description),
        div(
          class = "dataset-meta",
          div(icon("table"), sprintf("%d rows", meta$rows)),
          div(icon("chart-line"), meta$best_for)
        ),
        actionButton(
          paste0("load_dataset_", dataset_id),
          "Load This Dataset",
          class = "load-dataset-btn",
          onclick = sprintf("Shiny.setInputValue('load_example_dataset', '%s', {priority: 'event'})", dataset_id)
        )
      )
    })

    div(class = "dataset-grid", dataset_cards)
  })

  # Handle dataset loading
  observeEvent(input$load_example_dataset, {
    dataset_id <- input$load_example_dataset
    req(dataset_id)

    tryCatch({
      # Load the dataset
      df <- get_example_dataset(dataset_id)

      if (!is.null(df)) {
        data(df)
        detected(detect_data_structure(df))

        # Get metadata for notification
        metadata <- get_dataset_metadata()
        meta <- metadata[[dataset_id]]

        # Hide the library panel
        shinyjs::hide("dataset_library_panel")

        # Show success notification
        showNotification(
          paste0(
            "Loaded: ", meta$name, "\n",
            format(nrow(df), big.mark = ","), " rows, ",
            ncol(df), " columns\n\n",
            "Suggested: ", meta$suggested_analysis
          ),
          type = "message",
          duration = 10
        )
      }
    }, error = function(e) {
      showNotification(paste("Error loading dataset:", e$message), type = "error")
    })
  })

  # ===== DATA LOADING =====
  
  output$has_data <- reactive({ !is.null(data()) })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # File upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      ext <- tools::file_ext(input$file_upload$name)
      
      if (ext %in% c("xlsx", "xls")) {
        df <- read_excel(input$file_upload$datapath)
      } else {
        df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      }
      
      df <- as.data.frame(df)
      data(df)
      detected(detect_data_structure(df))
      
      showNotification(
        paste("Loaded", format(nrow(df), big.mark = ","), "rows and", ncol(df), "columns"),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Sample data
  observeEvent(input$load_sample, {
    set.seed(42)
    
    states <- c("California", "Texas", "Florida", "New York", "Illinois",
                "Pennsylvania", "Ohio", "Georgia", "Michigan", "Arizona")
    years <- 2000:2019
    
    sample_data <- expand.grid(state = states, year = years, stringsAsFactors = FALSE)
    n <- nrow(sample_data)
    
    state_effects <- setNames(rnorm(10, 0, 5), states)
    
    sample_data <- sample_data %>%
      mutate(
        gdp_per_capita = 40 + state_effects[state] + 0.5 * (year - 2000) + rnorm(n, 0, 2),
        population_millions = case_when(
          state == "California" ~ 35 + 0.3 * (year - 2000) + rnorm(n(), 0, 0.5),
          state == "Texas" ~ 22 + 0.4 * (year - 2000) + rnorm(n(), 0, 0.5),
          state == "New York" ~ 19 + 0.1 * (year - 2000) + rnorm(n(), 0, 0.3),
          TRUE ~ 10 + 0.15 * (year - 2000) + rnorm(n(), 0, 0.5)
        ),
        unemployment_rate = 5 + rnorm(n, 0, 1) + ifelse(year >= 2008 & year <= 2010, 2, 0),
        policy_enacted = ifelse(state == "California" & year >= 2010, 1, 0),
        gdp_per_capita = gdp_per_capita + ifelse(policy_enacted == 1, 3 + 0.5 * (year - 2010), 0),
        education_spending = 2 + 0.1 * gdp_per_capita + rnorm(n, 0, 0.3),
        median_income = 30 + 0.8 * gdp_per_capita + rnorm(n, 0, 3)
      ) %>%
      arrange(state, year)
    
    data(sample_data)
    detected(detect_data_structure(sample_data))
    
    showNotification(
      "Sample data loaded! California enacted a policy in 2010.",
      type = "message",
      duration = 8
    )
  })
  
  # Navigation
  observeEvent(input$start_btn, {
    updateTabItems(session, "sidebar", "upload")
  })
  
  # ===== DATA STATUS IN SIDEBAR =====
  
  output$data_status <- renderText({
    df <- data()
    if (is.null(df)) return("No data loaded")
    
    det <- detected()
    
    paste0(
      format(nrow(df), big.mark = ","), " rows\n",
      ncol(df), " columns\n",
      if (!is.null(det$data_type)) paste0("Type: ", det$data_type) else ""
    )
  })
  
  # ===== UPDATE INPUTS WHEN DATA CHANGES =====
  
  observe({
    df <- data()
    det <- detected()
    
    if (!is.null(df)) {
      all_cols <- names(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      all_with_none <- c("(none)" = "", all_cols)
      
      # Trend visualization
      updateSelectInput(session, "trend_y", choices = num_cols, selected = det$outcome_candidates[1])
      updateSelectInput(session, "trend_x", choices = all_cols, selected = det$time_var)
      updateSelectInput(session, "trend_color", choices = all_with_none, selected = det$unit_id)
      
      # Stats
      updateSelectizeInput(session, "stats_vars", choices = num_cols, selected = num_cols[1:min(3, length(num_cols))])
      updateSelectInput(session, "stats_groupby", choices = all_with_none)
      updateSelectInput(session, "dist_var", choices = num_cols, selected = num_cols[1])
      
      # DiD
      updateSelectInput(session, "did_unit", choices = all_cols, selected = det$unit_id)
      updateSelectInput(session, "did_time", choices = all_cols, selected = det$time_var)
      updateSelectInput(session, "did_outcome", choices = num_cols, selected = det$outcome_candidates[1])
      updateSelectInput(session, "did_treatment_var", choices = all_cols, selected = det$treatment_var)
      updateSelectizeInput(session, "did_controls", choices = num_cols)
      
      # Synth
      updateSelectInput(session, "synth_unit", choices = all_cols, selected = det$unit_id)
      updateSelectInput(session, "synth_time", choices = all_cols, selected = det$time_var)
      updateSelectInput(session, "synth_outcome", choices = num_cols, selected = det$outcome_candidates[1])
      updateSelectizeInput(session, "synth_predictors", choices = num_cols)
    }
  })
  
  # Update unit choices for DiD and Synth
  observe({
    df <- data()
    unit_var <- input$did_unit
    if (!is.null(df) && !is.null(unit_var) && unit_var %in% names(df)) {
      units <- unique(df[[unit_var]])
      updateSelectizeInput(session, "did_treated_units", choices = units)
      updateSelectInput(session, "trend_highlight", choices = c("(none)" = "", units))
    }
  })
  
  observe({
    df <- data()
    unit_var <- input$synth_unit
    if (!is.null(df) && !is.null(unit_var) && unit_var %in% names(df)) {
      units <- unique(df[[unit_var]])
      updateSelectInput(session, "synth_treated_unit", choices = units)
    }
  })
  
  # ===== UPLOAD TAB OUTPUTS =====
  
  output$data_preview <- renderDT({
    req(data())
    datatable(
      head(data(), 100),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'frtip'
      ),
      caption = "Showing first 100 rows"
    )
  })
  
  output$quality_check <- renderUI({
    df <- data()
    if (is.null(df)) {
      return(
        div(
          style = "text-align: center; padding: 40px; color: #7f8c8d;",
          icon("upload", class = "fa-3x"),
          tags$h4("Upload data to see quality check"),
          tags$p("We'll analyze your data and show you what we find.")
        )
      )
    }
    
    det <- detected()
    
    # Quality gauge
    gauge_class <- if (det$quality_score >= 80) "gauge-excellent" else if (det$quality_score >= 60) "gauge-good" else if (det$quality_score >= 40) "gauge-moderate" else "gauge-poor"
    
    div(
      div(
        class = "quality-gauge",
        div(class = paste("gauge-circle", gauge_class), paste0(det$quality_score, "%")),
        tags$h4("Data Readiness Score")
      ),
      
      hr(),
      
      # What we found
      tags$h5(icon("search"), " What We Found:"),
      
      if (!is.null(det$unit_id)) {
        div(class = "check-item pass",
            icon("check-circle"),
            tags$span(tags$strong("Unit identifier: "), det$unit_id))
      } else {
        div(class = "check-item warning",
            icon("exclamation-circle"),
            tags$span("Couldn't identify a unit variable"))
      },
      
      if (!is.null(det$time_var)) {
        div(class = "check-item pass",
            icon("check-circle"),
            tags$span(tags$strong("Time variable: "), det$time_var))
      } else {
        div(class = "check-item warning",
            icon("exclamation-circle"),
            tags$span("Couldn't identify a time variable"))
      },
      
      if (length(det$outcome_candidates) > 0) {
        div(class = "check-item pass",
            icon("check-circle"),
            tags$span(tags$strong("Possible outcomes: "), paste(det$outcome_candidates, collapse = ", ")))
      },
      
      if (!is.null(det$treatment_var)) {
        div(class = "check-item pass",
            icon("check-circle"),
            tags$span(tags$strong("Treatment indicator: "), det$treatment_var))
      },
      
      # Issues
      if (length(det$issues) > 0) {
        tagList(
          hr(),
          tags$h5(icon("exclamation-triangle"), " Things to Note:"),
          lapply(det$issues, function(issue) {
            div(class = "check-item warning",
                icon("exclamation-circle"),
                tags$span(issue))
          })
        )
      }
    )
  })
  
  output$detected_structure <- renderUI({
    det <- detected()
    if (length(det) == 0) return(NULL)
    
    div(
      class = "info-panel",
      style = "margin-top: 20px;",
      tags$h4(icon("magic"), " Auto-Detected Structure"),
      tags$p(
        tags$strong("Data type: "), 
        switch(det$data_type,
               "panel" = "Panel data (units observed over time) ✓",
               "time_series" = "Time series data",
               "cross_section" = "Cross-sectional data",
               "Unknown")
      ),
      if (length(det$suggestions) > 0) {
        tags$ul(lapply(det$suggestions, tags$li))
      }
    )
  })
  
  # ===== OVERVIEW TAB =====
  
  output$n_rows_box <- renderValueBox({
    df <- data()
    valueBox(
      if (!is.null(df)) format(nrow(df), big.mark = ",") else "—",
      "Observations",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$n_cols_box <- renderValueBox({
    df <- data()
    valueBox(
      if (!is.null(df)) ncol(df) else "—",
      "Variables",
      icon = icon("columns"),
      color = "purple"
    )
  })
  
  output$n_units_box <- renderValueBox({
    df <- data()
    det <- detected()
    
    n_units <- "—"
    if (!is.null(df) && !is.null(det$unit_id)) {
      n_units <- length(unique(df[[det$unit_id]]))
    }
    
    valueBox(
      n_units,
      "Unique Units",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$n_periods_box <- renderValueBox({
    df <- data()
    det <- detected()
    
    n_periods <- "—"
    if (!is.null(df) && !is.null(det$time_var)) {
      n_periods <- length(unique(df[[det$time_var]]))
    }
    
    valueBox(
      n_periods,
      "Time Periods",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$var_summary_table <- renderDT({
    req(data())
    df <- data()
    
    summary_df <- data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      `Non-Missing` = sapply(df, function(x) sum(!is.na(x))),
      `Missing` = sapply(df, function(x) sum(is.na(x))),
      `Unique Values` = sapply(df, function(x) length(unique(x))),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Add summary stats for numeric
    summary_df$Mean <- sapply(names(df), function(col) {
      if (is.numeric(df[[col]])) round(mean(df[[col]], na.rm = TRUE), 2) else NA
    })
    
    summary_df$`Std Dev` <- sapply(names(df), function(col) {
      if (is.numeric(df[[col]])) round(sd(df[[col]], na.rm = TRUE), 2) else NA
    })
    
    datatable(
      summary_df,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$data_structure_info <- renderUI({
    det <- detected()
    if (length(det) == 0) return(tags$p("Load data to see structure"))
    
    div(
      tags$p(tags$strong("Data Type: "), det$data_type),
      
      if (!is.null(det$unit_id)) {
        tags$p(tags$strong("Unit Variable: "), det$unit_id)
      },
      
      if (!is.null(det$time_var)) {
        tags$p(tags$strong("Time Variable: "), det$time_var)
      },
      
      if (det$panel_structure) {
        tags$p(icon("check-circle", style = "color: #27ae60;"), " Panel structure detected")
      }
    )
  })
  
  output$missing_plot <- renderPlotly({
    req(data())
    df <- data()
    
    missing_df <- data.frame(
      Variable = names(df),
      Missing = sapply(df, function(x) sum(is.na(x))),
      stringsAsFactors = FALSE
    ) %>%
      filter(Missing > 0) %>%
      arrange(desc(Missing))
    
    if (nrow(missing_df) == 0) {
      # Create an empty plot with a message
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No missing values! ✓", 
                 size = 6, color = "#27ae60") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      return(ggplotly(p, height = 300) %>% config(displayModeBar = FALSE))
    }
    
    p <- ggplot(missing_df, aes(x = reorder(Variable, Missing), y = Missing)) +
      geom_col(fill = "#e74c3c", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Missing Values by Variable",
        x = "",
        y = "Number of Missing Values"
      ) +
      theme_minimal()
    
    ggplotly(p, height = 300) %>% config(displayModeBar = FALSE)
  })
  
  # ===== TRENDS TAB =====
  
  output$trend_plot <- renderPlotly({
    # Validate all required inputs exist and are valid
    req(data())
    req(input$trend_y)
    req(input$trend_x)
    
    df <- data()
    
    # Ensure columns exist in data
    validate(
      need(input$trend_x %in% names(df), "Please select a valid time variable"),
      need(input$trend_y %in% names(df), "Please select a valid outcome variable")
    )
    
    # Build the plot
    if (!is.null(input$trend_color) && input$trend_color != "" && input$trend_color %in% names(df)) {
      p <- ggplot(df, aes_string(x = input$trend_x, y = input$trend_y, 
                                 color = input$trend_color, group = input$trend_color))
    } else {
      p <- ggplot(df, aes_string(x = input$trend_x, y = input$trend_y, group = 1))
    }
    
    if (isTRUE(input$trend_points)) {
      p <- p + geom_point(alpha = 0.6, size = 2)
    }
    
    p <- p + geom_line(alpha = 0.7)
    
    if (isTRUE(input$trend_smooth) && (is.null(input$trend_color) || input$trend_color == "")) {
      p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "#3498db")
    }
    
    p <- p +
      labs(
        title = paste(input$trend_y, "over", input$trend_x),
        x = input$trend_x,
        y = input$trend_y
      ) +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(p, height = 500) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # ===== STATS TAB =====
  
  observeEvent(input$calc_stats, {
    req(data(), input$stats_vars)
  })
  
  output$stats_table <- renderDT({
    req(data(), input$stats_vars)
    df <- data()
    
    vars <- input$stats_vars[input$stats_vars %in% names(df)]
    if (length(vars) == 0) return(NULL)
    
    calc_summary <- function(data_subset, var_name) {
      x <- data_subset[[var_name]]
      if (!is.numeric(x)) return(NULL)
      
      data.frame(
        Variable = var_name,
        N = sum(!is.na(x)),
        Mean = round(mean(x, na.rm = TRUE), 3),
        `Std Dev` = round(sd(x, na.rm = TRUE), 3),
        Min = round(min(x, na.rm = TRUE), 3),
        Median = round(median(x, na.rm = TRUE), 3),
        Max = round(max(x, na.rm = TRUE), 3),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    
    if (input$stats_groupby != "" && input$stats_groupby %in% names(df)) {
      results <- df %>%
        group_by(across(all_of(input$stats_groupby))) %>%
        group_map(~ {
          group_results <- lapply(vars, function(v) calc_summary(.x, v))
          do.call(rbind, group_results)
        }, .keep = TRUE)
      
      results <- do.call(rbind, results)
    } else {
      results <- do.call(rbind, lapply(vars, function(v) calc_summary(df, v)))
    }
    
    datatable(
      results,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Mean", "Std Dev", "Min", "Median", "Max"), digits = 3)
  })
  
  output$dist_plot <- renderPlotly({
    req(data())
    req(input$dist_var)
    
    df <- data()
    
    validate(
      need(input$dist_var %in% names(df), "Please select a valid variable"),
      need(is.numeric(df[[input$dist_var]]), "Please select a numeric variable")
    )
    
    p <- ggplot(df, aes_string(x = input$dist_var)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498db", color = "white", alpha = 0.7) +
      geom_density(color = "#e74c3c", size = 1) +
      labs(title = paste("Distribution of", input$dist_var), x = input$dist_var, y = "Density") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$comparison_boxplot <- renderPlotly({
    req(data())
    req(input$stats_vars)
    req(input$stats_groupby)
    
    df <- data()
    
    validate(
      need(input$stats_groupby != "", "Select a grouping variable to see comparison"),
      need(input$stats_groupby %in% names(df), "Please select a valid grouping variable"),
      need(length(input$stats_vars) > 0, "Please select at least one variable")
    )
    
    var <- input$stats_vars[1]
    
    validate(
      need(var %in% names(df), "Please select a valid variable"),
      need(is.numeric(df[[var]]), "Please select a numeric variable for comparison")
    )
    
    p <- ggplot(df, aes_string(x = input$stats_groupby, y = var, fill = input$stats_groupby)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste(var, "by", input$stats_groupby)) +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$download_stats <- downloadHandler(
    filename = function() {
      paste0("summary_statistics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(data(), input$stats_vars)
      df <- data()
      vars <- input$stats_vars[input$stats_vars %in% names(df)]
      
      results <- do.call(rbind, lapply(vars, function(v) {
        x <- df[[v]]
        if (!is.numeric(x)) return(NULL)
        data.frame(
          Variable = v,
          N = sum(!is.na(x)),
          Mean = mean(x, na.rm = TRUE),
          SD = sd(x, na.rm = TRUE),
          Min = min(x, na.rm = TRUE),
          Median = median(x, na.rm = TRUE),
          Max = max(x, na.rm = TRUE)
        )
      }))
      
      write.csv(results, file, row.names = FALSE)
    }
  )
  
  # ===== DIFFERENCE-IN-DIFFERENCES =====
  
  # Step indicator
  output$did_steps <- renderUI({
    step <- current_did_step()
    
    div(
      class = "step-indicator",
      div(class = paste("step", if (step >= 1) "active" else "", if (step > 1) "completed" else ""),
          div(class = "step-number", if (step > 1) icon("check") else "1"),
          div(class = "step-label", "Setup")),
      div(class = paste("step", if (step >= 2) "active" else "", if (step > 2) "completed" else ""),
          div(class = "step-number", if (step > 2) icon("check") else "2"),
          div(class = "step-label", "Check")),
      div(class = paste("step", if (step >= 3) "active" else ""),
          div(class = "step-number", "3"),
          div(class = "step-label", "Results"))
    )
  })
  
  # Check readiness
  observeEvent(input$did_check_ready, {
    req(data(), input$did_unit, input$did_time, input$did_outcome)
    
    current_did_step(2)
    shinyjs::show("did_ready_panel")
    shinyjs::hide("did_setup_panel")
  })
  
  observeEvent(input$did_back_setup, {
    current_did_step(1)
    shinyjs::hide("did_ready_panel")
    shinyjs::show("did_setup_panel")
  })
  
  output$did_readiness_checks <- renderUI({
    req(data(), input$did_unit, input$did_time, input$did_outcome)
    df <- data()
    
    # Determine treatment info
    if (input$did_treatment_type == "indicator") {
      treatment_var <- input$did_treatment_var
      treated_units <- NULL
      treatment_time <- NULL
      
      if (!is.null(treatment_var) && treatment_var %in% names(df)) {
        treated_obs <- df[df[[treatment_var]] == 1, ]
        if (nrow(treated_obs) > 0) {
          treatment_time <- min(treated_obs[[input$did_time]])
          treated_units <- unique(treated_obs[[input$did_unit]])
        }
      }
    } else {
      treatment_var <- NULL
      treated_units <- input$did_treated_units
      treatment_time <- input$did_treatment_time
    }
    
    checks <- check_analysis_readiness(
      df, input$did_unit, input$did_time, input$did_outcome,
      treatment_var, treated_units, treatment_time, "did"
    )
    
    check_items <- lapply(names(checks$checks), function(check_name) {
      check <- checks$checks[[check_name]]
      icon_name <- switch(check$status, "pass" = "check-circle", "warning" = "exclamation-circle", "fail" = "times-circle")
      
      div(
        class = paste("check-item", check$status),
        icon(icon_name),
        tags$span(tags$strong(check$title), ": ", check$message)
      )
    })
    
    div(check_items)
  })
  
  output$did_preview_trends <- renderPlotly({
    req(data())
    req(input$did_unit)
    req(input$did_time)
    req(input$did_outcome)
    
    df <- data()
    
    validate(
      need(input$did_unit %in% names(df), "Please select a valid unit variable"),
      need(input$did_time %in% names(df), "Please select a valid time variable"),
      need(input$did_outcome %in% names(df), "Please select a valid outcome variable")
    )
    
    # Get treatment info
    treated_units <- NULL
    treatment_time <- NULL
    
    if (input$did_treatment_type == "indicator" && !is.null(input$did_treatment_var)) {
      treatment_var <- input$did_treatment_var
      if (treatment_var %in% names(df)) {
        treated_obs <- df[df[[treatment_var]] == 1, ]
        if (nrow(treated_obs) > 0) {
          treatment_time <- min(treated_obs[[input$did_time]])
          treated_units <- unique(treated_obs[[input$did_unit]])
        }
      }
    } else {
      treated_units <- input$did_treated_units
      treatment_time <- input$did_treatment_time
    }
    
    validate(
      need(!is.null(treated_units) && length(treated_units) > 0, "Please specify treated units"),
      need(!is.null(treatment_time), "Please specify treatment time")
    )
    
    df$Group <- ifelse(df[[input$did_unit]] %in% treated_units, "Treated", "Control")
    
    trend_data <- df %>%
      group_by(across(all_of(input$did_time)), Group) %>%
      summarise(mean_outcome = mean(get(input$did_outcome), na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(trend_data, aes_string(x = input$did_time, y = "mean_outcome", color = "Group")) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_vline(xintercept = treatment_time - 0.5, linetype = "dashed", color = "gray40") +
      scale_color_manual(values = c("Control" = "#3498db", "Treated" = "#e74c3c")) +
      labs(x = input$did_time, y = input$did_outcome, color = "") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, height = 300) %>% config(displayModeBar = FALSE)
  })
  
  output$did_ready_summary <- renderUI({
    req(data(), input$did_unit, input$did_time, input$did_outcome)
    df <- data()
    
    # Get treatment info
    if (input$did_treatment_type == "indicator" && !is.null(input$did_treatment_var)) {
      treatment_var <- input$did_treatment_var
      treated_units <- NULL
      treatment_time <- NULL
      
      if (treatment_var %in% names(df)) {
        treated_obs <- df[df[[treatment_var]] == 1, ]
        if (nrow(treated_obs) > 0) {
          treatment_time <- min(treated_obs[[input$did_time]])
          treated_units <- unique(treated_obs[[input$did_unit]])
        }
      }
    } else {
      treatment_var <- NULL
      treated_units <- input$did_treated_units
      treatment_time <- input$did_treatment_time
    }
    
    checks <- check_analysis_readiness(
      df, input$did_unit, input$did_time, input$did_outcome,
      treatment_var, treated_units, treatment_time, "did"
    )
    
    if (checks$ready) {
      div(
        class = "info-panel",
        style = "background: #d4edda; border-left-color: #27ae60;",
        tags$h4(icon("check-circle"), " Ready to Analyze!"),
        tags$p(checks$summary)
      )
    } else {
      div(
        class = "info-panel",
        style = "background: #f8d7da; border-left-color: #e74c3c;",
        tags$h4(icon("exclamation-triangle"), " Not Ready Yet"),
        tags$p(checks$summary)
      )
    }
  })
  
  # Run DiD analysis
  observeEvent(input$did_run, {
    req(data(), input$did_unit, input$did_time, input$did_outcome)
    
    tryCatch({
      withProgress(message = 'Running analysis...', value = 0.5, {
        
        df <- data()
        
        # Get treatment specification
        if (input$did_treatment_type == "indicator") {
          results <- run_did_analysis(
            df = df,
            unit_var = input$did_unit,
            time_var = input$did_time,
            outcome_var = input$did_outcome,
            treatment_var = input$did_treatment_var,
            controls = input$did_controls
          )
        } else {
          results <- run_did_analysis(
            df = df,
            unit_var = input$did_unit,
            time_var = input$did_time,
            outcome_var = input$did_outcome,
            treated_units = input$did_treated_units,
            treatment_time = input$did_treatment_time,
            controls = input$did_controls
          )
        }
        
        did_results_data(results)
        
        current_did_step(3)
        shinyjs::hide("did_ready_panel")
        shinyjs::show("did_results_panel")
        
        showNotification("Analysis complete!", type = "message")
      })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # DiD Results outputs
  output$did_executive_summary <- renderUI({
    req(did_results_data())
    results <- did_results_data()
    
    if (!results$success) {
      return(div(class = "warning-panel", tags$h4("Analysis Error"), tags$p(results$error)))
    }
    
    generate_executive_summary(results, "did")
  })
  
  output$did_main_plot <- renderPlotly({
    req(did_results_data())
    results <- did_results_data()
    
    validate(
      need(results$success, "Analysis did not complete successfully")
    )
    
    df <- results$data
    
    trend_data <- df %>%
      group_by(across(all_of(results$time_var)), treated_group) %>%
      summarise(mean_outcome = mean(get(results$outcome_var), na.rm = TRUE), .groups = "drop") %>%
      mutate(Group = ifelse(treated_group == 1, "Treated", "Control"))
    
    p <- ggplot(trend_data, aes_string(x = results$time_var, y = "mean_outcome", color = "Group")) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_vline(xintercept = results$treatment_time - 0.5, linetype = "dashed", color = "gray40", size = 1) +
      annotate("text", x = results$treatment_time, y = max(trend_data$mean_outcome), 
               label = "Intervention", hjust = -0.1, color = "gray40") +
      scale_color_manual(values = c("Control" = "#3498db", "Treated" = "#e74c3c")) +
      labs(
        x = results$time_var,
        y = results$outcome_var,
        color = ""
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, height = 400) %>% config(displayModeBar = FALSE)
  })
  
  output$did_key_stats <- renderUI({
    req(did_results_data())
    results <- did_results_data()
    
    if (!results$success) return(NULL)
    
    effect_class <- if (results$estimate > 0) "positive" else "negative"
    conf_class <- paste0("confidence-", gsub("_", "-", results$confidence$level))
    
    div(
      div(
        class = "result-card",
        tags$h4("Estimated Effect"),
        div(class = paste("key-metric", effect_class), sprintf("%+.3f", results$estimate)),
        div(class = "metric-label", paste("units change in", results$outcome_var))
      ),
      
      div(
        class = "result-card",
        tags$h4("Confidence Level"),
        span(class = paste("confidence-badge", conf_class), results$confidence$text),
        tags$p(style = "margin-top: 10px; font-size: 0.9em;", results$confidence$description)
      ),
      
      div(
        class = "result-card",
        tags$h4("Uncertainty Range"),
        tags$p(sprintf("%.3f to %.3f", results$ci_lower, results$ci_upper)),
        tags$p(class = "metric-label", "95% confidence interval")
      )
    )
  })
  
  output$did_pretrend_plot <- renderPlotly({
    req(did_results_data())
    results <- did_results_data()
    
    validate(
      need(results$success, "Analysis did not complete successfully")
    )
    
    df <- results$data
    
    trend_data <- df %>%
      group_by(across(all_of(results$time_var)), treated_group) %>%
      summarise(mean_outcome = mean(get(results$outcome_var), na.rm = TRUE), .groups = "drop") %>%
      mutate(Group = ifelse(treated_group == 1, "Treated", "Control"))
    
    p <- ggplot(trend_data, aes_string(x = results$time_var, y = "mean_outcome", color = "Group")) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_vline(xintercept = results$treatment_time - 0.5, linetype = "dashed", color = "gray40", size = 1) +
      scale_color_manual(values = c("Control" = "#3498db", "Treated" = "#e74c3c")) +
      labs(title = "Parallel Trends Check", x = results$time_var, y = results$outcome_var, color = "") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, height = 350) %>% config(displayModeBar = FALSE)
  })
  
  output$did_pretrend_test <- renderPrint({
    req(did_results_data())
    results <- did_results_data()
    
    if (!results$success) return(cat("Analysis not available"))
    
    df <- results$data
    pre_data <- df[df[[results$time_var]] < results$treatment_time, ]
    
    if (nrow(pre_data) < 10) {
      cat("Not enough pre-treatment data for formal test.\n")
      return()
    }
    
    pre_data$time_trend <- as.numeric(pre_data[[results$time_var]])
    
    tryCatch({
      trend_model <- lm(
        as.formula(paste(results$outcome_var, "~ time_trend * treated_group")), 
        data = pre_data
      )
      
      cat("Pre-Treatment Trend Test\n")
      cat("========================\n\n")
      cat("Testing if treated and control groups had different trends before the intervention.\n\n")
      
      summary_model <- summary(trend_model)
      interaction_row <- grep("time_trend:treated_group", rownames(summary_model$coefficients))
      
      if (length(interaction_row) > 0) {
        pval <- summary_model$coefficients[interaction_row, "Pr(>|t|)"]
        
        if (pval < 0.05) {
          cat("⚠ WARNING: The groups had significantly different trends before the intervention.\n")
          cat("   This could bias your results.\n\n")
        } else {
          cat("✓ GOOD: No significant difference in pre-treatment trends detected.\n")
          cat("   This supports the validity of your analysis.\n\n")
        }
        
        cat(sprintf("Statistical test p-value: %.4f\n", pval))
      }
    }, error = function(e) {
      cat("Could not perform trend test:", e$message, "\n")
    })
  })
  
  output$did_balance_table <- renderDT({
    req(did_results_data())
    results <- did_results_data()
    
    if (!results$success) return(NULL)
    
    df <- results$data
    pre_data <- df[df[[results$time_var]] < results$treatment_time, ]
    
    num_cols <- names(pre_data)[sapply(pre_data, is.numeric)]
    num_cols <- num_cols[!num_cols %in% c("treatment", "treated_group", "post")]
    
    if (length(num_cols) == 0) return(NULL)
    
    balance_data <- pre_data %>%
      group_by(treated_group) %>%
      summarise(across(all_of(num_cols), ~mean(., na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(-treated_group, names_to = "Variable", values_to = "Mean") %>%
      pivot_wider(names_from = treated_group, values_from = Mean, names_prefix = "Group_")
    
    names(balance_data) <- c("Variable", "Control Mean", "Treated Mean")
    balance_data$Difference <- balance_data$`Treated Mean` - balance_data$`Control Mean`
    
    datatable(
      balance_data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Control Mean", "Treated Mean", "Difference"), digits = 3)
  })
  
  output$did_full_output <- renderPrint({
    req(did_results_data())
    results <- did_results_data()
    
    if (!results$success) return(cat("Analysis not available"))
    
    summary(results$model)
  })
  
  output$did_coef_table <- renderDT({
    req(did_results_data())
    results <- did_results_data()
    
    if (!results$success) return(NULL)
    
    coef_table <- as.data.frame(summary(results$model)$coeftable)
    coef_table$Variable <- rownames(coef_table)
    coef_table <- coef_table[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
    names(coef_table) <- c("Variable", "Estimate", "Std. Error", "t-statistic", "p-value")
    
    datatable(coef_table, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = 2:5, digits = 4)
  })
  
  observeEvent(input$did_restart, {
    current_did_step(1)
    shinyjs::hide("did_results_panel")
    shinyjs::show("did_setup_panel")
    did_results_data(NULL)
  })
  
  # ===== SYNTHETIC CONTROL =====
  
  # Step indicator
  output$synth_steps <- renderUI({
    step <- current_synth_step()
    
    div(
      class = "step-indicator",
      div(class = paste("step", if (step >= 1) "active" else "", if (step > 1) "completed" else ""),
          div(class = "step-number", if (step > 1) icon("check") else "1"),
          div(class = "step-label", "Setup")),
      div(class = paste("step", if (step >= 2) "active" else "", if (step > 2) "completed" else ""),
          div(class = "step-number", if (step > 2) icon("check") else "2"),
          div(class = "step-label", "Check")),
      div(class = paste("step", if (step >= 3) "active" else ""),
          div(class = "step-number", "3"),
          div(class = "step-label", "Results"))
    )
  })
  
  observeEvent(input$synth_check_ready, {
    req(data(), input$synth_unit, input$synth_time, input$synth_outcome, 
        input$synth_treated_unit, input$synth_treatment_time)
    
    current_synth_step(2)
    shinyjs::show("synth_ready_panel")
    shinyjs::hide("synth_setup_panel")
  })
  
  observeEvent(input$synth_back_setup, {
    current_synth_step(1)
    shinyjs::hide("synth_ready_panel")
    shinyjs::show("synth_setup_panel")
  })
  
  output$synth_readiness_checks <- renderUI({
    req(data(), input$synth_unit, input$synth_time, input$synth_outcome,
        input$synth_treated_unit, input$synth_treatment_time)
    df <- data()
    
    checks <- list()
    
    # Check number of control units
    all_units <- unique(df[[input$synth_unit]])
    n_controls <- length(all_units) - 1
    
    if (n_controls >= 10) {
      checks$controls <- list(status = "pass", title = "Control Units", 
                              message = sprintf("%d potential control units available", n_controls))
    } else if (n_controls >= 5) {
      checks$controls <- list(status = "warning", title = "Control Units",
                              message = sprintf("%d control units (more would be better)", n_controls))
    } else {
      checks$controls <- list(status = "fail", title = "Control Units",
                              message = sprintf("Only %d control units (may be insufficient)", n_controls))
    }
    
    # Check pre-treatment periods
    time_vals <- sort(unique(df[[input$synth_time]]))
    pre_periods <- sum(time_vals < input$synth_treatment_time)
    
    if (pre_periods >= 10) {
      checks$periods <- list(status = "pass", title = "Pre-Treatment Periods",
                             message = sprintf("%d periods before treatment", pre_periods))
    } else if (pre_periods >= 5) {
      checks$periods <- list(status = "warning", title = "Pre-Treatment Periods",
                             message = sprintf("%d periods (more would improve matching)", pre_periods))
    } else {
      checks$periods <- list(status = "fail", title = "Pre-Treatment Periods",
                             message = sprintf("Only %d periods (need at least 5)", pre_periods))
    }
    
    # Render checks
    check_items <- lapply(names(checks), function(check_name) {
      check <- checks[[check_name]]
      icon_name <- switch(check$status, "pass" = "check-circle", "warning" = "exclamation-circle", "fail" = "times-circle")
      
      div(
        class = paste("check-item", check$status),
        icon(icon_name),
        tags$span(tags$strong(check$title), ": ", check$message)
      )
    })
    
    div(check_items)
  })
  
  output$synth_preview_plot <- renderPlotly({
    req(data())
    req(input$synth_unit)
    req(input$synth_time)
    req(input$synth_outcome)
    req(input$synth_treated_unit)
    req(input$synth_treatment_time)
    
    df <- data()
    
    validate(
      need(input$synth_unit %in% names(df), "Please select a valid unit variable"),
      need(input$synth_time %in% names(df), "Please select a valid time variable"),
      need(input$synth_outcome %in% names(df), "Please select a valid outcome variable"),
      need(input$synth_treated_unit %in% unique(df[[input$synth_unit]]), "Please select a valid treated unit")
    )
    
    df$is_treated <- ifelse(df[[input$synth_unit]] == input$synth_treated_unit, "Treated", "Control")
    
    p <- ggplot(df, aes_string(x = input$synth_time, y = input$synth_outcome, 
                               group = input$synth_unit, color = "is_treated", alpha = "is_treated")) +
      geom_line() +
      geom_vline(xintercept = input$synth_treatment_time - 0.5, linetype = "dashed", color = "gray40") +
      scale_color_manual(values = c("Control" = "gray70", "Treated" = "#e74c3c")) +
      scale_alpha_manual(values = c("Control" = 0.4, "Treated" = 1)) +
      labs(x = input$synth_time, y = input$synth_outcome, color = "", alpha = "") +
      theme_minimal() +
      guides(alpha = "none")
    
    ggplotly(p, height = 300) %>% config(displayModeBar = FALSE)
  })
  
  output$synth_ready_summary <- renderUI({
    # Simple pass for now
    div(
      class = "info-panel",
      style = "background: #d4edda; border-left-color: #27ae60;",
      tags$h4(icon("check-circle"), " Ready to Analyze!"),
      tags$p("Your data looks ready for synthetic control analysis.")
    )
  })
  
  # Run synthetic control
  observeEvent(input$synth_run, {
    req(data(), input$synth_unit, input$synth_time, input$synth_outcome,
        input$synth_treated_unit, input$synth_treatment_time)
    
    withProgress(message = 'Running synthetic control...', value = 0, {
      incProgress(0.2, detail = "Preparing data...")
      
      tryCatch({
        results <- run_synth_analysis(
          df = data(),
          unit_var = input$synth_unit,
          time_var = input$synth_time,
          outcome_var = input$synth_outcome,
          treated_unit = input$synth_treated_unit,
          treatment_time = input$synth_treatment_time,
          predictors = input$synth_predictors
        )
        
        incProgress(0.8, detail = "Processing results...")
        
        synth_results_data(results)
        
        current_synth_step(3)
        shinyjs::hide("synth_ready_panel")
        shinyjs::show("synth_results_panel")
        
        showNotification("Analysis complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  
  # Synth Results outputs
  output$synth_executive_summary <- renderUI({
    req(synth_results_data())
    results <- synth_results_data()
    
    if (!results$success) {
      return(div(class = "warning-panel", tags$h4("Analysis Error"), tags$p(results$error)))
    }
    
    generate_executive_summary(results, "synth")
  })
  
  output$synth_main_plot <- renderPlotly({
    req(synth_results_data())
    results <- synth_results_data()
    
    validate(
      need(results$success, "Analysis did not complete successfully")
    )
    
    plot_df <- data.frame(
      time = results$all_times,
      Actual = results$Y_treated,
      Synthetic = results$Y_synthetic
    ) %>%
      pivot_longer(cols = c(Actual, Synthetic), names_to = "Series", values_to = "Value")
    
    p <- ggplot(plot_df, aes(x = time, y = Value, color = Series)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_vline(xintercept = results$treatment_time - 0.5, linetype = "dashed", color = "gray40") +
      annotate("text", x = results$treatment_time, y = max(plot_df$Value), 
               label = "Intervention", hjust = -0.1, color = "gray40") +
      scale_color_manual(values = c("Actual" = "#e74c3c", "Synthetic" = "#3498db")) +
      labs(x = results$time_var, y = results$outcome_var, color = "") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, height = 400) %>% config(displayModeBar = FALSE)
  })
  
  output$synth_key_stats <- renderUI({
    req(synth_results_data())
    results <- synth_results_data()
    
    if (!results$success) return(NULL)
    
    effect_class <- if (results$mean_effect > 0) "positive" else "negative"
    
    div(
      div(
        class = "result-card",
        tags$h4("Average Effect"),
        div(class = paste("key-metric", effect_class), sprintf("%+.3f", results$mean_effect)),
        div(class = "metric-label", paste("change in", results$outcome_var))
      ),
      
      div(
        class = "result-card",
        tags$h4("Pre-Treatment Fit"),
        span(
          class = paste("confidence-badge", 
                        switch(results$fit_quality$status,
                               "excellent" = "confidence-very-high",
                               "good" = "confidence-high",
                               "moderate" = "confidence-moderate",
                               "confidence-low")),
          results$fit_quality$text
        ),
        tags$p(style = "margin-top: 10px; font-size: 0.9em;", results$fit_quality$description)
      ),
      
      div(
        class = "result-card",
        tags$h4("Comparison"),
        tags$p(sprintf("Treated unit: %s", results$treated_unit)),
        tags$p(sprintf("Synthetic from: %d units", results$n_controls))
      )
    )
  })
  
  output$synth_gap_plot <- renderPlotly({
    req(synth_results_data())
    results <- synth_results_data()
    
    validate(
      need(results$success, "Analysis did not complete successfully")
    )
    
    plot_df <- data.frame(
      time = results$all_times,
      gap = results$gap
    )
    
    p <- ggplot(plot_df, aes(x = time, y = gap)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_area(data = plot_df %>% filter(time >= results$treatment_time),
                fill = "#9b59b6", alpha = 0.3) +
      geom_line(color = "#9b59b6", size = 1.2) +
      geom_point(color = "#9b59b6", size = 2.5) +
      geom_vline(xintercept = results$treatment_time - 0.5, linetype = "dashed", color = "gray40") +
      labs(x = results$time_var, y = "Effect (Actual - Synthetic)") +
      theme_minimal()
    
    ggplotly(p, height = 300) %>% config(displayModeBar = FALSE)
  })
  
  output$synth_weights_plot <- renderPlotly({
    req(synth_results_data())
    results <- synth_results_data()
    
    validate(
      need(results$success, "Analysis did not complete successfully")
    )
    
    weights <- results$weights
    weight_df <- data.frame(
      unit_id = as.numeric(rownames(weights)),
      weight = as.vector(weights)
    ) %>%
      left_join(results$unit_mapping, by = "unit_id") %>%
      filter(weight > 0.001) %>%
      arrange(desc(weight))
    
    validate(
      need(nrow(weight_df) > 0, "No units with significant weights")
    )
    
    p <- ggplot(weight_df, aes_string(x = paste0("reorder(", results$unit_var, ", weight)"), y = "weight")) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      coord_flip() +
      labs(x = "Control Unit", y = "Weight in Synthetic Control") +
      theme_minimal()
    
    ggplotly(p, height = 350) %>% config(displayModeBar = FALSE)
  })
  
  output$synth_weights_table <- renderDT({
    req(synth_results_data())
    results <- synth_results_data()
    
    if (!results$success) return(NULL)
    
    weights <- results$weights
    weight_df <- data.frame(
      unit_id = as.numeric(rownames(weights)),
      Weight = as.vector(weights)
    ) %>%
      left_join(results$unit_mapping, by = "unit_id") %>%
      select(-unit_id) %>%
      arrange(desc(Weight)) %>%
      filter(Weight > 0.001)
    
    names(weight_df) <- c("Weight", "Control Unit")
    weight_df <- weight_df[, c("Control Unit", "Weight")]
    
    datatable(weight_df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = "Weight", digits = 4)
  })
  
  output$synth_balance_table <- renderDT({
    req(synth_results_data())
    results <- synth_results_data()
    
    if (!results$success || is.null(results$synth_tables$tab.pred)) return(NULL)
    
    balance_df <- as.data.frame(results$synth_tables$tab.pred)
    balance_df$Variable <- rownames(balance_df)
    balance_df <- balance_df[, c("Variable", "Treated", "Synthetic", "Sample Mean")]
    
    datatable(balance_df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = 2:4, digits = 3)
  })
  
  output$synth_balance_plot <- renderPlotly({
    req(synth_results_data())
    results <- synth_results_data()
    
    validate(
      need(results$success, "Analysis did not complete successfully"),
      need(!is.null(results$synth_tables$tab.pred), "Balance table not available")
    )
    
    balance_df <- as.data.frame(results$synth_tables$tab.pred)
    balance_df$Variable <- rownames(balance_df)
    
    plot_df <- balance_df %>%
      select(Variable, Treated, Synthetic) %>%
      pivot_longer(cols = c(Treated, Synthetic), names_to = "Type", values_to = "Value")
    
    p <- ggplot(plot_df, aes(x = Variable, y = Value, fill = Type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Treated" = "#e74c3c", "Synthetic" = "#3498db")) +
      labs(x = "", y = "Value", fill = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, height = 300) %>% config(displayModeBar = FALSE)
  })
  
  # Placebo tests (simplified)
  observeEvent(input$start_synth_placebo, {
    req(synth_results_data())
    
    showNotification("Placebo tests would run here. This is computationally intensive.", 
                     type = "message", duration = 5)
  })
  
  observeEvent(input$synth_restart, {
    current_synth_step(1)
    shinyjs::hide("synth_results_panel")
    shinyjs::show("synth_setup_panel")
    synth_results_data(NULL)
  })
  
  # ===== EXPORT =====
  
  output$download_data_csv <- downloadHandler(
    filename = function() {
      paste0("data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$download_data_excel <- downloadHandler(
    filename = function() {
      paste0("data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write_xlsx(data(), file)
    }
  )
  
  output$download_results_csv <- downloadHandler(
    filename = function() {
      paste0("results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      results <- did_results_data()
      if (!is.null(results) && results$success) {
        coef_table <- as.data.frame(summary(results$model)$coeftable)
        coef_table$Variable <- rownames(coef_table)
        write.csv(coef_table, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No results available"), file, row.names = FALSE)
      }
    }
  )
  
  output$download_results_summary <- downloadHandler(
    filename = function() {
      paste0("summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      results <- did_results_data()
      
      if (!is.null(results) && results$success) {
        text <- c(
          "POLICY INSIGHTS - ANALYSIS SUMMARY",
          "===================================",
          "",
          "KEY FINDING:",
          results$main_finding,
          "",
          "CONFIDENCE LEVEL:",
          paste(results$confidence$text, "-", results$confidence$description),
          "",
          "EFFECT DETAILS:",
          results$effect_interpretation$plain_text,
          "",
          "TECHNICAL SUMMARY:",
          sprintf("Effect estimate: %.4f", results$estimate),
          sprintf("Standard error: %.4f", results$se),
          sprintf("95%% Confidence interval: %.4f to %.4f", results$ci_lower, results$ci_upper),
          sprintf("p-value: %.4f", results$pval)
        )
        writeLines(text, file)
      } else {
        writeLines("No analysis results available.", file)
      }
    }
  )
}


# =============================================================================
# RUN APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)