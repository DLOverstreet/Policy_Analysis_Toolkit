# =============================================================================
# EXAMPLE DATASETS FOR POLICY INSIGHTS TOOLKIT
# =============================================================================
# Pre-loaded datasets that users can explore to learn the toolkit
# Each dataset represents a realistic policy analysis scenario
# =============================================================================

library(dplyr)
library(tidyr)

# =============================================================================
# DATASET 1: MINIMUM WAGE POLICY IMPACT
# =============================================================================
# Scenario: Several states raised minimum wage in 2015. Did it affect
# employment rates? This is a classic DiD setup.
# =============================================================================

create_minimum_wage_dataset <- function() {
  set.seed(42)

  # 12 states, 10 years (2010-2019)
  states <- c("California", "New York", "Texas", "Florida", "Illinois",
              "Pennsylvania", "Ohio", "Georgia", "North Carolina", "Michigan",
              "New Jersey", "Virginia")
  years <- 2010:2019

  # Treatment states (raised minimum wage in 2015)
  treated_states <- c("California", "New York", "Illinois", "New Jersey")

  df <- expand.grid(
    State = states,
    Year = years,
    stringsAsFactors = FALSE
  ) %>%
    arrange(State, Year) %>%
    mutate(
      # Treatment indicator
      MinWage_Policy = ifelse(State %in% treated_states & Year >= 2015, 1, 0),

      # Base employment rate with state-specific levels
      state_effect = rnorm(n(), mean = 0, sd = 2)[match(State, unique(State))],

      # Common time trend
      time_trend = (Year - 2010) * 0.3,

      # Treatment effect (small negative effect on employment)
      treatment_effect = ifelse(MinWage_Policy == 1, -1.2, 0),

      # Random noise
      noise = rnorm(n(), 0, 0.8),

      # Employment rate (percentage)
      Employment_Rate = 92 + state_effect + time_trend + treatment_effect + noise,

      # Round to 1 decimal
      Employment_Rate = round(Employment_Rate, 1),

      # Add some covariates
      Population_Millions = case_when(
        State == "California" ~ 38 + (Year - 2010) * 0.4,
        State == "Texas" ~ 26 + (Year - 2010) * 0.5,
        State == "Florida" ~ 19 + (Year - 2010) * 0.3,
        State == "New York" ~ 19 + (Year - 2010) * 0.2,
        TRUE ~ 8 + (Year - 2010) * 0.15
      ),
      Population_Millions = round(Population_Millions, 1),

      # GDP growth rate
      GDP_Growth = 2.5 + rnorm(n(), 0, 1.5),
      GDP_Growth = round(GDP_Growth, 1),

      # Median wage (higher in treated states after policy)
      Median_Wage = 15 + state_effect * 0.5 + (Year - 2010) * 0.3 +
        ifelse(MinWage_Policy == 1, 2.5, 0) + rnorm(n(), 0, 0.5),
      Median_Wage = round(Median_Wage, 2)
    ) %>%
    select(State, Year, Employment_Rate, MinWage_Policy, Population_Millions,
           GDP_Growth, Median_Wage)

  return(df)
}

# =============================================================================
# DATASET 2: EDUCATION PROGRAM EVALUATION
# =============================================================================
# Scenario: 20 schools participated in a new reading program starting in 2018.
# Did student test scores improve?
# =============================================================================

create_education_dataset <- function() {
  set.seed(123)

  # 40 schools, 8 years (2014-2021)
  schools <- paste0("School_", sprintf("%02d", 1:40))
  years <- 2014:2021

  # 20 schools selected for program (treatment group)
  treated_schools <- schools[1:20]

  df <- expand.grid(
    School_ID = schools,
    Year = years,
    stringsAsFactors = FALSE
  ) %>%
    arrange(School_ID, Year) %>%
    mutate(
      # Treatment indicator (program starts in 2018)
      Program_Enrolled = ifelse(School_ID %in% treated_schools & Year >= 2018, 1, 0),

      # School-specific baseline
      school_baseline = rnorm(n(), mean = 0, sd = 8)[match(School_ID, unique(School_ID))],

      # Upward time trend
      time_trend = (Year - 2014) * 1.5,

      # Treatment effect (positive effect on scores)
      treatment_effect = ifelse(Program_Enrolled == 1, 5.5, 0),

      # Random variation
      noise = rnorm(n(), 0, 3),

      # Reading test scores (0-100 scale)
      Reading_Score = 65 + school_baseline + time_trend + treatment_effect + noise,
      Reading_Score = pmin(100, pmax(0, round(Reading_Score, 1))),

      # Math scores (not affected by reading program)
      Math_Score = 68 + school_baseline + time_trend * 0.8 + rnorm(n(), 0, 3),
      Math_Score = pmin(100, pmax(0, round(Math_Score, 1))),

      # Covariates
      Student_Count = round(rnorm(n(), 450, 80)[match(School_ID, unique(School_ID))]),

      Low_Income_Pct = round(runif(n(), 15, 75)[match(School_ID, unique(School_ID))], 1),

      Teacher_Student_Ratio = round(runif(n(), 12, 22)[match(School_ID, unique(School_ID))], 1),

      # Attendance rate
      Attendance_Rate = round(92 + rnorm(n(), 0, 2), 1)
    ) %>%
    select(School_ID, Year, Reading_Score, Math_Score, Program_Enrolled,
           Student_Count, Low_Income_Pct, Teacher_Student_Ratio, Attendance_Rate)

  return(df)
}

# =============================================================================
# DATASET 3: HEALTHCARE POLICY - MEDICAID EXPANSION
# =============================================================================
# Scenario: Some states expanded Medicaid in 2014 (ACA). Did uninsured rates drop?
# Classic policy evaluation with staggered adoption.
# =============================================================================

create_healthcare_dataset <- function() {
  set.seed(789)

  # 30 states, 10 years (2010-2019)
  states <- c(
    "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
    "Delaware", "Hawaii", "Illinois", "Iowa", "Kentucky",
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York", "North Dakota",
    "Ohio", "Oregon", "Pennsylvania", "Rhode Island", "Vermont",
    "Washington", "West Virginia", "Alabama", "Florida", "Georgia"
  )
  years <- 2010:2019

  # States that expanded Medicaid in 2014 (first 20)
  expansion_states <- states[1:20]

  df <- expand.grid(
    State = states,
    Year = years,
    stringsAsFactors = FALSE
  ) %>%
    arrange(State, Year) %>%
    mutate(
      # Treatment indicator
      Medicaid_Expansion = ifelse(State %in% expansion_states & Year >= 2014, 1, 0),

      # State baseline uninsured rate
      state_baseline = rnorm(n(), mean = 0, sd = 2)[match(State, unique(State))],

      # General declining trend in uninsured (national trend)
      time_trend = -(Year - 2010) * 0.3,

      # Strong treatment effect (expansion reduces uninsured)
      treatment_effect = ifelse(Medicaid_Expansion == 1, -4.5, 0),

      # Noise
      noise = rnorm(n(), 0, 0.5),

      # Uninsured rate (percentage)
      Uninsured_Rate = 15 + state_baseline + time_trend + treatment_effect + noise,
      Uninsured_Rate = pmax(2, round(Uninsured_Rate, 1)),

      # Healthcare spending per capita
      Healthcare_Spending = 8000 + state_baseline * 200 + (Year - 2010) * 350 +
        ifelse(Medicaid_Expansion == 1, 1200, 0) + rnorm(n(), 0, 300),
      Healthcare_Spending = round(Healthcare_Spending, 0),

      # Hospital visits per 1000 people
      Hospital_Visits = 250 + state_baseline * 5 +
        ifelse(Medicaid_Expansion == 1, 15, 0) + rnorm(n(), 0, 10),
      Hospital_Visits = round(Hospital_Visits, 0),

      # Preventive care rate
      Preventive_Care_Pct = 60 + state_baseline * 0.5 + (Year - 2010) * 0.8 +
        ifelse(Medicaid_Expansion == 1, 5, 0) + rnorm(n(), 0, 2),
      Preventive_Care_Pct = round(Preventive_Care_Pct, 1),

      # Population in millions
      Population_Millions = case_when(
        State == "California" ~ 37 + (Year - 2010) * 0.4,
        State == "Florida" ~ 19 + (Year - 2010) * 0.3,
        State == "New York" ~ 19 + (Year - 2010) * 0.1,
        TRUE ~ 6 + (Year - 2010) * 0.1
      ),
      Population_Millions = round(Population_Millions, 1)
    ) %>%
    select(State, Year, Uninsured_Rate, Medicaid_Expansion, Healthcare_Spending,
           Hospital_Visits, Preventive_Care_Pct, Population_Millions)

  return(df)
}

# =============================================================================
# DATASET 4: CARBON TAX POLICY
# =============================================================================
# Scenario: 5 states implemented a carbon tax in 2016. Did CO2 emissions decrease?
# Good for synthetic control (small number of treated units)
# =============================================================================

create_carbon_tax_dataset <- function() {
  set.seed(456)

  # 25 states, 12 years (2010-2021)
  states <- c(
    "California", "Washington", "Oregon", "Massachusetts", "New York",
    "Texas", "Florida", "Pennsylvania", "Ohio", "Illinois",
    "Michigan", "Georgia", "North Carolina", "New Jersey", "Virginia",
    "Arizona", "Indiana", "Tennessee", "Missouri", "Maryland",
    "Wisconsin", "Minnesota", "Colorado", "Alabama", "South Carolina"
  )
  years <- 2010:2021

  # States with carbon tax (implemented 2016)
  carbon_tax_states <- c("California", "Washington", "Oregon", "Massachusetts", "New York")

  df <- expand.grid(
    State = states,
    Year = years,
    stringsAsFactors = FALSE
  ) %>%
    arrange(State, Year) %>%
    mutate(
      # Treatment indicator
      Carbon_Tax = ifelse(State %in% carbon_tax_states & Year >= 2016, 1, 0),

      # State-specific emission levels
      state_baseline = rnorm(n(), mean = 0, sd = 15)[match(State, unique(State))],

      # Slight declining trend (efficiency improvements)
      time_trend = -(Year - 2010) * 0.8,

      # Treatment effect (tax reduces emissions)
      treatment_effect = ifelse(Carbon_Tax == 1, -8, 0),

      # Noise
      noise = rnorm(n(), 0, 3),

      # CO2 emissions (million metric tons)
      CO2_Emissions = 150 + state_baseline + time_trend + treatment_effect + noise,
      CO2_Emissions = pmax(50, round(CO2_Emissions, 1)),

      # Renewable energy percentage
      Renewable_Pct = 15 + (Year - 2010) * 1.5 +
        ifelse(Carbon_Tax == 1, 8, 0) + rnorm(n(), 0, 2),
      Renewable_Pct = pmin(60, pmax(5, round(Renewable_Pct, 1))),

      # Energy price index
      Energy_Price_Index = 100 + (Year - 2010) * 2 +
        ifelse(Carbon_Tax == 1, 12, 0) + rnorm(n(), 0, 3),
      Energy_Price_Index = round(Energy_Price_Index, 1),

      # Industrial GDP (billions)
      Industrial_GDP = 80 + state_baseline * 2 + (Year - 2010) * 3 + rnorm(n(), 0, 5),
      Industrial_GDP = round(Industrial_GDP, 1),

      # Population
      Population_Millions = case_when(
        State == "California" ~ 37 + (Year - 2010) * 0.4,
        State == "Texas" ~ 25 + (Year - 2010) * 0.5,
        State == "Florida" ~ 19 + (Year - 2010) * 0.3,
        State == "New York" ~ 19 + (Year - 2010) * 0.1,
        TRUE ~ 6 + (Year - 2010) * 0.1
      ),
      Population_Millions = round(Population_Millions, 1)
    ) %>%
    select(State, Year, CO2_Emissions, Carbon_Tax, Renewable_Pct,
           Energy_Price_Index, Industrial_GDP, Population_Millions)

  return(df)
}

# =============================================================================
# DATASET 5: JOB TRAINING PROGRAM
# =============================================================================
# Scenario: 100 participants enrolled in job training, 100 control group.
# Did their wages increase? Individual-level panel data.
# =============================================================================

create_job_training_dataset <- function() {
  set.seed(999)

  # 200 individuals, 5 years (2017-2021)
  individuals <- paste0("ID_", sprintf("%04d", 1:200))
  years <- 2017:2021

  # First 100 enrolled in training (starts 2019)
  treated_individuals <- individuals[1:100]

  df <- expand.grid(
    Participant_ID = individuals,
    Year = years,
    stringsAsFactors = FALSE
  ) %>%
    arrange(Participant_ID, Year) %>%
    mutate(
      # Treatment indicator (training in 2019)
      Training_Enrolled = ifelse(Participant_ID %in% treated_individuals & Year >= 2019, 1, 0),

      # Individual baseline wage
      individual_baseline = rnorm(n(), mean = 0, sd = 5000)[match(Participant_ID, unique(Participant_ID))],

      # Time trend (wage growth)
      time_trend = (Year - 2017) * 1200,

      # Treatment effect (training increases wages)
      treatment_effect = ifelse(Training_Enrolled == 1, 6500, 0),

      # Noise
      noise = rnorm(n(), 0, 1500),

      # Annual wage
      Annual_Wage = 35000 + individual_baseline + time_trend + treatment_effect + noise,
      Annual_Wage = pmax(20000, round(Annual_Wage, 0)),

      # Employment status (higher for trained)
      Employed = ifelse(Training_Enrolled == 1,
                       rbinom(n(), 1, 0.92),
                       rbinom(n(), 1, 0.78)),

      # Hours worked per week
      Hours_Per_Week = ifelse(Employed == 1,
                             round(35 + rnorm(n(), 0, 5)),
                             0),
      Hours_Per_Week = pmax(0, pmin(60, Hours_Per_Week)),

      # Demographics (fixed characteristics)
      Age = round(25 + rnorm(n(), 0, 8)[match(Participant_ID, unique(Participant_ID))]),
      Age = pmax(18, pmin(65, Age)),

      Education_Years = round(12 + rnorm(n(), 0, 2)[match(Participant_ID, unique(Participant_ID))]),
      Education_Years = pmax(8, pmin(20, Education_Years)),

      # Prior experience
      Years_Experience = pmax(0, round((Year - 2017) +
                                      rnorm(n(), 2, 1.5)[match(Participant_ID, unique(Participant_ID))]))
    ) %>%
    select(Participant_ID, Year, Annual_Wage, Training_Enrolled, Employed,
           Hours_Per_Week, Age, Education_Years, Years_Experience)

  return(df)
}

# =============================================================================
# METADATA FOR EACH DATASET
# =============================================================================

get_dataset_metadata <- function() {
  list(
    minimum_wage = list(
      name = "Minimum Wage Policy Impact",
      description = "12 US states from 2010-2019. Four states raised minimum wage in 2015. Analyze the impact on employment rates.",
      icon = "dollar-sign",
      difficulty = "Beginner",
      best_for = "Difference-in-Differences",
      key_variables = "Employment_Rate (outcome), MinWage_Policy (treatment), State (unit), Year (time)",
      rows = 120,
      suggested_analysis = "Compare employment trends between states that raised minimum wage vs. those that didn't.",
      color = "green"
    ),

    education = list(
      name = "School Reading Program Evaluation",
      description = "40 schools from 2014-2021. Twenty schools adopted a new reading program in 2018. Did test scores improve?",
      icon = "book",
      difficulty = "Beginner",
      best_for = "Difference-in-Differences",
      key_variables = "Reading_Score (outcome), Program_Enrolled (treatment), School_ID (unit), Year (time)",
      rows = 320,
      suggested_analysis = "Evaluate whether the reading program improved test scores compared to control schools.",
      color = "blue"
    ),

    healthcare = list(
      name = "Medicaid Expansion Impact",
      description = "30 US states from 2010-2019. Twenty states expanded Medicaid in 2014 under the ACA. Impact on uninsured rates.",
      icon = "heartbeat",
      difficulty = "Intermediate",
      best_for = "Difference-in-Differences",
      key_variables = "Uninsured_Rate (outcome), Medicaid_Expansion (treatment), State (unit), Year (time)",
      rows = 300,
      suggested_analysis = "Assess how Medicaid expansion affected the percentage of uninsured residents.",
      color = "red"
    ),

    carbon_tax = list(
      name = "Carbon Tax Environmental Impact",
      description = "25 US states from 2010-2021. Five states implemented a carbon tax in 2016. Effect on CO2 emissions.",
      icon = "leaf",
      difficulty = "Advanced",
      best_for = "Synthetic Control",
      key_variables = "CO2_Emissions (outcome), Carbon_Tax (treatment), State (unit), Year (time)",
      rows = 300,
      suggested_analysis = "Use synthetic control to analyze emissions trends in carbon tax states vs. synthetic comparison.",
      color = "green"
    ),

    job_training = list(
      name = "Job Training Program Outcomes",
      description = "200 individuals from 2017-2021. Half enrolled in job training in 2019. Impact on wages and employment.",
      icon = "briefcase",
      difficulty = "Intermediate",
      best_for = "Difference-in-Differences",
      key_variables = "Annual_Wage (outcome), Training_Enrolled (treatment), Participant_ID (unit), Year (time)",
      rows = 1000,
      suggested_analysis = "Compare wage growth for trained participants versus control group.",
      color = "purple"
    )
  )
}

# =============================================================================
# MAIN FUNCTION TO GET EXAMPLE DATASET
# =============================================================================

get_example_dataset <- function(dataset_name) {
  dataset <- switch(
    dataset_name,
    "minimum_wage" = create_minimum_wage_dataset(),
    "education" = create_education_dataset(),
    "healthcare" = create_healthcare_dataset(),
    "carbon_tax" = create_carbon_tax_dataset(),
    "job_training" = create_job_training_dataset(),
    NULL
  )

  return(dataset)
}
