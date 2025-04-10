library(tidyr)

###############################################################
###  Process Education Data for Age of School Entrance Analysis
###############################################################

#' This function calculate school entrance ages, and filtering individuals based on specified age ranges.
#'
#' @param educ_data A data frame containing individual education data. Must include columns `caseid`, `educ.yrs`, and `in.school`.
#' @param raw_IR_dat A data frame containing raw IR recode data. Must include columns `caseid`, `v008` (survey time in CMC format), and `v011` (date of birth in CMC format).
#' @param start_month An integer specifying the academic year start month for this country
#' @param age_range A numeric vector of length 2 specifying the valid range of ages for school entrance (default is c(5, 15)).
#' 
#' @return A data frame containing processed data with calculated and filtered school entrance information. 
#' Includes additional columns such as `school_start`, `age_start_educ`, and `entrance_age_rev`.
#'
#' @details
#' This function performs the following steps:
#' - Merges the education data with interview data based on `caseid`.
#' - Removes individuals who never attended school.
#' - Calculates the nearest academic year start month.
#' - Computes the approximate age at the start of education.
#' - Adjusts and filters ages based on the provided `age_range`.
#' - Reverse engineers the school entrance age relative to the upper age boundary.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' processed_data <- process_education_data(educ_data = educ_dat_ind, 
#'                                          raw_IR_dat = raw.IR.dat, 
#'                                          start_month = 10, 
#'                                          age_range = c(5, 15))
#' }
#' 
#' 
#' @export
process_start_educ_age <- function(educ_IR_merged_dat, start_month = 10, age_range = c(5, 15)) {
  
  
  ### !!! very important: valid range: age_range
  ### !!! very important: if not start school at age age_upper, assume will never start 
  ### (not in study population and thus no need to worry about right censor)
  
  # Extract age range boundaries
  age_lower <- age_range[1]
  age_upper <- age_range[2]
  
  # Remove individuals who never attended school
  filtered_data <- educ_IR_merged_dat %>%
    filter(!(educ.yrs == 0 & in.school == 0))
  
  # Calculate the nearest academic year start month
  filtered_data <- filtered_data %>%
    mutate(school_start = case_when(
      survey_time_cmc %% 12 < start_month ~ floor(survey_time_cmc / 12) * 12 - 12 + start_month, # Previous year
      TRUE ~ floor(survey_time_cmc / 12) * 12 + start_month                                       # Same year
    ))
  
  # Calculate the approximate age at the start of education
  filtered_data <- filtered_data %>%
    mutate(age_start_educ = floor((school_start - educ.yrs * 12 - dob_cmc) / 12))
  
  # Adjust and filter age of school entrance based on defined boundaries
  filtered_data <- filtered_data %>%
    mutate(age_start_educ = case_when(
      age_start_educ < age_lower ~ age_lower,  # Adjust extreme cases to lower boundary
      TRUE ~ age_start_educ                    # Keep valid ages as-is
    )) %>%
    filter(age_start_educ >= age_lower & age_start_educ <= age_upper)  # Filter by age range
  
  # Reverse engineer the entrance age relative to the upper age boundary
  filtered_data <- filtered_data %>%
    mutate(entrance_age_rev = age_upper - age_start_educ)
  
  # Return the processed dataset
  return(filtered_data)
}



###############################################################
###  split education history for discrete time hazard
###############################################################

#' split education history into single year segments
#'
#' @param data DHS IR recode raw data.
#' @param compact Whether to aggregate data (based on cluster etc.)
#' @param compact.by Variable for the aggregation
#' @param account.censor Whether to account for censor, i.e. whether treat survey year as stop receiving education


get_start_year_rev_splitted <- function(data = NULL,
                             compact = T, 
                             account.censor=T,
                             compact.by =  c('v001', "weights")
) {
  
  splitted.educ <- data %>%
    rowwise() %>%  # Enable row-wise operations
    mutate(
      # Dropout indicator
      start_grade = list(seq(0, entrance_age_rev)),
      end_grade = list(seq(1, entrance_age_rev+1))
    )%>%
    unnest(cols = c(start_grade, end_grade)) %>%
    ungroup() %>% 
    mutate(
      drop = ifelse(end_grade > entrance_age_rev, 1, 0),
    )   
  
  if(account.censor){
    splitted.educ <- splitted.educ %>% filter( !(end_grade > entrance_age_rev&in.school==0))
  }
  
  splitted.educ <- splitted.educ[splitted.educ$end_grade<= max(splitted.educ$entrance_age_rev),]
  
  ### specify each period
  splitted.educ$bin <- paste0(splitted.educ$start_grade,'-',splitted.educ$end_grade)
  splitted.educ$grade <- factor(as.character(splitted.educ$bin),levels=unique(splitted.educ$bin))
  
  ### if not compact
  if(!compact){
    return(splitted.educ)
  }
  
  ### compact by birth cohort/region/etc.
  #splitted.educ$birth.year <- splitted.educ$v010
  
  splitted.educ.aggre <- splitted.educ[, c(compact.by, "grade", "drop")]
  
  splitted.educ.aggre$total <- 1
  
  formula <- as.formula(paste0(".~ grade+ ", paste(compact.by, collapse = " + ")))
  
  splitted.educ.comp <- stats::aggregate(formula, data = splitted.educ.aggre, FUN = sum, drop = TRUE)
  
  splitted.educ.comp$drop_frac <- splitted.educ.comp$drop/splitted.educ.comp$total
  splitted.educ.comp$Y <- splitted.educ.comp$drop
  
  if('birth.year' %in% compact.by){
    splitted.educ.comp$birth.year.int <- splitted.educ.comp$birth.year-min(splitted.educ.comp$birth.year)+1
  }
  
  splitted.educ.comp$cluster <- splitted.educ.comp$v001
  
  return(splitted.educ.comp)
  
  
}









###############################################################
###  calculate distribution of start age 
###############################################################

#' This function computes the distribution of entrance ages, 
#' and adjusts the data to account for censoring and other factors.
#'
#' @param educ_start_data A data frame containing information about individuals' education start details.
#' Must include columns required by `get_start_year_rev_splitted`.
#' @param upper_age_limit An integer specifying the upper limit for age at the start of education (default is `15`).
#'
#' @return A data frame containing the adjusted entrance age distribution. 
#' Columns include:
#' - `age_start_educ`: Age at the start of education.
#' - `prob`: Probability distribution of entrance ages.
#'
#' @details
#' This function:
#' - Uses `get_start_year_rev_splitted` to process and adjust entrance data.
#' - Computes the survival probability distribution for reverse engineered starting education age
#' - Derives the probability of starting education at specific ages (`P(T = t)`).
#' - Adjusts the distribution to include a terminal row representing the cumulative probability.
#' - Returns the cleaned and adjusted distribution data frame.
#'
#' @examples
#' \dontrun{
#' cal_start_age_dist <- process_entrance_data(
#'   educ_start_data = educ_start_ind,
#'   upper_age_limit = 15
#' )
#' }
#'
#' @export
cal_start_age_dist <- function(educ_start_data, 
                                  upper_age_limit = 15) {
  
  # Step 1: prepare data in continuation odds model using `get_start_year_rev_splitted`
  splitted_data <- get_start_year_rev_splitted(
    data = educ_start_data,
    compact = T, 
    account.censor= T,
    compact.by =  c('v001','v022', "weights")
  )
  
  # Add strata information
  splitted_data$strata <- splitted_data$v022
  
  # Step 2: Compute survival probabilities using `getEduc.Direct`
  education_results <- getEduc.Direct(splitted_data)
  survival_prob <- education_results$surv_p
  
  # Step 3: Compute entrance age distribution (reverse engineered and transform back)
  entrance_age_distribution <- survival_prob %>%
    mutate(
      dist_p = mean - lead(mean, default = 0),  # Compute P(T = t)
      age_start_educ = upper_age_limit - grade  # Calculate starting age for education
    ) %>% # Add row for last age
    add_row(
      grade = 0, 
      dist_p = 1 - max(survival_prob$mean), 
      age_start_educ = upper_age_limit
    ) %>% 
    arrange(age_start_educ) %>%           # Ensure rows are ordered by age_start_educ
    select(age_start_educ, dist_p) %>%    # Keep relevant columns
    rename(prob = dist_p)                 # Rename dist_p to prob
  
  # Return the reversed back entrance age distribution
  return(entrance_age_distribution)
  
}





###############################################################
###  simulate censorship in the existing data
###############################################################


#' Simulate Randomly Censored Education Data
#'
#' This function simulates censored education years data by randomly sampling entrance ages, with specified hypothetical survey time
#' calculating key education milestones (start and end dates), and determining education status at a simulated survey time.
#'
#' @param educ_data A data frame containing individual education data. Must include columns `dob_cmc`, `educ.yrs`, and `survey_time_cmc`.
#' @param entrance_age_distribution A data frame containing the distribution of entrance ages. Must include columns `age_start_educ` and `prob`.
#' @param start_month An integer specifying the academic year start month (default is 10, for October).
#' @param sim_survey_cmc Hypothetical survey time to simulate censorship, default is 10 years before the actual survey time.
#'
#' @return A data frame with the following simulated columns added:
#' - `sim_entrance_age`: Simulated entrance age based on the given distribution.
#' - `sim_start_educ_cmc`: Simulated start time of education in CMC format.
#' - `sim_end_educ_cmc`: Simulated end time of education in CMC format.
#' - `sim_svy_cmc`: Simulated survey time in CMC format.
#' - `sim_in_school`: Indicator (1/0) for whether the individual is in school at the simulated survey time.
#' - `sim_educ_yrs`: Simulated years of education completed by the simulated survey time.
#'
#' @details
#' The function performs the following steps:
#' - Samples entrance ages from the provided distribution.
#' - Computes the start and end times of education in CMC format.
#' - Simulates a survey time based on the average survey time in the data, adjusted by a fixed offset.
#' - Determines whether individuals are in school at the simulated survey time.
#' - Calculates the simulated years of education completed.
#'
#' @examples
#' \dontrun{
#' simulated_data <- simulate_educ_censoring(
#'   educ_data = test_educ,
#'   entrance_age_distribution = entrance_age_dist,
#'   start_month = 10,
#'   sim_survey_cmc= NULL
#' )
#' }
#'
#' @export
simulate_educ_censoring <- function(educ_data, entrance_age_distribution, start_month,sim_survey_cmc=NULL) {
  
  # Step 1: Sample simulated entrance ages based on the distribution
  educ_data <- educ_data %>%
    mutate(sim_entrance_age = sample(
      entrance_age_distribution$age_start_educ,
      size = n(),
      replace = TRUE,
      prob = entrance_age_distribution$prob
    ))
  
  # Step 2: Calculate simulated start time of education in CMC format
  educ_data <- educ_data %>%
    mutate(sim_start_educ_cmc = case_when(
      dob_cmc %% 12 < start_month ~ floor(dob_cmc / 12) * 12 + sim_entrance_age * 12 + start_month, # Previous year
      TRUE ~ ceiling(dob_cmc / 12) * 12 + sim_entrance_age * 12 + start_month                      # Same year
    ))
  
  # Step 3: Calculate simulated end time of education in CMC format
  educ_data <- educ_data %>%
    mutate(sim_end_educ_cmc = sim_start_educ_cmc + educ.yrs * 12)
  
  # Step 4: Simulate survey time in CMC format (make sure youngest are >15 age when surveyed)
  if(is.null(sim_survey_cmc)){
    sim_survey_cmc= max(educ_data$dob_cmc)+12*15+1
  }
  
  educ_data <- educ_data %>%
    mutate(sim_svy_cmc = sim_survey_cmc)
  
  # Step 5: Determine whether individuals are in school at the simulated survey time
  educ_data <- educ_data %>%
    mutate(sim_in_school = as.numeric(sim_svy_cmc < sim_end_educ_cmc))
  
  # Step 6: Calculate simulated years of education completed
  educ_data <- educ_data %>%
    mutate(sim_educ_yrs = case_when(
      sim_in_school == 1 ~ floor((sim_svy_cmc - sim_start_educ_cmc) / 12), # Still in school
      TRUE ~ educ.yrs                                                      # Finished school
    )) %>%
    mutate(sim_educ_yrs = case_when(sim_educ_yrs < 0 ~ 0,
                                    TRUE~sim_educ_yrs))
  
  # Return the simulated data
  return(educ_data)
}
















###############################################################
###  simulate censored data and replace with original
###############################################################

#' Simulate and Combine Censored Education Data
#'
#' This function simulates censored education data for a specified subset of the dataset, 
#' updates the simulated values into the full dataset, and adjusts the education variables 
#' based on the simulation results.
#'
#' @param educ_all A data frame containing the full education dataset. Must include `caseid` and columns for education and survey details.
#' @param educ_to_sim A data frame containing the subset of data to be simulated. Must be a subset of `educ_all`.
#' @param entrance_age_distribution A data frame with the entrance age distribution. Must include columns `age_start_educ` and `prob`.
#' @param start_month An integer specifying the academic year start month (default is 10, for October).
#' @param sim_survey_cmc A numeric value specifying the simulated survey time in CMC format. If `NULL` (default), the survey time is estimated from the data.
#'
#' @return A data frame with updated simulated values for the specified subset, integrated into the full dataset.
#' Includes columns:
#' - `sim_entrance_age`: Simulated entrance age based on the given distribution.
#' - `sim_start_educ_cmc`: Simulated start time of education in CMC format.
#' - `sim_end_educ_cmc`: Simulated end time of education in CMC format.
#' - `sim_svy_cmc`: Simulated survey time in CMC format.
#' - `sim_in_school`: Indicator (1/0) for whether the individual is in school at the simulated survey time.
#' - `sim_educ_yrs`: Simulated years of education completed by the simulated survey time.
#' - Adjusted `educ.yrs` and `in.school` based on the simulated data.
#'
#' @details
#' This function:
#' - Simulates entrance age and related variables for a subset of the data using `simulate_educ_censoring`.
#' - Combines the simulated subset with the full dataset.
#' - Updates education variables (`educ.yrs` and `in.school`) for rows in the subset.
#'
#' @examples
#' \dontrun{
#' combined_data <- simulate_and_combine(
#'   educ_all = full_data,
#'   educ_to_sim = subset_data,
#'   entrance_age_distribution = entrance_age_dist,
#'   start_month = 10,
#'   sim_survey_cmc = NULL
#' )
#' }
#'
#' @export
simulate_censor_cohort <- function(educ_all, 
                                   educ_to_sim, 
                                   entrance_age_distribution,
                                   start_month, 
                                   sim_survey_cmc = NULL) {
  
  # Step 1: Simulate censored values for the specified subset
  educ_simulated_part <- simulate_educ_censoring(
    educ_data = educ_to_sim,
    entrance_age_distribution = entrance_age_distribution,
    start_month = start_month,
    sim_survey_cmc = sim_survey_cmc
  )
  
  # Step 2: Add columns to the full dataset and initialize as NA
  educ_sim_combined <- educ_all %>%
    mutate(
      sim_entrance_age = as.numeric(NA),        # Ensure numeric type
      sim_start_educ_cmc = as.numeric(NA),     # Ensure numeric type
      sim_end_educ_cmc = as.numeric(NA),       # Ensure numeric type
      sim_svy_cmc = as.numeric(NA),            # Ensure numeric type
      sim_in_school = as.numeric(NA),          # Ensure numeric type
      sim_educ_yrs = as.numeric(NA)            # Ensure numeric type
    )
  
  # Step 3: Update the rows for the subset with simulated values
  educ_sim_combined <- educ_sim_combined %>%
    rows_update(educ_simulated_part, by = c("caseid"))  # Match on unique identifier (e.g., `caseid`)
  
  # Step 4: Update education variables with simulated values
  educ_sim_combined <- educ_sim_combined %>%
    mutate(
      educ.yrs = case_when(!is.na(sim_educ_yrs) ~ sim_educ_yrs, TRUE ~ educ.yrs),
      in.school = case_when(!is.na(sim_in_school) ~ sim_in_school, TRUE ~ in.school)
    )
  
  # Return the combined dataset
  return(educ_sim_combined)
}
