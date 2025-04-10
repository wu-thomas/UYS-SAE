###########################################################################
#########  delta method for survival, conditional prob, grade dist and mst
###########################################################################


est_from_delta <- function(betas,
                           grade_cut,
                           grade_intervals,
                           V_all_betas) {
  
  
  cond_probs <- expit(betas)
  n.grades <- length(betas)
  p_survival <- unlist(lapply(1:n.grades, function(y) prod((1 - cond_probs[1:y]))))
  p_grades <- c(1,p_survival)- c(p_survival,0)
  
  ############################################
  ## Calculate mean/variance for P(T >= i)
  ############################################
  
  # Gradient function for each P(T >= i) with respect to each beta
  gradient_func_surv <- function(i, betas, p_survival) {
    grads <- numeric(length(betas))
    for (j in 1:length(betas)) {
      # Derivative of log(1 - p_j) with respect to beta_j
      grads[j] <- -p_survival[i] *  expit(betas[j])* (i >= j)
    }
    return(grads)
  }
  
  # Calculating the variance of each P(T >= i)
  var_p_survival <- sapply(1:n.grades, function(i) {
    surv_grads <- gradient_func_surv(i, betas, p_survival)
    t(surv_grads) %*% V_all_betas %*% surv_grads
  })
  
  # Calculate lower and upper confidence bounds for p_survival
  p_survival_lower <- pmax(0, p_survival + stats::qnorm(0.025) * sqrt(var_p_survival))
  p_survival_upper <- pmin(1, p_survival + stats::qnorm(0.975) * sqrt(var_p_survival))
  
  # Create data frame for p_survival
  p_survival_frame <- data.frame(
    grade = grade_cut,
    mean = p_survival,
    lower = p_survival_lower,
    upper = p_survival_upper,
    sd = sqrt(var_p_survival)
  )
  
  
  ############################################
  ## Calculate mean/variance for P(T = i)
  ############################################
  
  # Gradient function for each P(T = i) with respect to each beta
  gradient_func_grade_dist <- function(i, betas, p_survival) {
    
    grads <- numeric(length(betas))
    
    if(i==0){
      # 1- prob(t>=1)
      for (j in 1:length(betas)) {
        grads[j] <- (-(-p_survival[1] *  expit(betas[j])* (1 >= j)))
      }
      return(grads)
      
    }
    
    for (j in 1:length(betas)) {
      # Derivative of log(1 - p_j) with respect to beta_j
      grads[j] <- -p_survival[i] *  expit(betas[j])* (i >= j)
      if(i<length(betas)){
        grads[j] <- grads[j] - (-p_survival[i+1] *  expit(betas[j])* ((i+1) >= j))
      }
    }
    return(grads)
  }
  
  # Calculating the variance of each P(T = i)
  var_p_grades <- sapply(0:n.grades, function(i) {
    p_dist_grads <- gradient_func_grade_dist(i, betas, p_survival)
    t(p_dist_grads) %*% V_all_betas %*% p_dist_grads
  })
  
  # Calculate lower and upper confidence bounds for p_grades
  p_grades_lower <- pmax(0, p_grades + stats::qnorm(0.025) * sqrt(var_p_grades))
  p_grades_upper <- pmin(1, p_grades + stats::qnorm(0.975) * sqrt(var_p_grades))
  
  # Create data frame for p_grades
  p_grade_frame <- data.frame(
    grade = c(0,grade_cut),
    mean = p_grades,
    lower = p_grades_lower,
    upper = p_grades_upper,
    sd = sqrt(var_p_grades)
  )
  
  
  ##########################
  ## Calculate mean/variance for conditional probabilities (p_continue)
  ##########################
  
  # Calculate variance for conditional probabilities
  var_cond_probs <- cond_probs * (1 - cond_probs) * diag(V_all_betas) * cond_probs * (1 - cond_probs)
  
  # Calculate lower and upper confidence bounds for cond_probs
  cond_probs_lower <- pmax(0, cond_probs + stats::qnorm(0.025) * sqrt(var_cond_probs))
  cond_probs_upper <- pmin(1, cond_probs + stats::qnorm(0.975) * sqrt(var_cond_probs))
  
  # Create data frame for conditional survival probabilities
  cond_probs_frame <- data.frame(
    grade = names(betas),
    grade_int = grade_cut,
    mean = cond_probs,
    lower = cond_probs_lower,
    upper = cond_probs_upper,
    sd = sqrt(var_cond_probs)
  )
  
  ##########################
  ## Calculate mean/variance for education years
  ##########################
  
  # Estimate mean for education years
  mean_educ_years <- sum(p_survival * grade_intervals)
  
  # Derivatives for each beta in sum P(Y>=i)
  deriv_beta_educ_years <- function(i) {
    -expit(betas[i]) * sum(p_survival[i:length(p_survival)] * grade_intervals[i:length(p_survival)])
  }
  
  # Calculate all beta derivatives
  deriv_all_betas_educ_years <- sapply(1:length(p_survival), deriv_beta_educ_years)
  
  # Calculate variance for education years
  var_educ_years <- t(deriv_all_betas_educ_years) %*% V_all_betas %*% deriv_all_betas_educ_years
  
  # Calculate lower and upper confidence bounds for education years
  lims_educ_years <- mean_educ_years + stats::qnorm(c(0.025, 0.975)) * sqrt(c(var_educ_years))
  
  # Create result frame for education years
  educ_yrs_res <- data.frame(
    mean = mean_educ_years,
    lower = lims_educ_years[1],
    upper = lims_educ_years[2],
    sd = sqrt(var_educ_years)
  )
  
  ##########################
  ## Compile results
  ##########################
  
  res_obj <- list(
    surv_p = p_survival_frame,
    grade_dist_p = p_grade_frame,
    cond_surv_p = cond_probs_frame,
    educ_years = educ_yrs_res
  )
  
  return(res_obj)
}


###########################################################################
#########  survey weighted (direct) estimates for a (sub)population
###########################################################################

#' discrete-time hazard modelling of education years
#' might be biased when only looking at most recent birth cohorts

getEduc.Direct<- function(educ.comp= NULL,dhs_design=NULL){
  
  # set up design
  if(is.null(dhs_design)){
    dhs_design <-survey::svydesign(ids=~v001, strata=~strata, data=educ.comp, nest = T,weights=~weights)
  }
  
  # survey GLM for continuation probability
  glm.educ <- survey::svyglm(drop_frac ~ (-1) + grade, design = dhs_design, 
                             family = stats::quasibinomial, 
                             maxit = 50, weights = educ.comp$total)
  
  # number of grades, corresponds to betas
  n.grades <- length(summary(glm.educ)$coefficient[,1])
  grade.cut <-   as.numeric(sapply(strsplit(rownames(summary(glm.educ)$coefficient),"-"), `[`, 2))
  grade.intervals <- grade.cut[1:(n.grades)]-c(0,grade.cut[1:(n.grades-1)]) # years in each bin
  
  # extract parameters
  betas <- summary(glm.educ)$coefficient[1:n.grades,1]
  
  # dropout probability
  #probs <- expit(betas)
  
  # covariance matrix
  V.all.betas <- stats::vcov(glm.educ)[1:n.grades,1:n.grades]
  
  # cumulative probability for continuation
  #p_grades <- unlist(lapply(1:n.grades, function(y) prod((1 - probs[1:y]))))
  
  
  # calculate results 
  res_obj <- est_from_delta(betas=betas,
                      grade_cut=grade.cut,
                      grade_intervals=grade.intervals,
                      V_all_betas=V.all.betas) 
  
  return(res_obj)
  
}




getEduc.Direct.group <- function(splitted.educ = NULL, 
                                 group.var = NULL, 
                                 result.obj.names = c('surv_p','cond_surv_p','grade_dist_p','educ_years')) {
  
  # Identify unique group levels
  group.levels <- unique(splitted.educ[[group.var]])
  splitted.educ$group.var <- splitted.educ[[group.var]]
  
  # Create survey design
  dhs.design <- survey::svydesign(
    ids = ~v001, 
    strata = ~strata, 
    data = splitted.educ, 
    nest = TRUE, 
    weights = ~weights
  )
  
  # Initialize an empty list to store results
  result.obj <- list()
  
  for (i in seq_along(group.levels)) {
    # Subset the design and data for the current group
    tmp.design <- subset(dhs.design, group.var == group.levels[i])
    tmp.data <- subset(splitted.educ, group.var == group.levels[i])
    
    # Try to get results for the current group
    tmp.res <- tryCatch(
      {
        getEduc.Direct(educ.comp = tmp.data, dhs_design = tmp.design)
      },
      error = function(e) {
        message(paste0('Encountered error at ', group.levels[i], '\n',
                       'Error message is: ', e, '\n'))
        return(NULL)
      }
    )
    
    if (is.null(tmp.res)) next
    
    # Iterate over the specified result names and dynamically append to the results list
    for (res_name in result.obj.names) {
      if (!res_name %in% names(tmp.res)) {
        message(paste0("Warning: Result name '", res_name, "' not found in ",group.levels[i]))
        next
      }

      # Add group information
      tmp.res[[res_name]]$group <- group.levels[i]
      
      result.obj[[res_name]] <- rbind(result.obj[[res_name]], tmp.res[[res_name]])
      
      # If this component doesn't exist yet in the results list, initialize it
      # if (!res_name %in% names(result.obj)) {
      #   result.obj[[res_name]] <- tmp.res[[res_name]]
      # } else {
        # If it exists, append to the existing data frame
        # result.obj[[res_name]] <- rbind(result.obj[[res_name]], tmp.res[[res_name]])
      #}
    }
  }
  
  return(result.obj)
}



###########################################################################
#########  survey GLM estimates for cohorts (Proportional Odds assumption)
###########################################################################

#' discrete-time hazard modelling using survey GLM
#' assume same failure pattern for all cohorts (subgroups) 
#' each cohort will be included as a fixed effect, no interactions

getEduc.GLM <- function(educ.group.comp = NULL, 
                        group.var = NULL, 
                        result.obj.names = c('surv_p', 'cond_surv_p','grade_dist_p', 'educ_years')) {
  
  # Ensure the group variable is a factor
  educ.group.comp$group <- as.factor(educ.group.comp[[group.var]])
  
  # Get group levels
  group.levels <- levels(educ.group.comp$group)
  n.group <- length(group.levels)
  
  # Initialize result storage
  result.obj <- vector("list", n.group)
  
  for (i in seq_along(group.levels)) {
    print(paste("Processing group:", group.levels[i]))
    
    # Relevel the group variable
    educ.group.comp <- within(educ.group.comp, group <- relevel(group, ref = group.levels[i]))
    
    # Fit survey design
    dhs_design <- survey::svydesign(
      ids = ~v001, 
      strata = ~strata, 
      data = educ.group.comp, 
      nest = TRUE, 
      weights = ~weights
    )
    
    # Fit GLM
    glm.educ.group <- survey::svyglm(
      drop_frac ~ (-1) + grade + group, 
      design = dhs_design, 
      family = stats::quasibinomial, 
      maxit = 50, 
      weights = educ.group.comp$total
    )
    
    # Extract grade coefficients and calculate results
    coeff.names.all <- row.names(summary(glm.educ.group)$coefficient)
    grade.names <- coeff.names.all[grepl('grade', coeff.names.all)]
    n.grades <- length(grade.names)
    grade.cut <- as.numeric(sapply(strsplit(grade.names, "-"), `[`, 2))
    grade.intervals <- grade.cut[1:n.grades] - c(0, grade.cut[1:(n.grades - 1)])
    
    grade.coeff.idx <- which(grepl('grade', row.names(summary(glm.educ.group)$coefficient)))[1:n.grades]
    grade.betas <- summary(glm.educ.group)$coefficient[grade.coeff.idx, 1]
    V.grades <- stats::vcov(glm.educ.group)[grade.coeff.idx, grade.coeff.idx]
    
    # Calculate results for one group
    tmp.res <- est_from_delta(
      betas = grade.betas,
      grade_cut = grade.cut,
      grade_intervals = grade.intervals,
      V_all_betas = V.grades
    )
    
    # Add group information to results
    result.obj[[i]] <- lapply(result.obj.names, function(res_name) {
      tmp_result <- tmp.res[[res_name]]
      tmp_result$group <- group.levels[i]
      tmp_result
    })
    
    names(result.obj[[i]]) <- result.obj.names
  }
  
  # Combine results for each specified result name dynamically
  combined_results <- lapply(result.obj.names, function(res_name) {
    do.call(rbind, lapply(result.obj, function(group_results) {
      if (is.null(group_results[[res_name]])) return(NULL)
      group_results[[res_name]]
    }))
  })
  
  # Name the combined results and return
  names(combined_results) <- result.obj.names
  return(combined_results)
}




###########################################################################
#########  survey GLM estimates for cohorts, parallel
###########################################################################




getEduc.GLM.parallel <- function(educ.group.comp = NULL, 
                                 group.var = NULL, 
                                 result.obj.names = c('surv_p','cond_surv_p','grade_dist_p','educ_years')) { 
  
  educ.group.comp$group <- as.factor(educ.group.comp[[group.var]])
  group.levels <- levels(educ.group.comp$group)
  n.group <- length(unique(educ.group.comp$group))
  
  # Set up parallel backend
  plan(multisession, workers = parallel::detectCores() - 3) # Adjust the number of cores
  
  # Parallel processing using future_lapply
  result.obj <- future_lapply(1:n.group, function(i) {
    # Relevel the group variable
    educ.group.comp <- within(educ.group.comp, group <- relevel(group, ref = group.levels[i]))
    
    # Survey design
    dhs_design <- survey::svydesign(
      ids = ~v001, 
      strata = ~strata, 
      data = educ.group.comp, 
      nest = TRUE, 
      weights = ~weights
    )
    
    # Fit GLM
    glm.educ.group <- survey::svyglm(
      drop_frac ~ (-1) + grade + group, 
      design = dhs_design, 
      family = stats::quasibinomial, 
      maxit = 50, 
      weights = educ.group.comp$total
    )
    
    # Extract grade coefficients and calculate results
    coeff.names.all <- row.names(summary(glm.educ.group)$coefficient)
    grade.names <- coeff.names.all[grepl("grade", coeff.names.all)]
    
    n.grades <- length(grade.names)
    grade.cut <- as.numeric(sapply(strsplit(grade.names, "-"), `[`, 2))
    grade.intervals <- grade.cut[1:n.grades] - c(0, grade.cut[1:(n.grades - 1)])
    
    grade.coeff.idx <- which(grepl("grade", row.names(summary(glm.educ.group)$coefficient)))[1:n.grades]
    grade.betas <- summary(glm.educ.group)$coefficient[grade.coeff.idx, 1]
    V.grades <- stats::vcov(glm.educ.group)[grade.coeff.idx, grade.coeff.idx]
    
    # Calculate results using delta method
    tmp.res <- est_from_delta(
      betas = grade.betas,
      grade_cut = grade.cut,
      grade_intervals = grade.intervals,
      V_all_betas = V.grades
    )
    
    # Add group information to results
    for (res_name in result.obj.names) {
      # Add group information
      tmp.res[[res_name]]$group <- group.levels[i]
    }
    
    return(tmp.res)
    
  })
  
  # Combine results for each specified result name 
  combined_results <- lapply(result.obj.names, function(res_name) {
    do.call(rbind, lapply(result.obj, function(res) {
      if (is.null(res[[res_name]])) return(NULL)
      res[[res_name]]
    }))
  })
  
  # Name the combined results and return
  names(combined_results) <- result.obj.names
  return(combined_results)
}



###########################################################################
#########  survey GLM (subnational)
###########################################################################


#educ.group.comp <- educ.unadj.admin1.R
#group.var <- c('birth.year','admin1.char')

getEduc.GLM2 <- function(educ.group.comp = NULL, 
                         group.var = NULL, 
                         result.obj.names = c('surv_p', 'cond_surv_p','grade_dist_p', 'educ_years')) {
  
  # Ensure the group variables are factors
  educ.group.comp$group1 <- as.factor(educ.group.comp[[group.var[1]]])
  educ.group.comp$group2 <- as.factor(educ.group.comp[[group.var[2]]])
  
  # Get group levels
  group1.levels <- levels(educ.group.comp$group1)
  group2.levels <- levels(educ.group.comp$group2)
  
  # Initialize result storage
  result.obj <- vector("list", length(group1.levels))
  
  for (i in seq_along(group1.levels)) {
    # Relevel group1 for the current iteration
    educ.group.comp <- within(educ.group.comp, group1 <- relevel(group1, ref = group1.levels[i]))
    
    result.obj[[i]] <- lapply(seq_along(group2.levels), function(j) {
      # Relevel group2 for the current iteration
      educ.group.comp <- within(educ.group.comp, group2 <- relevel(group2, ref = group2.levels[j]))
      
      # Fit survey design
      dhs_design <- survey::svydesign(
        ids = ~v001, 
        strata = ~strata, 
        data = educ.group.comp, 
        nest = TRUE, 
        weights = ~weights
      )
      
      # Fit GLM
      glm.educ.group <- survey::svyglm(
        drop_frac ~ (-1) + grade + group1 + group2, 
        design = dhs_design, 
        family = stats::quasibinomial, 
        maxit = 50, 
        weights = educ.group.comp$total
      )
      
      # Extract grade coefficients and calculate results
      coeff.names.all <- row.names(summary(glm.educ.group)$coefficient)
      grade.names <- coeff.names.all[grepl('grade', coeff.names.all)]
      n.grades <- length(grade.names)
      grade.cut <- as.numeric(sapply(strsplit(grade.names, "-"), `[`, 2))
      grade.intervals <- grade.cut[1:n.grades] - c(0, grade.cut[1:(n.grades - 1)])
      
      grade.coeff.idx <- which(grepl('grade', row.names(summary(glm.educ.group)$coefficient)))[1:n.grades]
      grade.betas <- summary(glm.educ.group)$coefficient[grade.coeff.idx, 1]
      V.grades <- stats::vcov(glm.educ.group)[grade.coeff.idx, grade.coeff.idx]
      
      # Calculate results
      tmp.res <- est_from_delta(
        betas = grade.betas,
        grade_cut = grade.cut,
        grade_intervals = grade.intervals,
        V_all_betas = V.grades
      )
      
      # Add group information dynamically
      # Add group information dynamically
      for (res_name in result.obj.names) {
        # Add group information
        tmp.res[[res_name]]$group1 <- group1.levels[i]
        tmp.res[[res_name]]$group2 <- group2.levels[j]
        
      }
      
      return(tmp.res)
    })
    names(result.obj[[i]]) <- group2.levels
  }
  names(result.obj) <- group1.levels
  
  # Combine results for each specified result name dynamically
  combined_results <- lapply(result.obj.names, function(res_name) {
    do.call(rbind, lapply(seq_along(result.obj), function(i) {
      do.call(rbind, lapply(seq_along(result.obj[[i]]), function(j) {
        if (is.null(result.obj[[i]][[j]][[res_name]])) return(NULL)
        result.obj[[i]][[j]][[res_name]]
      }))
    }))
  })
  
  # Rename columns and finalize results
  combined_results <- lapply(combined_results, function(df) {
    df %>%
      rename(!!group.var[1] := group1, !!group.var[2] := group2)
  })
  
  # Name the combined results and return
  names(combined_results) <- result.obj.names
  return(combined_results)
}




###########################################################################
#########  survey GLM (subnational)
###########################################################################

getEduc.GLM2.parallel <- function(educ.group.comp = NULL, 
                                  group.var = NULL, 
                                  result.obj.names = c('surv_p', 'cond_surv_p','grade_dist_p', 'educ_years')) {
  
  # Ensure factors for grouping variables
  educ.group.comp$group1 <- as.factor(educ.group.comp[[group.var[1]]])
  educ.group.comp$group2 <- as.factor(educ.group.comp[[group.var[2]]])
  
  group1.levels <- levels(educ.group.comp$group1)
  group2.levels <- levels(educ.group.comp$group2)
  
  # Set up parallel backend
  plan(multisession, workers = parallel::detectCores() - 4) # Adjust the number of cores
  
  # Parallel processing using future_lapply
  result.obj <- future_lapply(1:length(group1.levels), function(i) {
    lapply(1:length(group2.levels), function(j) {
      # Create a local copy of the data for the worker
      local_data <- educ.group.comp
      
      # Relevel group variables
      local_data <- within(local_data, group1 <- relevel(group1, ref = group1.levels[i]))
      local_data <- within(local_data, group2 <- relevel(group2, ref = group2.levels[j]))
      
      # Survey design
      dhs_design <- survey::svydesign(
        ids = ~v001, 
        strata = ~strata, 
        data = local_data, 
        nest = TRUE, 
        weights = ~weights
      )
      
      # Fit GLM
      glm.educ.group <- survey::svyglm(
        drop_frac ~ (-1) + grade + group1 + group2, 
        design = dhs_design, 
        family = stats::quasibinomial, 
        maxit = 50, 
        weights = local_data$total
      )
      
      # Extract grade coefficients and calculate results
      coeff.names.all <- row.names(summary(glm.educ.group)$coefficient)
      grade.names <- coeff.names.all[grepl("grade", coeff.names.all)]
      n.grades <- length(grade.names)
      grade.cut <- as.numeric(sapply(strsplit(grade.names, "-"), `[`, 2))
      grade.intervals <- grade.cut[1:n.grades] - c(0, grade.cut[1:(n.grades - 1)])
      
      grade.coeff.idx <- which(grepl("grade", row.names(summary(glm.educ.group)$coefficient)))[1:n.grades]
      grade.betas <- summary(glm.educ.group)$coefficient[grade.coeff.idx, 1]
      V.grades <- stats::vcov(glm.educ.group)[grade.coeff.idx, grade.coeff.idx]
      
      # Calculate results using est_from_delta
      tmp.res <- est_from_delta(
        betas = grade.betas,
        grade_cut = grade.cut,
        grade_intervals = grade.intervals,
        V_all_betas = V.grades
      )
      
      # Add group information dynamically
      for (res_name in result.obj.names) {
        # Add group information
        tmp.res[[res_name]]$group1 <- group1.levels[i]
        tmp.res[[res_name]]$group2 <- group2.levels[j]
        
      }
      
      return(tmp.res)
      
    })
  }, future.seed = TRUE) # Ensure reproducibility
  
  # Combine results for each specified result name dynamically
  combined_results <- lapply(result.obj.names, function(res_name) {
    do.call(rbind, lapply(seq_along(result.obj), function(i) {
      do.call(rbind, lapply(seq_along(result.obj[[i]]), function(j) {
        if (is.null(result.obj[[i]][[j]][[res_name]])) return(NULL)
        result.obj[[i]][[j]][[res_name]]
      }))
    }))
  })
  
  # Rename columns and finalize results
  combined_results <- lapply(combined_results, function(df) {
    df %>%
      rename(!!group.var[1] := group1, !!group.var[2] := group2)
  })
  
  # Name the combined results and return
  names(combined_results) <- result.obj.names
  return(combined_results)
}
