###########################################################################
#########  INLA models, national+subnational, no st interaction
###########################################################################

getEduc.INLA <- function( family = c('betabinomial','binomial')[1], 
                          data, 
                          subnational = T,
                          region.num.var = 'admin1.num',
                          Amat = NULL,
                          spatial.effect = c('fixed','bym2')[1],
                          birth.cohort.specific = T,
                          rw.order=2,
                          time.trend = c('main','bym','bym2')[2],
                          proj.years = 0,
                          control.predictor = list(compute = TRUE),
                          control.compute =  list(config = TRUE), 
                          #control.inla = list(strategy = "adaptive", int.strategy = "auto"),
                          overdisp.mean = 0,
                          overdisp.prec = 0.4,
                          adm.names =NULL){
  
  ### initialize 
  
  inla.rw = utils::getFromNamespace("inla.rw", 
                                    "INLA")
  # set up priors for BYM2
  a <- 1
  b <- 0.01
  
  BYM2Prior <- list(
    phi = list(
      prior = "pc",
      param = c(0.5, 2/3)),
    prec = list(
      prior = "pc.prec",
      param = c(a, b)))
  
  
  ### base 
  formula <- Y ~ 0 + grade
  dat.INLA <- data
  
  
  ### subnational model specification 
  if(subnational){

    dat.INLA$region.int = dat.INLA[[region.num.var]]
    
    
    if(spatial.effect =='bym2'){
      
      if(is.null(Amat)){
        
        stop('Cannot fit BYM2 without specifying neighborhood structure (missing Amat)')
      }
      
      if(!is.null(Amat)){
        
        ### create precision matrix for BYM2 and interaction
        sp.prec.mx <- as.matrix(-Amat)
        
        diag(sp.prec.mx) <- -rowSums(sp.prec.mx)
        
        N.area <- dim(sp.prec.mx)[1]
        scaled.sp.prec.mx <- INLA::inla.scale.model(sp.prec.mx,
                                                    constr = list(A = matrix(1, 1, dim(sp.prec.mx)[1]), e = 0))
        
        dat.INLA$region.int <- as.integer(dat.INLA$region.int)
        
        ### update the formula 
        formula <- update(formula, ~. + f(region.int, model = "bym2", 
                                          scale.model = TRUE,
                                          constr = TRUE,
                                          graph = scaled.sp.prec.mx,
                                          hyper = BYM2Prior))
        
      }
      
      
      
    }
    
    if(spatial.effect =='fixed'){    
      dat.INLA$region.int <- as.factor(dat.INLA$region.int)
      
      formula <- update(formula, ~. + region.int)
      
    }
    
  }
  
  
  
  ### temporal component specification
  if(birth.cohort.specific){
    
    N.year<- max(dat.INLA$birth.year)-min(dat.INLA$birth.year)+1
    
    if(proj.years !=0){

      pseudo.data.add <- dat.INLA[c(1:proj.years),]
      pseudo.data.add$birth.year.int <- c(1:proj.years)+N.year
      pseudo.data.add$Y <-NA
      pseudo.data.add$total <- NA
      pseudo.data.add$drop_frac <- NA
      
      dat.INLA <- rbind(dat.INLA,pseudo.data.add)
      
    }
    
    
    ### set up rw for birth cohort 
    if(time.trend=='main'){
      
      formula <- update(formula, ~. + f(birth.year.int, model = paste0('rw',rw.order), 
                                        scale.model = TRUE, constr = TRUE))
    }
    
    if(time.trend=='bym'){
      
      inla.rw = utils::getFromNamespace("inla.rw", 
                                        "INLA")
      
      RW.birth.prec <- inla.rw(n = N.year+proj.years, order = rw.order, scale.model = T, 
                               sparse = TRUE)
      
      formula <- update(formula, ~. + f(birth.year.int, model = "bym", 
                                        scale.model = TRUE, constr = TRUE,
                                        graph = RW.birth.prec))
    }
    
    if(time.trend=='bym2'){
      
      inla.rw = utils::getFromNamespace("inla.rw", 
                                        "INLA")
      
      RW.birth.prec <- inla.rw(n = N.year+proj.years, order = rw.order, scale.model = FALSE, 
                               sparse = TRUE)
      
      formula <- update(formula, ~. + f(birth.year.int, model = "bym2", 
                                        scale.model = TRUE,
                                        constr = TRUE,
                                        rankdef = 1, 
                                        graph = RW.birth.prec,
                                        hyper = list(
                                          phi = list(
                                            prior = "pc",
                                            param = c(0.5, 2/3)),
                                          prec = list(
                                            prior = "pc.prec",
                                            param = c(1, 0.01)))))
    }
    
    
  }
  
  educ.INLA <- INLA::inla(formula= formula, 
                          family = family, 
                          control.compute =  list(config = TRUE), 
                          control.family =  list(hyper = list(rho = list(param = c(overdisp.mean, overdisp.prec)))), 
                          data = dat.INLA, 
                          control.predictor = list(compute = TRUE),
                          Ntrials = dat.INLA$total, 
                          #control.inla = control.inla,
                          verbose = F)
  
  return.obj <- list()
  return.obj[['INLA.mod']] <- educ.INLA
  

  if(subnational){
    return.obj[['adm.names']] <- adm.names
    return.obj[['region.num.var']] <- region.num.var
    
  }
  
  if(birth.cohort.specific){
    return.obj[['N.year']] <- N.year + proj.years
    return.obj[['start.year']] <- min(dat.INLA$birth.year)
    
  }
  
  return(return.obj)
  
  
}


# family = c('betabinomial','binomial')[1]
# data = educ.splitted.natl.adj
# subnational = T
# region.num.var = 'admin1.num'
# Amat = NULL
# spatial.effect = c('fixed','bym2')[2]
# birth.cohort.specific = T
# rw.order=2
# time.trend = c('main','combined')[2]
# proj.years = 5
# control.predictor = list(compute = TRUE)
# control.compute =  list(config = TRUE)
# overdisp.mean = 0
# overdisp.prec = 0.4
# verbose = T
# adm.names = NULL
# 
# 
# adm_nb <- spdep::poly2nb(country_shp_analysis[[paste0('Admin-',GADM.admin1.level)]], queen=F)
# Amat <- spdep::nb2mat(adm_nb, style="B",zero.policy=TRUE)

#res.inla <- getEduc.INLA(family='binomial',data=educ.splitted.adm1.R,adm.names=admin1.names)
#res.inla <- getEduc.INLA(family='betabinomial',data=educ.cluster.adm1.R,adm.names=admin1.names.list, birth.cohort.specific=T)





###########################################################################
#########  INLA models, posterior collection (updated)
###########################################################################

getEduc.INLA.postsamps <- function(est.grid, INLA.mod, post.sample=NULL, nSamp=1000) {
  
  
  ########################
  ## set parameters
  ########################
  
  # function to extract parameters from the INLA object
  extract_params <- function(inla_obj) {
    tags <- inla_obj$misc$configs$contents$tag
    list(
      grade_int_idx = inla_obj$misc$configs$contents$start[grepl("grade", tags)],
      all_region_idx = INLA.mod$misc$configs$contents$start[which(grepl('region', tags))],
      region_start_idx = INLA.mod$misc$configs$contents$start[which(grepl('region', tags))][1],
      birth_cohort_start_idx = inla_obj$misc$configs$contents$start[grepl("birth", tags)][1],
      space_time_start_idx = inla_obj$misc$configs$contents$start[grepl("time.area", tags)][1],
      n_grades = length(inla_obj$misc$configs$contents$start[grepl("grade", tags)]),
      grade_cut = as.numeric(sapply(strsplit(tags[grepl("grade", tags)], "-"), `[`, 2)),
      grade_intervals = {
        grade_cut <- as.numeric(sapply(strsplit(tags[grepl("grade", tags)], "-"), `[`, 2))
        grade_cut[1:length(grade_cut)] - c(0, grade_cut[1:(length(grade_cut) - 1)])
      }
    )
  }
  
  # generated posterior sample if not provided
  if(is.null(post.sample)){
    post.sample = INLA::inla.posterior.sample(n = nSamp, result = INLA.mod)
  }
  
  
  # Extract parameters
  params <- extract_params(INLA.mod)
  
  # Determine the model
  if(length(params$region_start_idx)==0){
    subnational = F
  }else{
    subnational = T
  }
  
  if(length(params$all_region_idx)>1){
    fixed.eff = T
  }else{
    fixed.eff = F
  }
  
  if(is.na(params$space_time_start_idx) |length(params$space_time_start_idx)==0){
    typeIV.int = F
  }else{
    typeIV.int = T
  }
  
  ########################
  ## Initialize storage
  ########################
  
  n_comb <- nrow(est.grid)
  
  cond_surv_p_draws <- array(NA, dim = c(n_comb, params$n_grades, nSamp))
  surv_p_draws <- array(NA, dim = c(n_comb, params$n_grades, nSamp))
  grade_dist_p_draws <- array(NA, dim = c(n_comb, params$n_grades + 1, nSamp))
  
  educ_year_draws <- matrix(0, nrow = nSamp, ncol = n_comb)
  
  ########################
  ## Iterate over samples
  ########################
  
  for (i in 1:nSamp) {
    cat("Processing sample", i, "\n")
    
    post_sample_latent <- post.sample[[i]]$latent
    betas <- post_sample_latent[params$grade_int_idx]
    
    for (j in 1:n_comb) {
      
      ########################
      ## Extract indices
      ########################
      
      grid_item <- est.grid[j, ]
      birth_cohort_idx <- as.integer(grid_item$birth.year.int)
      
      if(subnational){
        region.idx <- as.integer(grid_item$region.int)
      }
      
      if(typeIV.int){
        st.int.idx <- as.integer(grid_item$time.area)
      }
      
      ##################################
      ## Compute survival probabilities
      ################################## 
      
      ## intercept + time trend
      logit_probs <- betas + post_sample_latent[params$birth_cohort_start_idx + birth_cohort_idx - 1] 
      
      ## + spatial main effect
      if(subnational & fixed.eff){ ## spatial fixed effects 
        if(region.idx >1){ # reference level is region_idx = 1 
          logit_probs <- logit_probs+post_sample_latent[params$region_start_idx+region.idx-2]       
        }
      }
      
      if(subnational &(!fixed.eff)){ ## spatial random effects 
        logit_probs <- logit_probs+post_sample_latent[params$region_start_idx + region.idx - 1]       
      }
      
      ## + spatial temporal interaction
      if(typeIV.int){ ## spatial random effects 
        logit_probs <- logit_probs+post_sample_latent[params$space_time_start_idx + st.int.idx - 1]
      }
      
      
      ##################################
      ## Compute estimands 
      ################################## 
      
      ## compute conditional droppout/survival probabilities 
      cond_probs <- expit(logit_probs)
      cond_surv_p_draws[j, , i] <- 1-cond_probs
      
      ## compute survival probabilities 
      p_survival <- unlist(lapply(1:params$n_grades, function(y) prod(1 - cond_probs[1:y])))
      surv_p_draws[j, , i] <- p_survival
      
      # Compute grade probabilities
      p_grade_dist <- c(1, p_survival) - c(p_survival, 0)
      grade_dist_p_draws[j, , i] <- p_grade_dist
      
      # Store years of education
      mean_est <- sum(p_survival * params$grade_intervals)
      educ_year_draws[i, j] <- mean_est
    }
  }
  
  ### assign rownames for the conditional survival probabilities 
  dimnames(cond_surv_p_draws) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = params$grade_cut,      # second dimension: grade
    sample.idx = c(1:nSamp)       # third dimension: samples 
  )
  
  ### assign rownames for the survival probabilities 
  dimnames(surv_p_draws) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = params$grade_cut, # second dimension: grade
    sample.idx = c(1:nSamp)        # third dimension: samples 
  )
  
  ### assign rownames for the grade distributions
  dimnames(grade_dist_p_draws) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = c(0,params$grade_cut) , # Names for the second dimension
    sample.idx = c(1:nSamp)          # third dimension: samples 
  )
  
  ### return draws 
  return(list(cond_surv_p_draws = cond_surv_p_draws,
              surv_p_draws = surv_p_draws,
              grade_dist_p_draws = grade_dist_p_draws,
              educ_year_draws = educ_year_draws, 
              est.grid = est.grid))
  
}


###########################################################################
#########  INLA models, summarize posterior draws 
###########################################################################

### helper function 
#posterior_array <- U.est.draws$cond_surv_p_draws

summarize_est_array <- function(posterior_array,
                                alpha.level=0.05,
                                est.grid) {
  
  n_comb <- dim(posterior_array)[1]
  n_grades <- dim(posterior_array)[2]
  
  # Precompute summaries for each combination and grade
  summaries <- apply(posterior_array, c(1, 2), function(draws) {
    c(
      mean = mean(draws, na.rm = TRUE),
      lower = unname(quantile(draws, alpha.level/2, na.rm = TRUE)),  # Remove automatic names
      upper = unname(quantile(draws, 1-alpha.level/2, na.rm = TRUE)), # Remove automatic names
      sd = sd(draws, na.rm = TRUE)
    )
  })
  
  # Convert summaries to a data frame
  summaries_df <- as.data.frame.table(summaries, responseName = "value")
  summaries_df <- summaries_df[, c("order.idx", "grade", "Var1", "value")]
  colnames(summaries_df) <- c("order.idx", "grade", "stat", "value")
  
  # Reshape data frame to wide format
  summaries_wide <- tidyr::pivot_wider(
    summaries_df,
    names_from = stat,
    values_from = value
  )
  
  # Convert grades to numeric if necessary
  summaries_wide$grade <- as.numeric(as.character(summaries_wide$grade))
  summaries_wide$order.idx <- as.numeric(as.character(summaries_wide$order.idx))
  
  # merge the estimation grid 
  summaries_wide <- merge(summaries_wide, est.grid, by='order.idx',all.x=T)
  
  # reorder 
  summaries_wide <- summaries_wide[order(summaries_wide$order.idx,
                                         summaries_wide$grade),]
  
  return(summaries_wide)
  
}


# post.draws = U.est.draws
# est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')]
# adm.names = adm.names
# subnational =T
# nSamp=1000
# start.year = min(educ_dat_ind$birth.year)

summarize.Educ.INLA.postsamps <- function(est.grid,
                                          post.draws,
                                          adm.names = NULL,
                                          subnational =T,
                                          start.year=NULL,
                                          nSamp=1000,
                                          alpha.level=0.05){
  
  
  # prepare posterior draws 
  cond_surv_p_draws <- post.draws$cond_surv_p_draws
  surv_p_draws <- post.draws$surv_p_draws
  grade_dist_p_draws <- post.draws$grade_dist_p_draws
  educ_year_draws <- post.draws$educ_year_draws
  
  n_comb <- dim(est.grid)[1]
  
  if(dim(educ_year_draws)[2]!=n_comb){
    stop('Provided estimation grid does not match draws. Check whether the posterior draws and estimation grid are at the same geographical level and/or for the same range of birth cohorts.')
  }
  
  if(!'order.idx' %in% colnames(est.grid)){
    message('Order index not provided for the estimation grid. Assume the order of posterior draws align with the grid. Please double check.')
    est.grid$order.idx <- c(1:n_comb)
  }
    

  ### prepare estimation grid
  if(subnational){
    est.grid <- merge(est.grid, adm.names, by='region.int')
  }
  
  if(!is.null(est.grid$birth.year.int)& !is.null(start.year)){
    est.grid$birth.year <- est.grid$birth.year.int+ start.year -1
  }
  
  
  ##########################################
  ### summarize conditional probabilities
  ##########################################

  if(!is.null(cond_surv_p_draws)){
    cond_surv_p_summary <- summarize_est_array(cond_surv_p_draws,
                                               alpha.level=alpha.level,
                                               est.grid = est.grid)  
  }else{
    cond_surv_p_summary = NULL
  }
  
  ##########################################
  ### summarize survival probabilities
  ##########################################
  
  if(!is.null(surv_p_draws)){
    surv_p_summary <- summarize_est_array(surv_p_draws,
                                          alpha.level=alpha.level,
                                          est.grid = est.grid)  
  }else{
    surv_p_summary = NULL
  }
  
  ##########################################
  ### summarize grade distribution
  ##########################################
  
  if(!is.null(grade_dist_p_draws)){
    grade_dist_p_summary <- summarize_est_array(grade_dist_p_draws,
                                                alpha.level=alpha.level,
                                                est.grid = est.grid)  
  }else{
    grade_dist_p_summary = NULL
  }
  
  ##################################
  ### summarize years of education 
  ##################################
  
  educ.years.q = matrix(0, nrow = n_comb, ncol = 3)
  educ.years.mean = vector()
  
  for(i in 1:n_comb){
    educ.years.q[i,] = quantile(educ_year_draws[,i], probs = c(alpha.level/2, 0.50, 1-alpha.level/2), na.rm = TRUE)
    educ.years.mean[i] = mean(educ_year_draws[,i],na.rm=T)
  }
  
  educ.years.summary = est.grid
  educ.years.summary$lower = educ.years.q[,1]
  educ.years.summary$median = educ.years.q[,2]
  educ.years.summary$upper = educ.years.q[,3]
  educ.years.summary$mean = educ.years.mean
  
  ### return object
  return(list(est.grid = est.grid,
              cond_surv_p = cond_surv_p_summary,
              surv_p = surv_p_summary,
              grade_dist_p = grade_dist_p_summary,
              educ_years = educ.years.summary))
  
  
}




###########################################################################
#########  INLA models, U/R aggregation
###########################################################################

# helper function to align matrices
align_UR_draws_columns <- function(mat1, mat2) {
  # Convert matrices to data frames
  df1 <- as.data.frame(mat1)
  df2 <- as.data.frame(mat2)
  
  # Bind rows, automatically aligning columns and filling missing ones with NA
  combined <- bind_rows(df1, df2)
  
  # Separate back into two matrices
  mat1_aligned <- as.matrix(combined[1:nrow(mat1), , drop = FALSE])
  mat2_aligned <- as.matrix(combined[(nrow(mat1) + 1):nrow(combined), , drop = FALSE])
  
  list(mat1 = mat1_aligned, mat2 = mat2_aligned)
}


# post.draws.U <- U.est.draws
# post.draws.R <- R.est.draws
# UR.frac <- ur.frac.grid

aggre.Educ.UR <- function(post.draws.U,
                          post.draws.R,
                          UR.frac,
                          nSamp = 1000){
  
  ##################
  ## preparation
  ##################
  
  educ.draws.U <- post.draws.U$educ_year_draws
  surv.p.draws.U <- post.draws.U$surv_p_draws
  grade.dist.draws.U <-post.draws.U$grade_dist_p_draws
  
  educ.draws.R <- post.draws.R$educ_year_draws
  surv.p.draws.R <- post.draws.R$surv_p_draws
  grade.dist.draws.R <-post.draws.R$grade_dist_p_draws
  
  aggre.grid <- UR.frac
  n_comb <- dim(aggre.grid)[1]
  
  # check whether the urban/rural parts correspond to each other
  if(dim(educ.draws.U)[2]!=dim(educ.draws.R)[2]){
    stop('Urban/rural estimates do not macth. Check whether the models are at the same geographical level and/or for the same range of birth cohorts.')
  }
  
  if(dim(educ.draws.U)[2]!=dim(aggre.grid)[1]){
    stop('Provided UR fractions do not match draws. Check whether the models and fractions are at the same geographical level and/or for the same range of birth cohorts.')
  }
  

  ############################
  ## merge posterior draws
  ############################

  
  ### education years 
  educ.draws.overall <- aggre.grid$urban_frac*t(educ.draws.U) + aggre.grid$rural_frac*t(educ.draws.R)
  educ.draws.overall <- t(educ.draws.overall)
  
  
  ### survival probabilities and conditional survival probabilities 
  n.grades.overall <- max(dim(surv.p.draws.R)[2],dim(surv.p.draws.U)[2])
  surv.p.draws.overall <- array(NA, dim = c(n_comb, n.grades.overall, nSamp))
  cond.surv.p.draws.overall <- array(NA, dim = c(n_comb, n.grades.overall, nSamp))
  
  for(i in 1:nSamp){
    cat("Processing sample", i, "\n")
    
    #i=1
    #print(i)
    
    surv.p.ite.U <- surv.p.draws.U[,,i]
    surv.p.ite.R <- surv.p.draws.R[,,i]
    
    # Align the columns of U and R
    aligned.surv.p <- align_UR_draws_columns(surv.p.ite.U,surv.p.ite.R)
    
    surv.p.ite.U.merged <- aligned.surv.p$mat1
    surv.p.ite.R.merged <- aligned.surv.p$mat2 
    
    surv.p.ite.U.merged[is.na(surv.p.ite.U.merged)] <- 0
    surv.p.ite.R.merged[is.na(surv.p.ite.R.merged)] <- 0
    
    # aggregate 
    surv.p.ite.overall <- aggre.grid$urban_frac*surv.p.ite.U.merged +
      aggre.grid$rural_frac*surv.p.ite.R.merged
    
    
    # prepare the conditional suvival probabilities 
    cond.surv.p.ite.overall <- surv.p.ite.overall # Initialize result matrix
    for (k in 2:ncol(surv.p.ite.overall)) {
      cond.surv.p.ite.overall[, k] <- surv.p.ite.overall[, k] / surv.p.ite.overall[, k - 1]
    }
    cond.surv.p.ite.overall[, 1] <- surv.p.ite.overall[, 1] / 1
    
    # store results
    cond.surv.p.draws.overall[,,i] <- as.matrix(cond.surv.p.ite.overall)
    surv.p.draws.overall[,,i] <- surv.p.ite.overall
  }
  
  ### grade distribution
  n.grades.interval.overall <- max(dim(grade.dist.draws.U)[2],dim(grade.dist.draws.R)[2])
  grade.draws.overall <- array(NA, dim = c(n_comb, n.grades.interval.overall, nSamp))
  
  for(i in 1:nSamp){
    
    #i=1
    #print(i)
    
    grade.dist.ite.U <- grade.dist.draws.U[,,i]
    grade.dist.ite.R <- grade.dist.draws.R[,,i]
    
    # Align the columns of mat1 and mat2
    aligned <- align_UR_draws_columns(grade.dist.ite.U,grade.dist.ite.R)
    
    # Access the aligned matrices
    grade.dist.ite.U.merged <- aligned$mat1
    grade.dist.ite.R.merged <- aligned$mat2 
    
    grade.dist.ite.U.merged[is.na(grade.dist.ite.U.merged)] <- 0
    grade.dist.ite.R.merged[is.na(grade.dist.ite.R.merged)] <- 0
    
    grade.dist.ite.overall <- aggre.grid$urban_frac*grade.dist.ite.U.merged +
      aggre.grid$rural_frac*grade.dist.ite.R.merged
    
    grade.draws.overall[,,i] <- grade.dist.ite.overall
  }
  
  ############################
  ## prepare return object
  ############################
  
  ### assign rownames for the conditional survival probabilities 
  dimnames(cond.surv.p.draws.overall) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = colnames(surv.p.ite.U.merged),      # second dimension: grade
    sample.idx = c(1:nSamp)       # third dimension: samples 
  )
  
  ### assign rownames for the survival probabilities 
  dimnames(surv.p.draws.overall) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = colnames(surv.p.ite.U.merged), # second dimension: grade
    sample.idx = c(1:nSamp)        # third dimension: samples 
  )
  
  ### assign rownames for the grade distributions
  dimnames(grade.draws.overall) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = colnames(grade.dist.ite.U.merged) , # Names for the second dimension
    sample.idx = c(1:nSamp)          # third dimension: samples 
  )
  
  ### return draws 
  return(list(cond_surv_p_draws = cond.surv.p.draws.overall,
              surv_p_draws = surv.p.draws.overall,
              grade_dist_p_draws = grade.draws.overall,
              educ_year_draws = educ.draws.overall, 
              est.grid = aggre.grid))
  
  
  
}






###########################################################################
#########  INLA models, U/R difference
###########################################################################

# helper function to align matrices
align_UR_draws_columns <- function(mat1, mat2) {
  # Convert matrices to data frames
  df1 <- as.data.frame(mat1)
  df2 <- as.data.frame(mat2)
  
  # Bind rows, automatically aligning columns and filling missing ones with NA
  combined <- bind_rows(df1, df2)
  
  # Separate back into two matrices
  mat1_aligned <- as.matrix(combined[1:nrow(mat1), , drop = FALSE])
  mat2_aligned <- as.matrix(combined[(nrow(mat1) + 1):nrow(combined), , drop = FALSE])
  
  list(mat1 = mat1_aligned, mat2 = mat2_aligned)
}


# post.draws.U <- U.est.draws
# post.draws.R <- R.est.draws
# UR.frac <- ur.frac.grid

cal.Educ.UR.diff <- function(post.draws.U,
                          post.draws.R,
                          est.grid,
                          nSamp = 1000){
  
  ##################
  ## preparation
  ##################
  
  educ.draws.U <- post.draws.U$educ_year_draws
  surv.p.draws.U <- post.draws.U$surv_p_draws
  cond.surv.p.draws.U <- post.draws.U$cond_surv_p_draws
  grade.dist.draws.U <-post.draws.U$grade_dist_p_draws
  
  educ.draws.R <- post.draws.R$educ_year_draws
  surv.p.draws.R <- post.draws.R$surv_p_draws
  cond.surv.p.draws.R <- post.draws.R$cond_surv_p_draws
  grade.dist.draws.R <-post.draws.R$grade_dist_p_draws
  
  est.grid <- est.grid
  n_comb <- dim(est.grid)[1]
  
  # check whether the urban/rural parts correspond to each other
  if(dim(educ.draws.U)[2]!=dim(educ.draws.R)[2]){
    stop('Urban/rural estimates do not macth. Check whether the models are at the same geographical level and/or for the same range of birth cohorts.')
  }
  
  if(dim(educ.draws.U)[2]!=dim(est.grid)[1]){
    stop('Provided estimation grid does not match draws. Check whether the models and fractions are at the same geographical level and/or for the same range of birth cohorts.')
  }
  
  
  ############################
  ## merge posterior draws
  ############################
  
  
  ### education years 
  educ.draws.diff <- educ.draws.U- educ.draws.R
  
  ### survival probabilities and conditional survival probabilities 
  n.grades.overall <- max(dim(surv.p.draws.R)[2],dim(surv.p.draws.U)[2])
  surv.p.draws.diff <- array(NA, dim = c(n_comb, n.grades.overall, nSamp))
  cond.surv.p.draws.diff <- array(NA, dim = c(n_comb, n.grades.overall, nSamp))
  
  for(i in 1:nSamp){
    cat("Processing sample", i, "\n")
    
    #i=1
    #print(i)
    
    surv.p.ite.U <- surv.p.draws.U[,,i]
    surv.p.ite.R <- surv.p.draws.R[,,i]
    cond.surv.p.ite.U <- cond.surv.p.draws.U[,,i]
    cond.surv.p.ite.R <- cond.surv.p.draws.R[,,i]
    
    # Align the columns of U and R
    aligned.surv.p <- align_UR_draws_columns(surv.p.ite.U,surv.p.ite.R)
    aligned.cond.surv.p <- align_UR_draws_columns(cond.surv.p.ite.U,cond.surv.p.ite.R)
    
    surv.p.ite.U.merged <- aligned.surv.p$mat1
    surv.p.ite.R.merged <- aligned.surv.p$mat2 
    surv.p.ite.U.merged[is.na(surv.p.ite.U.merged)] <- 0
    surv.p.ite.R.merged[is.na(surv.p.ite.R.merged)] <- 0
    
    cond.surv.p.ite.U.merged <- aligned.cond.surv.p$mat1
    cond.surv.p.ite.R.merged <- aligned.cond.surv.p$mat2 
    cond.surv.p.ite.U.merged[is.na(cond.surv.p.ite.U.merged)] <- 0
    cond.surv.p.ite.R.merged[is.na(cond.surv.p.ite.R.merged)] <- 0
    
    # calculate UR difference 
    surv.p.ite.diff <- surv.p.ite.U.merged-surv.p.ite.R.merged
    cond.surv.p.ite.diff <- cond.surv.p.ite.U.merged-cond.surv.p.ite.R.merged
    
    # store results
    cond.surv.p.draws.diff[,,i] <- cond.surv.p.ite.diff
    surv.p.draws.diff[,,i] <- surv.p.ite.diff
  }
  
  ### grade distribution
  n.grades.interval.overall <- max(dim(grade.dist.draws.U)[2],dim(grade.dist.draws.R)[2])
  grade.draws.diff <- array(NA, dim = c(n_comb, n.grades.interval.overall, nSamp))
  
  for(i in 1:nSamp){
    cat("Processing sample", i, "\n")
    
    #i=1
    #print(i)
    
    grade.dist.ite.U <- grade.dist.draws.U[,,i]
    grade.dist.ite.R <- grade.dist.draws.R[,,i]
    
    # Align the columns of mat1 and mat2
    aligned <- align_UR_draws_columns(grade.dist.ite.U,grade.dist.ite.R)
    
    # Access the aligned matrices
    grade.dist.ite.U.merged <- aligned$mat1
    grade.dist.ite.R.merged <- aligned$mat2 
    
    grade.dist.ite.U.merged[is.na(grade.dist.ite.U.merged)] <- 0
    grade.dist.ite.R.merged[is.na(grade.dist.ite.R.merged)] <- 0
    
    grade.dist.ite.diff <- grade.dist.ite.U.merged-grade.dist.ite.R.merged
    
    grade.draws.diff[,,i] <- grade.dist.ite.diff
  }
  
  ############################
  ## prepare return object
  ############################
  
  ### assign rownames for the conditional survival probabilities 
  dimnames(cond.surv.p.draws.diff) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = colnames(surv.p.ite.U.merged),      # second dimension: grade
    sample.idx = c(1:nSamp)       # third dimension: samples 
  )
  
  ### assign rownames for the survival probabilities 
  dimnames(surv.p.draws.diff) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = colnames(surv.p.ite.U.merged), # second dimension: grade
    sample.idx = c(1:nSamp)        # third dimension: samples 
  )
  
  ### assign rownames for the grade distributions
  dimnames(grade.draws.diff) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = colnames(grade.dist.ite.U.merged) , # Names for the second dimension
    sample.idx = c(1:nSamp)          # third dimension: samples 
  )
  
  ### return draws 
  return(list(cond_surv_p_draws = cond.surv.p.draws.diff,
              surv_p_draws = surv.p.draws.diff,
              grade_dist_p_draws = grade.draws.diff,
              educ_year_draws = educ.draws.diff, 
              est.grid = est.grid))
  
  
  
}





###########################################################################
#########  INLA models, arbitrary aggregation
###########################################################################


aggre.Educ.general <- function(aggre.grid,
                               post.draws,
                               new_column_names,
                               nSamp=1000){
  
  
  ##################
  ## preparation
  ##################
  
  surv_p_draws <- post.draws$surv_p_draws
  #cond_surv_p_draws <- post.draws$cond_surv_p_draws
  grade_dist_p_draws <- post.draws$grade_dist_p_draws
  
  educ_year_draws <- post.draws$educ_year_draws
  
  n_comb <- dim(aggre.grid)[1]
  
  if(dim(educ_year_draws)[2]!=n_comb){
    stop('Provided aggregation grid does not match draws. Check whether the posterior draws and estimation grid are at the same geographical level and/or for the same range of birth cohorts.')
  }
  
  # Unique groups (used for aggre.grid matrix columns)
  unique_groups <- unique(aggre.grid$group)
  
  # Sparse matrix construction
  weights.mat <- sparseMatrix(
    i = 1:n_comb,                                # Row indices (original combinations)
    j = match(aggre.grid$group, unique_groups),   # Column indices (groups)
    x = aggre.grid$aggre.weight,                        # Weights as values
    dims = c(n_comb, length(unique_groups))
  )
  
  
  # Matrix multiplication for aggregation
  aggre_educ_draws <- as.matrix(educ_year_draws %*% weights.mat)
  
  
  ############################
  ## aggregate posterior draws
  ############################
  
  ### education years 
  educ.year.draws.aggre <- as.matrix(educ_year_draws %*% weights.mat)
  
  
  ### grade distribution, survival probabilities and conditional survival probabilities
  surv.p.draws.aggre <- array(NA, dim = c(length(unique_groups), dim(surv_p_draws)[2], nSamp))
  cond.surv.p.draws.aggre <- array(NA, dim = c(length(unique_groups), dim(surv_p_draws)[2], nSamp))
  grade.draws.aggre <- array(NA, dim = c(length(unique_groups), dim(grade_dist_p_draws)[2], nSamp))
  
  for(i in 1:nSamp){
    cat("Processing sample", i, "\n")
    
    # i= 1
    #print(i)
    
    ### survival probilities 
    surv.p.ite <- surv_p_draws[,,i]
    aggre.surv.p.ite <- t(as.matrix( t(surv.p.ite) %*% weights.mat))
    surv.p.draws.aggre[,,i] <- aggre.surv.p.ite
    
    ### conditional survival probabilities 
    aggre.cond.surv.p.ite <- aggre.surv.p.ite # Initialize result matrix
    for (k in 2:ncol(aggre.surv.p.ite)) {
      aggre.cond.surv.p.ite[, k] <- aggre.surv.p.ite[, k] / aggre.surv.p.ite[, k - 1]
    }
    aggre.cond.surv.p.ite[, 1] <- aggre.surv.p.ite[, 1] / 1
    
    cond.surv.p.draws.aggre[,,i] <- aggre.cond.surv.p.ite
    
    
    ### grade distribution 
    grade.dist.ite <- grade_dist_p_draws[,,i]
    aggre.grade.dist.ite <- t(as.matrix( t(grade.dist.ite) %*% weights.mat))
    grade.draws.aggre[,,i] <- aggre.grade.dist.ite
    
  }
  
  ############################
  ## prepare return object
  ############################
  
  ### assign rownames for the conditional survival probabilities 
  dimnames(cond.surv.p.draws.aggre) <- list(
    order.idx = seq_along(unique_groups),       # first dimension: combinations 
    grade = colnames(surv.p.ite),      # second dimension: grade
    sample.idx = c(1:nSamp)       # third dimension: samples 
  )
  
  ### assign rownames for the survival probabilities 
  dimnames(surv.p.draws.aggre) <- list(
    order.idx = seq_along(unique_groups),       # first dimension: combinations 
    grade = colnames(surv.p.ite), # second dimension: grade
    sample.idx = c(1:nSamp)        # third dimension: samples 
  )
  
  ### assign rownames for the grade distributions
  dimnames(grade.draws.aggre) <- list(
    order.idx = seq_along(unique_groups),       # first dimension: combinations 
    grade = colnames(grade.dist.ite) , # Names for the second dimension
    sample.idx = c(1:nSamp)          # third dimension: samples 
  )
  
  # Reference table for resulting groups
  group_ref_table <- data.frame(
    group = as.character(unique_groups),
    order.idx = seq_along(unique_groups)
  )
  
  # Separate the `group` variable into multiple columns
  split_group <- do.call(rbind, strsplit(group_ref_table$group, "\\*"))
  group_ref_table[new_column_names] <- as.data.frame(split_group)
  
  # convert index columns into integers 
  for (col in new_column_names) {
    if (grepl("int", col)) {
      group_ref_table[[col]] <- as.integer(group_ref_table[[col]])
    }
  }
  
  ### return draws 
  return(list(cond_surv_p_draws = cond.surv.p.draws.aggre,
              surv_p_draws = surv.p.draws.aggre,
              grade_dist_p_draws = grade.draws.aggre,
              educ_year_draws = educ.year.draws.aggre, 
              est.grid = group_ref_table))
  
}










###########################################################################
#########  INLA models, aggregate grades to educational levels 
###########################################################################

aggre.Educ.levels <- function(post.draws,
                              educ_level_cutoffs = c(0,1,7,8,12,14)){
  
  
  ##################
  ## preparation
  ##################
  
  surv_p_draws <- post.draws$surv_p_draws
  grade_dist_p_draws <- post.draws$grade_dist_p_draws
  
  #educ_year_draws <- post.draws$educ_year_draws
  
  n_comb <- dim(grade_dist_p_draws)[1]
  n_grade <- dim(grade_dist_p_draws)[2]-1
  nSamp <-dim(grade_dist_p_draws)[3]
  
  ### prepare education levels info
  educ_level_breaks <- c(educ_level_cutoffs, max(n_grade))  
  educ_level_labels <- educ_level_cutoffs 
  educ_levels <- cut(0:n_grade, breaks = educ_level_breaks, labels = educ_level_labels,
                     include.lowest = TRUE, right = FALSE)
  n_educ_levels <- length(educ_level_labels)

  
  ############################
  ## aggregate posterior draws
  ############################
  
  ### grade distributions 
  aggre_grade_p_draws <- array(0, dim = c(n_comb, n_educ_levels, nSamp))
  
  for (i in seq_along(educ_level_labels)) {
    idx <- which(educ_levels == educ_level_labels[i])
    aggre_grade_p_draws[, i, ] <- apply(grade_dist_p_draws[, idx, , drop = FALSE], c(1, 3), sum)
  }
  
  
  ### survival probabilities
  aggre_surv_p_draws <- array(0, dim = c(n_comb, n_educ_levels-1, nSamp))
  
  for (i in c(1:(n_educ_levels-1))) {
    grade_tmp <- educ_level_cutoffs[i+1] ## not interest in grade 0 
    aggre_surv_p_draws[, i, ] <- surv_p_draws[,grade_tmp,]
  }
  #tmp_test <- aggregated_surv_p_draws[,,1]

  ### conditional survival probabilities 
  aggre_cond_surv_p_draws <- array(0, dim = c(n_comb, n_educ_levels-1, nSamp))
  
  for(i in 1:nSamp){

    ### extract survival probilities 
    aggre.surv.p.ite <- aggre_surv_p_draws[,,i]

    ### conditional survival probabilities 
    aggre.cond.surv.p.ite <- aggre.surv.p.ite # Initialize result matrix
    for (k in 2:ncol(aggre.surv.p.ite)) {
      aggre.cond.surv.p.ite[, k] <- aggre.surv.p.ite[, k] / aggre.surv.p.ite[, k - 1]
    }
    aggre.cond.surv.p.ite[, 1] <- aggre.surv.p.ite[, 1] / 1
    
    aggre_cond_surv_p_draws[,,i] <- aggre.cond.surv.p.ite
    
  }
  #tmp_test <- aggre_cond_surv_p_draws[,,1]
  
  
  ############################
  ## prepare return object
  ############################
  
  ### assign rownames for the conditional survival probabilities 
  dimnames(aggre_cond_surv_p_draws) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = as.numeric(educ_level_cutoffs[2:n_educ_levels]),      # second dimension: grade
    sample.idx = c(1:nSamp)       # third dimension: samples 
  )
  
  ### assign rownames for the survival probabilities 
  dimnames(aggre_surv_p_draws) <- list(
    order.idx =  c(1:n_comb),       # first dimension: combinations 
    grade =  as.numeric(educ_level_cutoffs[2:n_educ_levels]), # second dimension: grade
    sample.idx = c(1:nSamp)        # third dimension: samples 
  )
  
  ### assign rownames for the grade distributions
  dimnames(aggre_grade_p_draws) <- list(
    order.idx = c(1:n_comb),       # first dimension: combinations 
    grade = as.numeric(educ_level_cutoffs),      # second dimension: grade
    sample.idx = c(1:nSamp)       # third dimension: samples 
  )
  
  
  ### return draws 
  return(list(cond_surv_p_draws = aggre_cond_surv_p_draws,
              surv_p_draws = aggre_surv_p_draws,
              grade_dist_p_draws = aggre_grade_p_draws,
              educ_year_draws = post.draws$educ_year_draws, 
              est.grid = post.draws$est.grid))
  
}






