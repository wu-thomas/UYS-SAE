##############################################################################
#########   load packages
##############################################################################

rm(list = ls())

# Load libraries and info ----------------------------------------------------------
library(dplyr)
library(labelled)
library(survey)
library(haven)
library(rdhs)
library(data.table)

library(SUMMER)

################################################################
#########   set directory and parameters
################################################################
# path to home directory
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# country and survey year
country_survey <- code.path.splitted[length(code.path.splitted)-2]
country <- unlist(strsplit(country_survey, "_"))[1]
survey_year <- unlist(strsplit(country_survey, "_"))[2]
survey <- paste0(country,'_',survey_year)

# directories 
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-4)], collapse = "/")
data.dir<-paste0(home.dir,'/Data/',survey)
res.dir<-paste0(home.dir,'/Results/',survey)
code.dir <- paste0(home.dir,'/Scripts/',survey)


# load country/survey info
setwd(paste0(data.dir,sep=''))

info.name <- paste0(survey, "_general_info.rda")
load(file = paste0(info.name, sep=''))


################################################################
#########   create directories
################################################################

setwd(paste0(res.dir))


if(!dir.exists(paths = paste0('Direct_unadj'))){ 
  dir.create(file.path('Direct_unadj'))
}

if(!dir.exists(paths = paste0('Direct_modified'))){ 
  dir.create(file.path('Direct_modified'))
}


################################################################
#########   load helper functions
################################################################

setwd(paste0(code.dir))
source('helper_functions/DataProcessing_helper.R')
source('helper_functions/helper_educ_direct_GLM.R')



##############################################################################
#########   load polygon files
##############################################################################

setwd(paste0(data.dir,'/shapeFiles_gadm'))

country_shp_analysis <- readRDS('country_shp_analysis.rds')
country_shp_smoothed <- readRDS('country_shp_smoothed.rds')
admin1_info <- readRDS('admin1_info.rds')
admin2_info <- readRDS('admin2_info.rds')
  

setwd(paste0(data.dir,'/DHS_data'))
cluster.info <- readRDS(file=paste0(country,'_',survey_year,'_cluster_info.rds'))

################################################################
######### load data
################################################################

### original data 

setwd(paste0(data.dir,'/prepared_dat'))

educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))




################################################################
######### prepare analysis data set
################################################################

modified_ind = T

if(modified_ind){
  account.censor.ind=T
  folder.suffix='modified'
}else{
  account.censor.ind = F
  folder.suffix = 'unadj'
}

### overall
educ.splitted.overall <- getEduc_splitted(data = educ_dat_ind,
                                      compact = T, 
                                      account.censor=account.censor.ind,
                                      compact.by =  c('v001','v022', "weights","birth.year"))

educ.splitted.overall$strata <- educ.splitted.overall$v022

### urban
educ.splitted.U <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='urban',],
                                             compact = T, 
                                             account.censor=account.censor.ind,
                                             compact.by =  c('v001','v022', "weights","birth.year"))

educ.splitted.U$strata <- educ.splitted.U$v022


### rural
educ.splitted.R <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='rural',],
                                               compact = T, 
                                               account.censor=account.censor.ind,
                                               compact.by =  c('v001','v022', "weights","birth.year"))

educ.splitted.R$strata <- educ.splitted.R$v022

################################################################
######### national direct estimates 
################################################################

setwd(paste0(res.dir,'/Direct_',folder.suffix,'/'))

if(file.exists('natl_yearly_direct_U.rds')&
   file.exists('natl_yearly_direct_R.rds')&
   file.exists('natl_yearly_direct_overall.rds')){
  
  natl.U.direct.yearly <- readRDS('natl_yearly_direct_U.rds')
  natl.R.direct.yearly <- readRDS('natl_yearly_direct_R.rds')
  natl.overall.direct.yearly <- readRDS('natl_yearly_direct_overall.rds')
  
}else{
  ### natl all years unadjusted direct estimates
  natl.overall.direct.yearly<- getEduc.Direct.group(splitted.educ= educ.splitted.overall,
                                                    group.var='birth.year')
  saveRDS(natl.overall.direct.yearly,'natl_yearly_direct_overall.rds')
  
  
  ### urban/rural specific, yearly
  natl.U.direct.yearly <- getEduc.Direct.group(splitted.educ= educ.splitted.U,
                                               group.var='birth.year')
  saveRDS(natl.U.direct.yearly,'natl_yearly_direct_U.rds')
  
  natl.R.direct.yearly <- getEduc.Direct.group(splitted.educ= educ.splitted.R,
                                               group.var='birth.year')
  saveRDS(natl.R.direct.yearly,'natl_yearly_direct_R.rds')
  
}


################################################################
######### subnational direct estimates (admin-1, 5 years group)
################################################################

max.year <- max(educ.splitted.overall$birth.year)
min.year <- min(educ.splitted.overall$birth.year)

age_group_length <- 5

setwd(paste0(res.dir,'/Direct_',folder.suffix,'/'))


### overall

if(file.exists('adm1_agegrp_direct_overall.rds')){
  
  adm1.agegrp.direct.overall <- readRDS('adm1_agegrp_direct_overall.rds')
  
}else{
  
  educ.splitted.admin1.overall <- merge_adm_info(data = educ.splitted.overall,
                                              adm1.info = admin1_info$data,
                                              adm2.info = admin2_info$data,
                                              cluster.info = cluster.info)
  
  ### define birth year group
  educ.splitted.admin1.overall <- educ.splitted.admin1.overall %>%
    mutate(offset = max.year %% age_group_length - age_group_length %/% 2,
           birth.year.group = round((birth.year - offset) / age_group_length) * age_group_length + offset
    ) %>%select(-offset) 
  
  educ.splitted.admin1.overall <- educ.splitted.admin1.overall %>%
    mutate(birth.year.group = if_else(birth.year.group < min.year, birth.year.group + age_group_length, birth.year.group))
  
  educ.splitted.admin1.overall <- educ.splitted.admin1.overall %>%
    mutate(birth.year.group = if_else(birth.year.group > max.year, birth.year.group - age_group_length, birth.year.group))
  
  
  ### loop over regions 
  region.list <- unique(educ.splitted.admin1.overall$admin1.char)

  adm1.educ.years <- data.frame()
  adm1.survival.p <- data.frame()
  adm1.cond.survival.p <- data.frame()
  adm1.grade.dist.p <- data.frame()
  
  for(tmp.region in region.list ){
    
    tmp.educ <- educ.splitted.admin1.overall[educ.splitted.admin1.overall$admin1.char ==tmp.region,]
    
    ### remove years with too few observations 
    tmp.educ <- tmp.educ %>% 
      group_by(birth.year.group) %>% 
      filter(n_distinct(v001) >= 2)
    
    tmp.educ <- as.data.frame(tmp.educ)
    
    tmp.res <- getEduc.Direct.group(splitted.educ= tmp.educ,
                                    group.var = 'birth.year.group')
    
    tmp.res.educ <- tmp.res$educ_years
    tmp.res.surv.p <- tmp.res$surv_p
    tmp.res.cond.p <- tmp.res$cond_surv_p
    tmp.res.grade.dist.p <- tmp.res$grade_dist_p
    
    tmp.res.educ$admin1.char <- tmp.region
    tmp.res.surv.p$admin1.char <- tmp.region
    tmp.res.cond.p$admin1.char <- tmp.region
    tmp.res.grade.dist.p$admin1.char <- tmp.region
    
    adm1.educ.years <- rbind(tmp.res.educ,adm1.educ.years)
    adm1.survival.p <- rbind(tmp.res.surv.p,adm1.survival.p)
    adm1.cond.survival.p <- rbind(tmp.res.cond.p,adm1.cond.survival.p)
    adm1.grade.dist.p <- rbind(tmp.res.grade.dist.p,adm1.grade.dist.p)
    
  }
  
  adm1.agegrp.direct.overall <- list(surv_p = adm1.survival.p,
                                     cond_surv_p = adm1.cond.survival.p,
                                     grade_dist_p = adm1.grade.dist.p,
                                     educ_years = adm1.educ.years)
  
  saveRDS(adm1.agegrp.direct.overall ,file= 'adm1_agegrp_direct_overall.rds')
  
}

### urban specific

if(file.exists('adm1_agegrp_direct_U.rds')){
  
  adm1.agegrp.direct.U <- readRDS('adm1_agegrp_direct_U.rds')
  
}else{
  
  educ.splitted.admin1.U <- merge_adm_info(data = educ.splitted.U,
                                           adm1.info = admin1_info$data,
                                           adm2.info = admin2_info$data,
                                           cluster.info = cluster.info)
  
  ### define birth year group
  educ.splitted.admin1.U <- educ.splitted.admin1.U %>%
    mutate(offset = max.year %% age_group_length - age_group_length %/% 2,
           birth.year.group = round((birth.year - offset) / age_group_length) * age_group_length + offset
    ) %>%select(-offset) 
  
  educ.splitted.admin1.U <- educ.splitted.admin1.U %>%
    mutate(birth.year.group = if_else(birth.year.group < min.year, birth.year.group + age_group_length, birth.year.group))
  
  educ.splitted.admin1.U <- educ.splitted.admin1.U %>%
    mutate(birth.year.group = if_else(birth.year.group > max.year, birth.year.group - age_group_length, birth.year.group))
  
  ### loop through regions
  region.list <- unique(educ.splitted.admin1.U$admin1.char)
  
  adm1.educ.years <- data.frame()
  adm1.survival.p <- data.frame()
  adm1.cond.survival.p <- data.frame()
  adm1.grade.dist.p <- data.frame()
  
  for(tmp.region in region.list ){
    
    tmp.educ <- educ.splitted.admin1.U[educ.splitted.admin1.U$admin1.char ==tmp.region,]
    
    ### remove years with too few observations 
    tmp.educ <- tmp.educ %>% 
      group_by(birth.year.group) %>% 
      filter(n_distinct(v001) >= 2)
    
    tmp.educ <- as.data.frame(tmp.educ)
    
    
    tmp.res <- getEduc.Direct.group(splitted.educ = tmp.educ, group.var = 'birth.year.group')

    tmp.res.educ <- tmp.res$educ_years
    tmp.res.surv.p <- tmp.res$surv_p
    tmp.res.cond.p <- tmp.res$cond_surv_p
    tmp.res.grade.dist.p <- tmp.res$grade_dist_p
    
    tmp.res.educ$admin1.char <- tmp.region
    tmp.res.surv.p$admin1.char <- tmp.region
    tmp.res.cond.p$admin1.char <- tmp.region
    tmp.res.grade.dist.p$admin1.char <- tmp.region
    
    adm1.educ.years <- rbind(tmp.res.educ,adm1.educ.years)
    adm1.survival.p <- rbind(tmp.res.surv.p,adm1.survival.p)
    adm1.cond.survival.p <- rbind(tmp.res.cond.p,adm1.cond.survival.p)
    adm1.grade.dist.p <- rbind(tmp.res.grade.dist.p,adm1.grade.dist.p)
    
  }
  
  adm1.agegrp.direct.U <- list(surv_p = adm1.survival.p,
                               cond_surv_p = adm1.cond.survival.p,
                               grade_dist_p = adm1.grade.dist.p,
                               educ_years = adm1.educ.years)
  
  
  saveRDS(adm1.agegrp.direct.U ,file= 'adm1_agegrp_direct_U.rds')
  
}



### rural specific

if(file.exists('adm1_agegrp_direct_R.rds')){
  
  adm1.agegrp.direct.R <- readRDS('adm1_agegrp_direct_R.rds')
  
}else{
  
  educ.splitted.admin1.R <- merge_adm_info(data = educ.splitted.R,
                                           adm1.info = admin1_info$data,
                                           adm2.info = admin2_info$data,
                                           cluster.info = cluster.info)
  
  ### define birth year group
  educ.splitted.admin1.R <- educ.splitted.admin1.R %>%
    mutate(offset = max.year %% age_group_length - age_group_length %/% 2,
           birth.year.group = round((birth.year - offset) / age_group_length) * age_group_length + offset
    ) %>%select(-offset) 
  
  educ.splitted.admin1.R <- educ.splitted.admin1.R %>%
    mutate(birth.year.group = if_else(birth.year.group < min.year, birth.year.group + age_group_length, birth.year.group))
  
  educ.splitted.admin1.R <- educ.splitted.admin1.R %>%
    mutate(birth.year.group = if_else(birth.year.group > max.year, birth.year.group - age_group_length, birth.year.group))
  
  ### loop through regions
  region.list <- unique(educ.splitted.admin1.R$admin1.char)
  
  adm1.educ.years <- data.frame()
  adm1.survival.p <- data.frame()
  adm1.cond.survival.p <- data.frame()
  adm1.grade.dist.p <- data.frame()
  
  for(tmp.region in region.list ){
    
    tmp.educ <- educ.splitted.admin1.R[educ.splitted.admin1.R$admin1.char ==tmp.region,]
    
    ### remove years with too few observations 
    tmp.educ <- tmp.educ %>% 
      group_by(birth.year.group) %>% 
      filter(n_distinct(v001) >= 2)
    
    tmp.educ <- as.data.frame(tmp.educ)
    
    
    tmp.res <- getEduc.Direct.group(splitted.educ = tmp.educ, group.var = 'birth.year.group')
    
    tmp.res.educ <- tmp.res$educ_years
    tmp.res.surv.p <- tmp.res$surv_p
    tmp.res.cond.p <- tmp.res$cond_surv_p
    tmp.res.grade.dist.p <- tmp.res$grade_dist_p
    
    tmp.res.educ$admin1.char <- tmp.region
    tmp.res.surv.p$admin1.char <- tmp.region
    tmp.res.cond.p$admin1.char <- tmp.region
    tmp.res.grade.dist.p$admin1.char <- tmp.region
    
    adm1.educ.years <- rbind(tmp.res.educ,adm1.educ.years)
    adm1.survival.p <- rbind(tmp.res.surv.p,adm1.survival.p)
    adm1.cond.survival.p <- rbind(tmp.res.cond.p,adm1.cond.survival.p)
    adm1.grade.dist.p <- rbind(tmp.res.grade.dist.p,adm1.grade.dist.p)
    
  }
  
  adm1.agegrp.direct.R <- list(surv_p = adm1.survival.p,
                               cond_surv_p = adm1.cond.survival.p,
                               grade_dist_p = adm1.grade.dist.p,
                               educ_years = adm1.educ.years)
  
  saveRDS(adm1.agegrp.direct.R ,file= 'adm1_agegrp_direct_R.rds')
  
}



################################################################
######### sanity checks
################################################################

### test if the CIs are correct 
if(FALSE){
  
  
  natl.educ.direct.unadj <- getEduc.Direct(educ.comp= educ.splitted.overall)
  
  # grade specific probabilities 
  t=7
  educ.exactly.t <- educ_dat_ind
  educ.exactly.t$exactly.t <- as.integer(educ.exactly.t$educ.yrs==t)
  educ.tmp.design <- svydesign(id = ~cluster, strata = ~v022, 
                               weights = ~weights, data = educ.exactly.t, nest = TRUE)
  
  test_estimate <- svyglm(exactly.t ~ 1, design = educ.tmp.design, family = binomial)
  expit(confint(test_estimate))
  expit(test_estimate$coefficients)
  natl.educ.direct.unadj$grade_dist_prob[t+1,]
  
  
  # continuation probabilities 
  educ.more.than.x <- educ_dat_ind[educ_dat_ind$educ.yrs>4,]
  educ.more.than.x$more.than.x <- as.integer(1- (educ.more.than.x$educ.yrs > 5))
  educ.tmp.design <- svydesign(id = ~cluster, strata = ~v022, 
                               weights = ~weights, data = educ.more.than.x, nest = TRUE)
  
  test_estimate <- svyglm(more.than.x ~ 1, design = educ.tmp.design, family = binomial)
  
  expit(confint(test_estimate))
  expit(test_estimate$coefficients)
  natl.educ.direct.unadj$cond_survival_p[6,]
  
  # survival probabilities (P(Y>=i))
  educ_dat_ind$some_educ <- educ_dat_ind$v133>4
  tmp_test_design <- svydesign(id = ~cluster, strata = ~v022, weights = ~weights, data = educ_dat_ind, nest = TRUE)
  test_estimate <- svyglm(some_educ ~ 1, design = tmp_test_design, family = binomial)
  
  expit(confint(test_estimate))
  expit(test_estimate$coefficients)
  natl.educ.direct.unadj$survival_p[5,]
  
  # mean years of education
  tmp_test_design <- svydesign(id = ~cluster, strata = ~v022, weights = ~weights, data = educ_dat_ind, nest = TRUE)
  test_estimate <- svyglm(educ.yrs ~ 1, design = tmp_test_design, family = gaussian)
  
  (confint(test_estimate))
  (test_estimate$coefficients)
  natl.educ.direct.unadj$mean_educ_years
  
  # mean years of education by birth year
  # tmp_test_design <- svydesign(id = ~cluster, strata = ~v022, weights = ~weights, data = educ_dat_ind, nest = TRUE)
  # test_estimate <- svyglm(educ.yrs ~ (-1)+as.factor(birth.year), design = tmp_test_design, family = gaussian)
  # 
  # (confint(test_estimate))
  # (test_estimate$coefficients)
  
}


# test if the results match with the natl direct estimates 
if(FALSE){
  # mean years of education
  tmp_test_design <- svydesign(id = ~cluster, strata = ~v022, weights = ~weights, data = educ_dat_ind, nest = TRUE)
  weighted_yrs_by_birth_year <- svyglm(educ.yrs ~ -1+ as.factor(birth.year), design = tmp_test_design, family = gaussian)
  
  (confint(weighted_yrs_by_birth_year))
  
}


### 5-year groups to check proportionality on log odds, for surveyGLM model
if(FALSE){
  educ.splitted.natl.adj$birth.year.group <- round((educ.splitted.natl.adj$birth.year/5))*5
  
  natl.educ.direct.adj.year.group <- getEduc.Direct.group(splitted.educ= educ.splitted.natl.adj,
                                                          group.var='birth.year.group')
}


# check proportional odds assumption
if(FALSE){
  continuation_odd_test <- natl.educ.direct.adj.year.group$cond_survival_p
  continuation_odd_test$logit_prob <- logit(continuation_odd_test$prob)
  # remove 0 or 1 estimates
  continuation_odd_test <- continuation_odd_test[abs(continuation_odd_test$logit_prob)<8,]
  
  
  library(ggplot2)
  ggplot(data = continuation_odd_test, 
         aes(x = grade_int, y = logit_prob, group = as.factor(group), color = as.factor(group))) +
    geom_line() +
    geom_point() 
}




