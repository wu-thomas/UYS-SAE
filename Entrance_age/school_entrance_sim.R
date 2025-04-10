##############################################################################
#########   load packages
##############################################################################

rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.


# Load libraries and info ----------------------------------------------------------
library(dplyr)
library(labelled)
library(survival)
library(haven)
library(rdhs)
library(surveyPrev)
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


academic_year_start_month <- 1
start_age_range <- c(6, 15)

##############################################################################
#########   load helper functions 
##############################################################################

setwd(paste(code.dir))
source('helper_functions/school_entrance_helper.R')
source('helper_functions/helper_educ_direct_GLM.R')


##############################################################################
#########   create folders for prepared data
##############################################################################

setwd(paste(data.dir))

dir.create(file.path('.', 'prepared_dat'))

if(!dir.exists(paths = paste0(data.dir,'/prepared_dat/entrance_age'))){ 
  dir.create(file.path(paste0(data.dir,'/prepared_dat/entrance_age')))
}


if(!dir.exists(paths = paste0(data.dir,'/prepared_dat/sim_censored'))){ 
  dir.create(file.path(paste0(data.dir,'/prepared_dat/sim_censored')))
}

for (rep_idx in c(1:10)){
  
  if(!dir.exists(paths = paste0(data.dir,'/prepared_dat/sim_censored/rep_',rep_idx))){ 
    dir.create(file.path(paste0(data.dir,'/prepared_dat/sim_censored/rep_',rep_idx)))
  }
  
}

##############################################################################
#########   load DHS data and processed education data
##############################################################################


setwd(paste0(data.dir,'/DHS_data'))

raw.geo.dat <- readRDS(file=paste0(country,'_',survey_year,'_GPS.rds'))
raw.IR.dat <- readRDS(file=paste0(country,'_',survey_year,'_IR.rds'))
raw.PR.dat <- readRDS(file=paste0(country,'_',survey_year,'_PR.rds'))
cluster.info <- readRDS(file=paste0(country,'_',survey_year,'_cluster_info.rds'))

setwd(paste0(data.dir,'/prepared_dat'))

educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))

##############################################################################
#########   estimate school entrance age (left-censored)
##############################################################################

# Merge education data with raw IR data to get interview date etc
educ_IR_merged <- educ_dat_ind %>%
  left_join(raw.IR.dat[, c('caseid', 'v008', 'v011')], by = 'caseid') %>%
  rename(
    dob_cmc = v011,            # Date of birth in CMC format
    survey_time_cmc = v008     # Survey time in CMC format
  )



### !!! very important: valid range: age_range
### !!! very important: if not start school at age age_upper, assume will never start 
### (not in study population and thus no need to worry about right censor)
educ_start_ind <- process_start_educ_age(educ_IR_merged_dat=educ_IR_merged,
                                         start_month = academic_year_start_month,
                                         age_range = start_age_range)

educ_start_ind$educ.yrs <- educ_start_ind$entrance_age_rev
educ_start_ind <- educ_start_ind %>% filter(educ_start_ind$birth.year>(survey_year-30))  
  
### country specific adjustment on stratification variable 
if(FALSE){
  #educ_start_ind$v022 <- paste0(educ_start_ind$admin1.name,':',educ_start_ind$urban)
}

################################################
####   urban and rural specific pattern
################################################

### simulate three consecutive 5-year groups of censorship (targeting 25-29,30-34 and 35-39 age groups)
### similar to the current 15-19 censorship

entrance_age_dist_U <- cal_start_age_dist(educ_start_data = educ_start_ind[educ_start_ind$urban=='urban',],
                                          upper_age_limit = start_age_range[2])

entrance_age_dist_R <- cal_start_age_dist(educ_start_data = educ_start_ind[educ_start_ind$urban=='rural',],
                                        upper_age_limit = start_age_range[2])

#setwd(paste0(data.dir,'/prepared_dat/entrance_age'))
#saveRDS(entrance_age_dist_U,file=paste0('entrance_age_dist_U_',survey_year,'.rds'))
#saveRDS(entrance_age_dist_R,file=paste0('entrance_age_dist_R_',survey_year,'.rds'))



educ_all_to_sim_U <- educ_IR_merged %>% filter(urban=='urban')
educ_all_to_sim_R <- educ_IR_merged %>% filter(urban=='rural')


################################################
####  simulation (repeat 10 times)
################################################

seed_list <- 2024+c(0:9)
  
  
for(rep_idx in 1:10){
  
  seed_tmp <- seed_list[rep_idx]
  
  #(targeting 25-29,30-34 and 35-39 age groups)
  for(age_group in c(25,30,35,40)){ 
    
    setwd(paste0(data.dir,'/prepared_dat/sim_censored/rep_',rep_idx))
    ### urban 
    set.seed(seed_tmp)
    
    educ_censored_sim_U <- simulate_censor_cohort(
      educ_all = educ_all_to_sim_U,
      educ_to_sim = educ_all_to_sim_U %>% filter(birth.year>=max(educ_IR_merged$birth.year-(age_group-10))&
                                                   birth.year<max(educ_IR_merged$birth.year-(age_group-15))),
      entrance_age_distribution = entrance_age_dist_U,
      start_month = academic_year_start_month,
      sim_survey_cmc = NULL
    )
    
    if(!file.exists(paste0('educ_sim_',age_group,'_',age_group+4,'_U.rds'))){
      saveRDS(educ_censored_sim_U,paste0('educ_sim_',age_group,'_',age_group+4,'_U.rds'))
    }  
    
    ### rural 
    set.seed(seed_tmp)
    
    educ_censored_sim_R <- simulate_censor_cohort(
      educ_all = educ_all_to_sim_R,
      educ_to_sim = educ_all_to_sim_R %>% filter(birth.year>=max(educ_IR_merged$birth.year-(age_group-10))&
                                                   birth.year<max(educ_IR_merged$birth.year-(age_group-15))),
      entrance_age_distribution = entrance_age_dist_R,
      start_month = academic_year_start_month,
      sim_survey_cmc = NULL
    )
    
    if(!file.exists(paste0('educ_sim_',age_group,'_',age_group+4,'_R.rds'))){
      saveRDS(educ_censored_sim_R,paste0('educ_sim_',age_group,'_',age_group+4,'_R.rds'))
    }  
    
    
    ### combine urban/rural to overall
    educ_censored_sim <- rbind(educ_censored_sim_U %>% filter(urban=='urban') ,
                               educ_censored_sim_R %>% filter(urban=='rural'))
    
    
    if(!file.exists(paste0('educ_sim_',age_group,'_',age_group+4,'_overall.rds'))){
      saveRDS(educ_censored_sim,paste0('educ_sim_',age_group,'_',age_group+4,'_overall.rds'))
    }
    
  }
  
  
  
}




### country specific stratification variable (typically no need to change, use v022)

# if(country=='Tanzania' & survey_year==2022){
#   for(rep_idx in 1:10){
#     
#     seed_tmp <- seed_list[rep_idx]
#     
#     setwd(paste0(data.dir,'/prepared_dat/sim_censored/rep_',rep_idx))
#     
#     for(age_group in c(25,30,35,40)){ 
#       
#       educ_censored_sim <- readRDS(paste0('educ_sim_',age_group,'_',age_group+4,'_overall.rds'))
#       
#       educ_censored_sim$v022 <- paste0(educ_censored_sim$admin1.name,':',educ_censored_sim$urban)
#       saveRDS(educ_censored_sim,paste0('educ_sim_',age_group,'_',age_group+4,'_overall.rds'))
#       
#       
#     }
#     
#   }
# }



### calculate delayed entrance statistics

# proportion of 15-year old still in school
aged_15 <- educ_dat_ind %>% filter(birth.year==2007)
mean(aged_15$in.school*aged_15$weights)

# proportion of 20-year old still in school
aged_20 <- educ_dat_ind %>% filter(birth.year==2002)
mean(aged_20$in.school*aged_20$weights)

