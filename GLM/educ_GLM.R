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
library(future.apply)


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

if(!dir.exists(paths = paste0('GLM'))){ 
  dir.create(file.path('GLM'))
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

### individual level
setwd(paste0(data.dir,'/prepared_dat'))

if(file.exists(paste0('educ_dat_ind_',survey_year,'.rds'))){
  
  educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))
  
}

################################################################
######### prepare analysis data set
################################################################

### overall
educ.adj.overall <- getEduc_splitted(data = educ_dat_ind,
                                     compact = T,
                                     account.censor=T,
                                     compact.by =  c('v001','v022', "weights","birth.year"))

educ.adj.overall$strata <- educ.adj.overall$v022

### urban/rural specific
educ.adj.U <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='urban',],
                               compact = T, 
                               account.censor=T,
                               compact.by =  c('v001','v022', "weights","birth.year"))

educ.adj.U$strata <- educ.adj.U$v022



educ.adj.R <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='rural',],
                               compact = T, 
                               account.censor=T,
                               compact.by =  c('v001','v022', "weights","birth.year"))

educ.adj.R$strata <- educ.adj.R$v022

################################################################
######### national survey GLM estimates 
################################################################

setwd(paste0(res.dir,'/GLM'))

### overall
if(file.exists('natl_yearly_GLM_overall.rds')){
  
  natl.overall.adj.GLM.yearly <- readRDS('natl_yearly_GLM_overall.rds')
  
}else{
  
  natl.overall.adj.GLM.yearly <- getEduc.GLM.parallel(educ.group.comp= educ.adj.overall,
                                              group.var='birth.year')
  
  saveRDS(natl.overall.adj.GLM.yearly,file='natl_yearly_GLM_overall.rds')
  
}


### urban
if(file.exists('natl_yearly_GLM_U.rds')){
  
  natl.U.adj.GLM.yearly <- readRDS('natl_yearly_GLM_U.rds')
  
}else{
  
  natl.U.adj.GLM.yearly <- getEduc.GLM.parallel(educ.group.comp= educ.adj.U,
                                       group.var='birth.year')
  
  saveRDS(natl.U.adj.GLM.yearly,file='natl_yearly_GLM_U.rds')
  
}

### rural
if(file.exists('natl_yearly_GLM_R.rds')){
  natl.R.adj.GLM.yearly <- readRDS('natl_yearly_GLM_R.rds')
}else{
  natl.R.adj.GLM.yearly <- getEduc.GLM.parallel(educ.group.comp= educ.adj.R,
                                       group.var='birth.year')
  
  saveRDS(natl.R.adj.GLM.yearly,file='natl_yearly_GLM_R.rds')
  
}




################################################################
######### subnational, yearly, survey GLM estimates 
################################################################
setwd(paste0(res.dir,'/GLM'))

### overall
if(file.exists('adm1_yearly_GLM_overall.rds')){
  
  adm1.yearly.GLM.overall <- readRDS('adm1_yearly_GLM_overall.rds')
  
}else{
  educ.adj.admin1.overall <- merge_adm_info(data = educ.adj.overall,
                                            adm1.info = admin1_info$data,
                                            adm2.info = admin2_info$data,
                                            cluster.info = cluster.info)
  
  adm1.yearly.GLM.overall <- getEduc.GLM2.parallel(educ.group.comp= educ.adj.admin1.overall,
                                                   group.var=c('birth.year','admin1.char'))
  
  
  saveRDS(adm1.yearly.GLM.overall,file='adm1_yearly_GLM_overall.rds')
  
}

### urban

if(file.exists('adm1_yearly_GLM_U.rds')){
  
  adm1.yearly.GLM.U. <- readRDS('adm1_yearly_GLM_U.rds')
  
}else{
  educ.adj.admin1.U <- merge_adm_info(data = educ.adj.U,
                                      adm1.info = admin1_info$data,
                                      adm2.info = admin2_info$data,
                                      cluster.info = cluster.info)
  
  adm1.yearly.GLM.U <- getEduc.GLM2.parallel(educ.group.comp= educ.adj.admin1.U,
                                             group.var=c('birth.year','admin1.char'))
  
  saveRDS(adm1.yearly.GLM.U,file='adm1_yearly_GLM_U.rds')
  
}


### rural
if(file.exists('adm1_yearly_GLM_R.rds')){
  
  adm1.yearly.GLM.R <- readRDS('adm1_yearly_GLM_R.rds')
  
}else{
  educ.adj.admin1.R <- merge_adm_info(data = educ.adj.R,
                                      adm1.info = admin1_info$data,
                                      adm2.info = admin2_info$data,
                                      cluster.info = cluster.info)
  
  adm1.yearly.GLM.R <- getEduc.GLM2.parallel(educ.group.comp= educ.adj.admin1.R,
                                             group.var=c('birth.year','admin1.char'))
  
  saveRDS(adm1.yearly.GLM.R,file='adm1_yearly_GLM_R.rds')
  
}

# library(future.apply)
# 
# 
# tmp_test_dat <- educ.adj.admin1.overall[educ.adj.admin1.overall$admin1.num<6,]
# 
# 
# start.time <- Sys.time()
# 
# test.unpara <- getEduc.GLM2(educ.group.comp= tmp_test_dat,
#                                         group.var=c('birth.year','admin1.char'))
# 
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# start.time <- Sys.time()
# test.para <- getEduc.GLM2.parallel(educ.group.comp= tmp_test_dat,
#                                         group.var=c('birth.year','admin1.char'))
# 
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# 
# 







################################################################
######### subnational, 5 yrs, survey GLM estimates 
################################################################

max.year <- max(educ.adj.overall$birth.year)
min.year <- min(educ.adj.overall$birth.year)

age_group_length <- 5


setwd(paste0(res.dir,'/GLM'))

### overall
if(file.exists('adm1_5yr_GLM_overall.rds')){
  
  adm1.5yr.GLM.overall <- readRDS('adm1_5yr_GLM_overall.rds')
  
}else{
  educ.adj.admin1.overall <- merge_adm_info(data = educ.adj.overall,
                                            adm1.info = admin1_info$data,
                                            adm2.info = admin2_info$data,
                                            cluster.info = cluster.info)
  
  ### define birth year group
  educ.adj.admin1.overall <- educ.adj.admin1.overall %>%
    mutate(offset = max.year %% age_group_length - age_group_length %/% 2,
           birth.year.group = round((birth.year - offset) / age_group_length) * age_group_length + offset
    ) %>%select(-offset) 
  
  educ.adj.admin1.overall <- educ.adj.admin1.overall %>%
    mutate(birth.year.group = if_else(birth.year.group < min.year, birth.year.group + age_group_length, birth.year.group))
  
  educ.adj.admin1.overall <- educ.adj.admin1.overall %>%
    mutate(birth.year.group = if_else(birth.year.group > max.year, birth.year.group - age_group_length, birth.year.group))
  
  
  
  adm1.5yr.GLM.overall <- getEduc.GLM2.parallel(educ.group.comp=  educ.adj.admin1.overall,
                                                   group.var=c('birth.year.group','admin1.char'))
  
  setwd(paste0(res.dir,'/GLM'))
  
  saveRDS(adm1.5yr.GLM.overall,file='adm1_5yr_GLM_overall.rds')
  
}

### urban
setwd(paste0(res.dir,'/GLM'))

if(file.exists('adm1_5yr_GLM_U.rds')){
  
  adm1.5yr.GLM.U <- readRDS('adm1_5yr_GLM_U.rds')
  
}else{
  educ.adj.admin1.U <- merge_adm_info(data = educ.adj.U,
                                      adm1.info = admin1_info$data,
                                      adm2.info = admin2_info$data,
                                      cluster.info = cluster.info)
  
  ### define birth year group
  educ.adj.admin1.U <- educ.adj.admin1.U %>%
    mutate(offset = max.year %% age_group_length - age_group_length %/% 2,
           birth.year.group = round((birth.year - offset) / age_group_length) * age_group_length + offset
    ) %>%select(-offset) 
  
  educ.adj.admin1.U <- educ.adj.admin1.U %>%
    mutate(birth.year.group = if_else(birth.year.group < min.year, birth.year.group + age_group_length, birth.year.group))
  
  educ.adj.admin1.U <- educ.adj.admin1.U %>%
    mutate(birth.year.group = if_else(birth.year.group > max.year, birth.year.group - age_group_length, birth.year.group))
  
  
  
  adm1.5yr.GLM.U <- getEduc.GLM2.parallel(educ.group.comp= educ.adj.admin1.U,
                                             group.var=c('birth.year.group','admin1.char'))
  
  setwd(paste0(res.dir,'/GLM'))
  
  saveRDS(adm1.5yr.GLM.U,file='adm1_5yr_GLM_U.rds')
  
}


### rural
if(file.exists('adm1_5yr_GLM_R.rds')){
  
  adm1.5yr.GLM.R <- readRDS('adm1_5yr_GLM_R.rds')
  
}else{
  educ.adj.admin1.R <- merge_adm_info(data = educ.adj.R,
                                      adm1.info = admin1_info$data,
                                      adm2.info = admin2_info$data,
                                      cluster.info = cluster.info)
  
  ### define birth year group
  educ.adj.admin1.R <- educ.adj.admin1.R %>%
    mutate(offset = max.year %% age_group_length - age_group_length %/% 2,
           birth.year.group = round((birth.year - offset) / age_group_length) * age_group_length + offset
    ) %>%select(-offset) 
  
  educ.adj.admin1.R <- educ.adj.admin1.R %>%
    mutate(birth.year.group = if_else(birth.year.group < min.year, birth.year.group + age_group_length, birth.year.group))
  
  educ.adj.admin1.R <- educ.adj.admin1.R %>%
    mutate(birth.year.group = if_else(birth.year.group > max.year, birth.year.group - age_group_length, birth.year.group))
  
  
  adm1.5yr.GLM.R <- getEduc.GLM2.parallel(educ.group.comp= educ.adj.admin1.R,
                                             group.var=c('birth.year.group','admin1.char'))
  
  saveRDS(adm1.5yr.GLM.R,file='adm1_5yr_GLM_R.rds')
  
}

