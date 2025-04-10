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

if(!dir.exists(paths = paste0('/Direct_modified'))){ 
  dir.create(file.path('/Direct_modified'))
}




### directory to store simulated censorship data

for(sim_age_group in c(25,30,35,40)){
  
  res_folder_suffix = paste0('sim_censored_',sim_age_group,'_',sim_age_group+4)
  
  if(!dir.exists(paths = paste0('Direct_unadj/',res_folder_suffix))){ 
    dir.create(file.path(paste0('Direct_unadj/',res_folder_suffix)))
  }
  
  for(rep_idx in 1:10){
    if(!dir.exists(paths = paste0('Direct_unadj/',res_folder_suffix,'/rep_',rep_idx))){ 
      dir.create(file.path(paste0('Direct_unadj/',res_folder_suffix,'/rep_',rep_idx)))
    }
  }
  
  
}

for(sim_age_group in c(25,30,35,40)){
  
  res_folder_suffix = paste0('sim_censored_',sim_age_group,'_',sim_age_group+4)
  
  if(!dir.exists(paths = paste0('Direct_modified/',res_folder_suffix))){ 
    dir.create(file.path(paste0('Direct_modified/',res_folder_suffix)))
  }
  
  for(rep_idx in 1:10){
    if(!dir.exists(paths = paste0('Direct_modified/',res_folder_suffix,'/rep_',rep_idx))){ 
      dir.create(file.path(paste0('Direct_modified/',res_folder_suffix,'/rep_',rep_idx)))
    }
  }
  
  
}

################################################################
#########   load aggregation functions
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
######### determine whether modified or unadjusted
################################################################

modified_ind = F

if(modified_ind){
  account.censor.ind=T
  folder.suffix='modified'
}else{
  account.censor.ind = F
  folder.suffix = 'unadj'
}


################################################################
######### analysis
################################################################

### simulated censored
for(rep_idx in c(1:10)){
  
  print(rep_idx)
  for(sim_age_group in c(25,30,35,40)){
    
    
    print(sim_age_group)
    
    res_folder_suffix = paste0('sim_censored_',sim_age_group,'_',sim_age_group+4)
    
    ### load individual level data
    setwd(paste0(data.dir,'/prepared_dat/sim_censored/rep_',rep_idx))
    educ_dat_ind <- readRDS( paste0('educ_sim_',sim_age_group,'_',sim_age_group+4,'_overall.rds'))
    
    
    
    ################################################################
    ######### prepare analysis data set
    ################################################################
    
    ### overall
    educ.splitted.overall <- getEduc_splitted_sim(data = educ_dat_ind,
                                              compact = T, 
                                              account.censor=account.censor.ind,
                                              compact.by =  c('v001','v022', "weights","birth.year"))
    
    educ.splitted.overall$strata <- educ.splitted.overall$v022
    
    ### urban
    educ.splitted.U <- getEduc_splitted_sim(data = educ_dat_ind[educ_dat_ind$urban=='urban',],
                                        compact = T, 
                                        account.censor=account.censor.ind,
                                        compact.by =  c('v001','v022', "weights","birth.year"))
    
    educ.splitted.U$strata <- educ.splitted.U$v022
    
    
    ### rural
    educ.splitted.R <- getEduc_splitted_sim(data = educ_dat_ind[educ_dat_ind$urban=='rural',],
                                        compact = T, 
                                        account.censor=account.censor.ind,
                                        compact.by =  c('v001','v022', "weights","birth.year"))
    
    educ.splitted.R$strata <- educ.splitted.R$v022
    
    
    ################################################################
    ######### national direct estimates 
    ################################################################
    
    setwd(paste0(res.dir,'/Direct_',folder.suffix,'/',res_folder_suffix,'/rep_',rep_idx))
    
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
    
    
  }
  
}





