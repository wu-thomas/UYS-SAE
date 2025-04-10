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

### directory to store simulated censorship data

for(sim_age_group in c(25,30,35,40)){
  
  res_folder_suffix = paste0('sim_censored_',sim_age_group,'_',sim_age_group+4)
  
  if(!dir.exists(paths = paste0('GLM/',res_folder_suffix))){ 
    dir.create(file.path(paste0('GLM/',res_folder_suffix)))
  }
  
  for(rep_idx in 1:10){
    if(!dir.exists(paths = paste0('GLM/',res_folder_suffix,'/rep_',rep_idx))){ 
      dir.create(file.path(paste0('GLM/',res_folder_suffix,'/rep_',rep_idx)))
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
######### load data
################################################################

### original data or simulated censored
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
    educ.adj.overall <- rbind(getEduc_splitted_sim(data = educ_dat_ind[educ_dat_ind$birth.year<=survey_year-24,],
                                         compact = T,
                                         account.censor=T,
                                         compact.by =  c('v001','v022', "weights","birth.year")),
                              getEduc_splitted(data = educ_dat_ind[educ_dat_ind$birth.year>survey_year-24,],
                                                   compact = T,
                                                   account.censor=T,
                                                   compact.by =  c('v001','v022', "weights","birth.year"))
    )
    
    educ.adj.overall$strata <- educ.adj.overall$v022
    
    ### urban/rural specific
    educ.adj.U <- rbind(getEduc_splitted_sim(data = educ_dat_ind[educ_dat_ind$birth.year<=survey_year-24&
                                                                   educ_dat_ind$urban=='urban',],
                                             compact = T,
                                             account.censor=T,
                                             compact.by =  c('v001','v022', "weights","birth.year")),
                        getEduc_splitted(data = educ_dat_ind[educ_dat_ind$birth.year>survey_year-24&
                                                               educ_dat_ind$urban=='urban',],
                                         compact = T,
                                         account.censor=T,
                                         compact.by =  c('v001','v022', "weights","birth.year"))
    )
    
    educ.adj.U$strata <- educ.adj.U$v022
    
    
    
    educ.adj.R <- rbind(getEduc_splitted_sim(data = educ_dat_ind[educ_dat_ind$birth.year<=survey_year-24&
                                                                   educ_dat_ind$urban=='rural',],
                                             compact = T,
                                             account.censor=T,
                                             compact.by =  c('v001','v022', "weights","birth.year")),
                        getEduc_splitted(data = educ_dat_ind[educ_dat_ind$birth.year>survey_year-24&
                                                               educ_dat_ind$urban=='rural',],
                                         compact = T,
                                         account.censor=T,
                                         compact.by =  c('v001','v022', "weights","birth.year"))
    )
    
    educ.adj.R$strata <- educ.adj.R$v022
    
    ################################################################
    ######### national survey GLM estimates 
    ################################################################
    
    setwd(paste0(res.dir,'/GLM/',res_folder_suffix,'/rep_',rep_idx))
    
    ### overall
    if(file.exists('natl_yearly_GLM_overall.rds')){
      
      natl.overall.adj.GLM.yearly <- readRDS('natl_yearly_GLM_overall.rds')
      
    }else{
      
      natl.overall.adj.GLM.yearly <- getEduc.GLM.parallel(educ.group.comp= educ.adj.overall,
                                                          group.var='birth.year')
      
      saveRDS(natl.overall.adj.GLM.yearly,file='natl_yearly_GLM_overall.rds')
      
    }
    
    
    # ### urban
    # 
    # if(file.exists('natl_yearly_GLM_U.rds')){
    # 
    #   natl.U.adj.GLM.yearly <- readRDS('natl_yearly_GLM_U.rds')
    # 
    # }else{
    # 
    #   natl.U.adj.GLM.yearly <- getEduc.GLM(educ.group.comp= educ.adj.U,
    #                                              group.var='birth.year')
    # 
    #   saveRDS(natl.U.adj.GLM.yearly,file='natl_yearly_GLM_U.rds')
    # 
    # }
    # 
    # ### rural
    # 
    # if(file.exists('natl_yearly_GLM_R.rds')){
    #   natl.R.adj.GLM.yearly <- readRDS('natl_yearly_GLM_R.rds')
    # }else{
    #   natl.R.adj.GLM.yearly <- getEduc.GLM(educ.group.comp= educ.adj.R,
    #                                        group.var='birth.year')
    # 
    #   saveRDS(natl.R.adj.GLM.yearly,file='natl_yearly_GLM_R.rds')
    # 
    # }
    
    
  }
  
  
  
}


