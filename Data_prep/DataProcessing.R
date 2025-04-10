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

##############################################################################
#########   load helper functions 
##############################################################################

setwd(paste(code.dir))
source('helper_functions/DataProcessing_helper.R')


##############################################################################
#########   create folders for prepared data
##############################################################################

setwd(paste(data.dir))

dir.create(file.path('.', 'prepared_dat'))

##############################################################################
###### RDHS Configration 
##############################################################################


## login
# set API to get DHS data -- you will need to change this to your information!

if(FALSE){
  rdhs::set_rdhs_config(email = "xxxxxxxxxxx",
                        project = "xxxxxxxxxxxxxxxxxxxxxxxxxx")
  
  rdhs::update_rdhs_config(email = "xxxxxxxxxxxxxxxxxxxxxxx", password = T,
                           project = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
}

##############################################################################
#########   load survey meta data
##############################################################################



setwd(paste0(data.dir))

if(file.exists('DHS_meta.rda')){
  load('DHS_meta.rda')
}else{
  
  ### run for the first time use saved object later
  DHS.country.meta <- rdhs::dhs_countries()
  DHS.survey.meta <- rdhs::dhs_surveys()
  DHS.dataset.meta <- rdhs::dhs_datasets()
  
  setwd(paste0(data.dir))
  
  save(DHS.country.meta,
       DHS.survey.meta,
       DHS.dataset.meta,
       file='DHS_meta.rda')
}

##############################################################################
#########   load polygon files
##############################################################################

setwd(paste0(data.dir,'/shapeFiles_gadm'))

if(file.exists('country_shp_analysis.rds')&
   file.exists('country_shp_smoothed.rds')&
   file.exists('admin1_info.rds')&
   file.exists('admin2_info.rds')&
   file.exists('poly_adm1_sf.rds')&
   file.exists('poly_adm2_sf.rds')
   
){
  
  country_shp_analysis <- readRDS('country_shp_analysis.rds')
  country_shp_smoothed <- readRDS('country_shp_smoothed.rds')
  admin1_info <- readRDS('admin1_info.rds')
  admin2_info <- readRDS('admin2_info.rds')
  poly.adm1.sf <- readRDS('poly_adm1_sf.rds')
  poly.adm2.sf <- readRDS('poly_adm2_sf.rds')
  
  
}else{

### run for the first time and use stored results later
  ### need to customize based on country specific information
  ### i.e. what GADM levels corresponds to admin-1 and admin-2 of our interest
  
  
  # directly download 
  #country_shp_analysis <- get_country_GADM(country=country,resolution=1)
  #country_shp_smoothed <- get_country_GADM(country=country,resolution=2)

  # load downloaded
  setwd(paste0(home.dir,'/Data/GADM_shp/',gadm.abbrev))
  country_shp_analysis <- readRDS(paste0(gadm.abbrev,'_GADM_analysis.rds'))
  country_shp_smoothed <- readRDS(paste0(gadm.abbrev,'_GADM_display.rds'))
  
    
  country_shp_analysis <- lapply(country_shp_analysis, function(x) {
    sf::st_set_crs(x, 4326)
  })
  country_shp_smoothed <- lapply(country_shp_smoothed, function(x) {
    sf::st_set_crs(x, 4326)
  })
  
  
  admin1_info <- surveyPrev::adminInfo(poly.adm = country_shp_analysis[[paste0('Admin-',GADM.admin1.level)]],
                                       admin = 1,by.adm=paste0("NAME_",GADM.admin1.level)) ### DHS admin-1 can be GADM admin-2!!
  admin1_info$data$admin1.char <- paste0("admin1_", 1:dim(admin1_info$data)[1])
  admin1_info$data$admin1.num <- c(1:dim(admin1_info$data)[1])
  
  admin2_info <- surveyPrev::adminInfo(poly.adm = country_shp_analysis[[paste0('Admin-',GADM.admin2.level)]],
                                       admin = 2,
                                       by.adm.upper = paste0("NAME_",GADM.admin1.level),
                                       by.adm=paste0("NAME_",GADM.admin2.level)) ### DHS admin-2 can be GADM admin-3!!
  admin2_info$data$admin2.char <- paste0("admin2_", 1:dim(admin2_info$data)[1])
  admin2_info$data$admin2.num <- c(1:dim(admin2_info$data)[1])
  
  admin2_info$data <- merge(admin2_info$data,admin1_info$data[,c('admin1.num','admin1.char','admin1.name')],
        by='admin1.name',all.x=T)
  
  
  
  poly.adm1.sf <- country_shp_analysis[[paste0('Admin-',GADM.admin1.level)]]
  poly.adm1.sf$admin1.num <- c(1:dim(poly.adm1.sf)[1])
  #poly.adm1.sf$admin1.char <- paste0("admin1_", 1:dim(poly.adm1.sf)[1])
  
  poly.adm2.sf <- country_shp_analysis[[paste0('Admin-',GADM.admin2.level)]]
  poly.adm2.sf$admin2.num <-  c(1:dim(poly.adm2.sf)[1])
  #poly.adm2.sf$admin2.char <- paste0("admin2_", 1:dim(poly.adm2.sf)[1])
  
  
  setwd(paste0(data.dir,'/shapeFiles_gadm'))
  
  saveRDS(country_shp_analysis,'country_shp_analysis.rds')
  saveRDS(country_shp_smoothed,'country_shp_smoothed.rds')
  saveRDS(admin1_info,'admin1_info.rds')
  saveRDS(admin2_info,'admin2_info.rds')
  saveRDS(poly.adm1.sf,'poly_adm1_sf.rds')
  saveRDS(poly.adm2.sf,'poly_adm2_sf.rds')
  
  
  #save(country_shp_analysis,
  #     country_shp_smoothed,
  #     admin1_info,admin2_info,
  #     file='shape_info.rda')
}

# setwd(paste0(data.dir,'/shapeFiles_gadm'))
# 
# old.adm2 <- st_read(paste0('gadm40_MDG_shp/gadm40_MDG_3.shp'))
# 
# old.adm2 <- geodata::gadm(country = 'MDG',version='4.0',
#                           level = 3,
#                           path = tempdir())
# old.adm2 <- sf::st_as_sf(old.adm2)
# old.adm2 <- sf::st_set_crs(old.adm2, 4326)
# 
# ggplot(data = old.adm2[23,]) +
#      annotation_map_tile(type = "osm") +
#      geom_sf() +
#      theme_void()


##############################################################################
#########   process DHS surveys
##############################################################################


setwd(paste0(data.dir,'/DHS_data'))

if(file.exists(paste0(country,'_',survey_year,'_GPS.rds'))&
   file.exists(paste0(country,'_',survey_year,'_IR.rds'))&
   file.exists(paste0(country,'_',survey_year,'_PR.rds'))&
   file.exists(paste0(country,'_',survey_year,'_cluster_info.rds'))){
  
  setwd(paste0(data.dir,'/DHS_data'))
  
  raw.geo.dat <- readRDS(file=paste0(country,'_',survey_year,'_GPS.rds'))
  raw.IR.dat <- readRDS(file=paste0(country,'_',survey_year,'_IR.rds'))
  raw.PR.dat <- readRDS(file=paste0(country,'_',survey_year,'_PR.rds'))
  cluster.info <- readRDS(file=paste0(country,'_',survey_year,'_cluster_info.rds'))
  
  
}else{
  ### run for the first time and use stored results later
  
  # Find DHS surveys ----------------------------------------------------------
  
  #get country ID
  countryId <- DHS.country.meta[DHS.country.meta$ISO3_CountryCode==toupper(gadm.abbrev),]
  
  
  potential_surveys <-  DHS.dataset.meta %>% dplyr::filter(CountryName==country &
                                                             SurveyYear == survey_year &
                                                             ((FileType == 'Individual Recode' &
                                                                 FileFormat=='Stata dataset (.dta)') |
                                                                (FileType == 'Geographic Data')|
                                                                (FileType == 'Household Member Recode' &
                                                                   FileFormat=='Stata dataset (.dta)')))
  
  #only keep surveys with both IR recode and a geographic dataset
  # dhs_survey_ids <- as.numeric(unique(potential_surveys$SurveyNum)[sapply(unique(potential_surveys$SurveyNum),
  #                                                                         function(num){
  #                                                                           if(sum(c("Individual Recode","Geographic Data","Household Member Recode") %in% (potential_surveys %>% filter(SurveyNum==num))$FileType) ==3){return(T)
  #                                                                           }else(return(F))})])
  # 
  # surveys <- potential_surveys %>% filter(SurveyNum %in% dhs_survey_ids) %>% 
  #   group_by(SurveyYear) %>% arrange(SurveyYear,DatasetType)
  # 
  
  # CHECK THAT SURVEYS FOR CORRECT COUNTRY HAVE BEEN CHOSEN
  #unique(surveys$CountryName)
  surveys <- potential_surveys
  
  data.paths.tmp <- rdhs::get_datasets(surveys[surveys$SurveyYear==survey_year,]$FileName, clear_cache = T)
  
  raw.IR.dat <- readRDS(paste0(data.paths.tmp[which(grepl('Individual', surveys$FileType, ignore.case = TRUE))]))
  raw.PR.dat <- readRDS(paste0(data.paths.tmp[which(grepl('Household', surveys$FileType, ignore.case = TRUE))]))
  
  setwd(paste0(data.dir,'/DHS_data'))
  
  saveRDS(raw.IR.dat,file=paste0(country,'_',survey_year,'_IR.rds'))
  saveRDS(raw.PR.dat,file=paste0(country,'_',survey_year,'_PR.rds'))
  
  ### for surveys with GPS, download geographical info
  if(length(data.paths.tmp[which(grepl('Geographic', surveys$FileType, ignore.case = TRUE))])>0){
    raw.geo.dat <- readRDS(paste0(data.paths.tmp[which(grepl('Geographic', surveys$FileType, ignore.case = TRUE))]))
    
    ### prepare cluster info
    cluster.info <- surveyPrev::clusterInfo(geo = raw.geo.dat,
                                            poly.adm1 = country_shp_analysis[[paste0('Admin-',GADM.admin1.level)]],
                                            by.adm1=paste0("NAME_",GADM.admin1.level), ### very important to check whether levels in GAMD is consistent with DHS admin levels
                                            poly.adm2 = country_shp_analysis[[paste0('Admin-',GADM.admin2.level)]],
                                            by.adm2=paste0("NAME_",GADM.admin2.level))
    
    
    setwd(paste0(data.dir,'/DHS_data'))
    
    saveRDS(raw.geo.dat,file=paste0(country,'_',survey_year,'_GPS.rds'))
    saveRDS(cluster.info,file=paste0(country,'_',survey_year,'_cluster_info.rds'))
  }
  
}



##############################################################################
#########   prepare maternal education data
##############################################################################

### individual level
setwd(paste0(data.dir,'/prepared_dat'))

if(file.exists(paste0('educ_dat_ind_',survey_year,'.rds'))){
  
  educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))
  
}else{
  
  educ_dat_ind <- prepare_educ_yrs(PR.recode.raw=raw.PR.dat,
                                   IR.recode.raw=raw.IR.dat)
  
  educ_dat_ind <- merge_adm_info(data = educ_dat_ind,
                                 adm1.info = admin1_info$data,
                                 adm2.info = admin2_info$data,
                                 cluster.info = cluster.info)
  
  message(paste0('Number of individuals missing years of education: ',sum(is.na(educ_dat_ind$educ.yrs)),'\n',
                 'Number of individuals missing in-school status: ',sum(is.na(educ_dat_ind$in.school))))
  
  
  ### only complete information 
  educ_dat_ind <- educ_dat_ind %>% filter(!is.na(educ.yrs)) %>%
    filter(!is.na(in.school)) 
  
  ### drop birth year with very few individuals 
  min_birth_year <- min(educ_dat_ind$birth.year)
  max_birth_year <- max(educ_dat_ind$birth.year)
  
  if(sum(educ_dat_ind$birth.year==min_birth_year)<50){
    educ_dat_ind <- educ_dat_ind %>% filter(birth.year>min_birth_year)
  }
  
  if(sum(educ_dat_ind$birth.year==max_birth_year)<50){
    educ_dat_ind <- educ_dat_ind %>% filter(birth.year<max_birth_year)
  }
  
  
  
  ### country specific stratification variable (typically no need to change, use v022)
  if(country=='Tanzania' & survey_year %in% c(2015,2022)){
    educ_dat_ind$v022 <- paste0(educ_dat_ind$admin1.name,':',educ_dat_ind$urban)
  }
  
  if(country=='Tanzania' & survey_year %in% c(2010)){
    educ_dat_ind$v022 <- educ_dat_ind$v024
    educ_dat_ind <- educ_dat_ind %>% filter(educ.yrs<90) %>%
      mutate(educ.yrs= case_when(educ.yrs>16~16,
                                 TRUE~educ.yrs))
    
  }
  
  
  ## country correction
  if(country=='Tanzania' & survey_year==2022){
    educ_dat_ind <- educ_dat_ind %>%
      mutate(educ.yrs = case_when(v106>1~educ.yrs-1,
                                  TRUE~educ.yrs))


  }
  
  
  setwd(paste0(data.dir,'/prepared_dat'))
  saveRDS(educ_dat_ind, file = paste0('educ_dat_ind_',survey_year,'.rds'))
  
}


### sanity checks 

# message(paste0('Number of individuals missing years of education: ',sum(is.na(educ_dat_ind$educ.yrs)),'\n',
#                'Number of individuals missing in-school status: ',sum(is.na(educ_dat_ind$in.school))))
# 
# 
# educ_ind_inschool = educ_dat_ind %>% filter(in.school==1) %>%
#   select(caseid,v106,v107,v133,hv106,hv107,hv108,hv121,hv122,hv123,hv124,educ.yrs)
# 
# educ_ind_not_inschool = educ_dat_ind %>% filter(in.school==0) %>%
#   select(caseid,v106,v107,v133,hv106,hv107,hv108,hv121,hv122,hv123,hv124,educ.yrs)
# 
# table(educ_ind_inschool$v133-educ_ind_inschool$hv108)
# table(educ_ind_inschool$v133-educ_ind_inschool$educ.yrs)
# 
# table(educ_ind_inschool$educ.yrs-educ_ind_inschool$hv108)
# 
# table(educ_ind_inschool$hv108-educ_ind_inschool$hv124)
# educ_ind_inschool$current_diff <- educ_ind_inschool$hv108-educ_ind_inschool$hv124
# table(educ_ind_inschool$current_diff,educ_ind_inschool$v133)
