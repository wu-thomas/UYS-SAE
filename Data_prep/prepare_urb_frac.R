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
library(sf)

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
#########   load polygon files
##############################################################################

setwd(paste0(data.dir,'/shapeFiles_gadm'))


country_shp_analysis <- readRDS('country_shp_analysis.rds')
country_shp_smoothed <- readRDS('country_shp_smoothed.rds')
admin1_info <- readRDS('admin1_info.rds')
admin2_info <- readRDS('admin2_info.rds')
poly.adm1.sf <- readRDS('poly_adm1_sf.rds')
poly.adm2.sf <- readRDS('poly_adm2_sf.rds')

##############################################################################
#########   create population file folders
##############################################################################

setwd(paste0(data.dir))

dir.create(file.path('.', 'UR'))

dir.create(file.path('.', 'UR/Classification'))

dir.create(file.path('.', 'UR/Fractions'))


################################################################
#########   prepare national grid for easier integration
################################################################

### prepare reference link between admin-1 and admin-2
adm_link <- admin2_info$data %>% select(-c('urban','population'))
adm_link <- adm_link[order(adm_link$admin2.num),]

setwd(paste0(data.dir,'/prepared_dat'))

if(file.exists('natl_grid_df.rds')){
  
  natl_grid_df <- readRDS('natl_grid_df.rds')
  
}else{
  
  ### prepare national grid
  coords <- as.data.frame(terra::xyFromCell(worldpop, 1:terra::ncell(worldpop)))
  colnames(coords) <- c("LONGNUM", "LATNUM")
  
  natl_grid_sf <- st_as_sf(coords[c('LONGNUM','LATNUM')], coords = c("LONGNUM", "LATNUM"), crs = st_crs(worldpop))
  #poly.adm2.sf <- st_transform(poly.adm2.sf, st_crs(worldpop))
  
  ### merge admin info
  adm2_match <- st_join(natl_grid_sf,poly.adm2.sf, join = st_intersects)
  
  natl_grid_df <- data.frame(
    x = coords$LONGNUM,
    y = coords$LATNUM,
    admin2.num = adm2_match$admin2.num
  )
  
  natl_grid_df <- natl_grid_df %>% left_join(adm_link,by='admin2.num')
  
  
  setwd(paste0(data.dir,'/prepared_dat'))
  
  saveRDS(natl_grid_df,'natl_grid_df.rds')
  
  
}

##############################################################################
#########   load classifications
##############################################################################

setwd(paste0(data.dir,'/UR/Classification',sep=''))

#urb.ras <- terra::rast('Madagascar_pred_surf.tif')
urb.ras <- terra::rast(paste0(country,'_ind_surf.tif'))

# terra::plot(urb.ras)

##############################################################################
#########   load total population surface
##############################################################################

setwd(paste0(data.dir,'/worldpop'))

# UNadjusted population counts
worldpop <- terra::rast(paste0(country.abbrev,
                          '_ppp_',survey_year,
                          '_1km_Aggregated_UNadj.tif',sep=''))

# pop_tmp <- terra::values(worldpop)
# urb_tmp <- terra::values(urb.ras)
# sum(pop_tmp*urb_tmp,na.rm=T)/(sum(pop_tmp,na.rm=T))

# pop_f_15_49_tmp <- terra::values(pop_f_15_49)
# sum(pop_f_15_49_tmp*urb_tmp,na.rm=T)/(sum(pop_f_15_49_tmp,na.rm=T))

##############################################################################
######### Function to calculate subnational urban fractions
##############################################################################
#' Function to calculate subnational urban fractions
#' 
#' @param natl.grid A national grid
#' must contain a column indicating completeness
#' @examples
#' 

# 
# natl.grid <- natl_grid_df
# pop.ras <- worldpop
# urb.ras <- urb.ras
# admin.var <- 'admin1.num'
# admin.tab <- admin1_info$data[,c('admin1.num','admin1.char','admin1.name')]

get.urb.frac <- function(natl.grid,
                         pop.ras,
                         urb.ras,
                         admin.var,
                         admin.tab){
  
  
  # assign population density
  pop_extracted  <- terra::extract(pop.ras, natl.grid[c('x','y')],ID=F)
  natl.grid$pop <- as.vector(pop_extracted[,1])
  
  # assign urban rural predicted probability
  urb_prob_extracted  <- terra::extract(urb.ras, natl.grid[c('x','y')],ID=F)
  natl.grid$urban_prob <- as.vector(urb_prob_extracted[,1])
  
  # exclude pixels without classification
  natl.grid[is.na(natl.grid$urban_prob),]$pop <- NA    # omit pixels without classification
  
  # calculate national fraction
  # if(is.null(admin.poly)){
  #   natl.frac <- sum(natl.grid$urban_prob*natl.grid$pop,na.rm = TRUE)/
  #     sum(natl.grid$pop,na.rm=TRUE)
  #   return(natl.frac)  
  # }
  
  # calculate fractions
  adm.urb.frac <- natl.grid %>%
    group_by(!!sym(admin.var)) %>%
    summarise(Frac = sum(pop*urban_prob, na.rm = TRUE)/sum(pop,na.rm = TRUE))
  
  adm.urb.frac <- adm.urb.frac[!is.na(adm.urb.frac[[admin.var]]),]
  
  # assign admin names
  adm.urb.frac <- admin.tab %>% left_join(adm.urb.frac,by=admin.var)
  
  return(adm.urb.frac)
}


# ex_frac <- get.urb.frac(natl.grid = natl_grid_df,
#                         pop.ras = worldpop,
#                         urb.ras = urb.ras,
#                         admin.var ='admin1.num',
#                         admin.tab = admin1_info$data[,c('admin1.num','admin1.char','admin1.name')]
# )


##############################################################################
#########  calculate subnational urban fractions
##############################################################################

years <- c(beg.year:end.year)
adm1.weight.frame <- data.frame()
adm2.weight.frame <- data.frame()

age_group <- c(3:9)*5

setwd(paste0(data.dir,'/UR/Fractions'))

if(file.exists('agegrp_admin1_urban_weights.rds')&
   file.exists('agegrp_admin2_urban_weights.rds')){
  
  setwd(paste0(data.dir,'/UR/Fractions'))
  
  adm1.weight.frame <- readRDS(paste0('agegrp_admin1_urban_weights.rds'))
  adm2.weight.frame <- readRDS(paste0('agegrp_admin2_urban_weights.rds'))
  
}else{
  
  for ( t in 1:length(years)){
    print(t)
    
    for(age_int in 1:7){
      print(age_int)
      year <- years[t]
      
      # load U5 population at year t
      setwd(paste0(data.dir,'/worldpop/pop_1km'))
      
      age <- age_group[age_int]
      
      sex<- 'f'
      f_age_pop<- terra::rast(paste0(country.abbrev,'_', sex, '_',age, '_',year,'_1km.tif'))
      
      # admin1 urban fraction for U5 population at year t
      f_age_urb_admin1<-get.urb.frac(natl.grid = natl_grid_df,
                                     pop.ras = f_age_pop,
                                     urb.ras = urb.ras,
                                     admin.var ='admin1.num',
                                     admin.tab =  admin1_info$data %>% select(-c('urban','population','surveyWeight'))
      )
      
      f_age_urb_admin1$year <- year
      f_age_urb_admin1$agegrp <- paste0(age,'-',age+4,sep='')
      f_age_urb_admin1$agegrp.int <- age_int
      
      adm1.weight.frame <- rbind(adm1.weight.frame,f_age_urb_admin1)
      
      # admin2 urban fraction for U5 population at year t
      f_age_urb_admin2 <- get.urb.frac(natl.grid = natl_grid_df,
                                       pop.ras = f_age_pop,
                                       urb.ras = urb.ras,
                                       admin.var ='admin2.num',
                                       admin.tab =  admin2_info$data %>% select(-c('urban','population'))
      )
      
      f_age_urb_admin2$year <- year
      f_age_urb_admin2$agegrp <- paste0(age,'-',age+4,sep='')
      f_age_urb_admin2$agegrp.int <- age_int
      adm2.weight.frame <- rbind(adm2.weight.frame,f_age_urb_admin2)
    }
    #setwd(res_dir)
    
  }
  
  
  # process admin 1 urban rural weights data frame
  adm1.weight.frame <- adm1.weight.frame %>% 
    mutate(Frac=case_when(is.na(Frac) ~ 0,
                          TRUE~Frac))
  adm1.weight.frame <- adm1.weight.frame %>% rename('urban_frac'='Frac')
  
  adm1.weight.frame$rural_frac <- 1 - adm1.weight.frame$urban_frac
  
  # process admin 2 urban rural weights data frame
  
  adm2.weight.frame <- adm2.weight.frame %>% 
    mutate(Frac=case_when(is.na(Frac) ~ 0,
                          TRUE~Frac))
  adm2.weight.frame <- adm2.weight.frame %>% rename('urban_frac'='Frac')
  
  adm2.weight.frame$rural_frac <- 1 - adm2.weight.frame$urban_frac
  
  
  
  
  # save weights frames
  
  setwd(paste0(data.dir,'/UR/Fractions'))
  
  saveRDS(adm1.weight.frame,paste0('agegrp_admin1_urban_weights.rds'))
  saveRDS(adm2.weight.frame,paste0('agegrp_admin2_urban_weights.rds'))
  
}

#adm1.weight.frame$year <- adm1.weight.frame$years
#adm2.weight.frame$year <- adm2.weight.frame$years
#colnames(adm1.weight.frame)  <- c('region','year','urban_frac','agegrp','agegrp.int','rural_frac')
#colnames(adm2.weight.frame)  <- c('region','year','urban_frac','agegrp','agegrp.int','rural_frac')