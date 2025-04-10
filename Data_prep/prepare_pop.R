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
#########   process worldpop data for the year of census
##############################################################################

options(timeout = 30000) # adjust this time, should be longer than each download

setwd(paste0(data.dir,'/worldpop'))

### download population
pop.abbrev <- tolower(gadm.abbrev)
pop_file <- paste0(pop.abbrev,'_ppp_',survey_year+survey_year_span,'_1km_Aggregated_UNadj.tif')


if(!file.exists(pop_file)){
  
  url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", 
                min(survey_year+survey_year_span,2020), "/", toupper(pop.abbrev),"/",      
                pop.abbrev,'_ppp_',min(survey_year+survey_year_span,2020),
                '_1km_Aggregated_UNadj.tif')
  
  download.file(url, pop_file, method = "libcurl",mode="wb")
}

### load population
worldpop <- terra::rast(paste0(country.abbrev, '_ppp_', survey_year+survey_year_span, '_1km_Aggregated_UNadj.tif'))

##############################################################################
#########   download age group specific population
##############################################################################

setwd(paste0(data.dir))

dir.create(file.path('.', 'worldpop'))
dir.create(file.path('worldpop', 'pop_1km'))
dir.create(file.path('worldpop', 'pop_frac'))


setwd(paste0(data.dir,'/worldpop'))

pop.year <- beg.year:end.year
pop.abbrev <- tolower(gadm.abbrev)

options(timeout = 30000) # adjust this time, should be longer than each download
rigorousFileTest = T # set to TRUE after files have been downloaded to test 
# if files were downloaded correctly, i.e. if they can be loaded into R
sex<- 'f'

for(year in pop.year){
  
  print(year)
  
  for(age in c(3:9)*5){
    
    setwd(paste0(data.dir,'/worldpop/pop_1km'))
    
    year.rep <- min(year,2020)
    
    preload.file <- paste0(pop.abbrev,'_', sex, '_', age, '_', year.rep,'_1km.tif')
    new.file.name = paste0(pop.abbrev,'_', sex, '_', age, '_', year,'_1km.tif') 
    ### check whether predownloaded 
    pre_load_dir <- paste0('E:/worldpop/',year.rep,'/',toupper(pop.abbrev),'/')
    
    #if(file.exists(paste0(pre_load_dir,file))){
    
    file.copy(
      from = paste0(pre_load_dir, preload.file), 
      to = paste0(data.dir, '/worldpop/pop_1km/',new.file.name), 
      overwrite = TRUE
    )
    
    # if(year.rep < 2021){
    #   
    #   file.copy(from = paste0(pre_load_dir,file), to = paste0(data.dir,'/worldpop/pop_1km'), overwrite = TRUE) 
    #   
    # }else{
    #   
    #   # average 2020 and year to be better representative, 2020 onwards are crude
    #   
    #   file.year.rep =  terra::rast(paste0(pre_load_dir,file))
    #   file.2020 =  terra::rast(paste0('E:/worldpop/',2020,'/',toupper(pop.abbrev),'/',
    #                                   pop.abbrev,'_', sex, '_', age, '_', 2020,'_1km.tif'))
    #   
    #   file.avg = (file.year.rep+file.2020)/2
    #   
    #   terra::writeRaster(file.avg, 
    #                      paste0(data.dir,'/worldpop/pop_1km/',pop.abbrev,'_', sex, '_', age, '_', year.rep,'_1km.tif'),
    #                      overwrite=TRUE)
    #   
    #   
    # }
    
    
    
    
    #}else{
    
    ### if not predownloaded, download directly
    # if(FALSE){
    #   # check if the raster file exists. If rigorousFileTest == TRUE, also check
    #   # if the file can be successfully loaded
    # 
    #   goodFile = file.exists(file)
    #   if(goodFile && rigorousFileTest) {
    #     goodFile = tryCatch(
    #       {
    #         test = terra::rast(file)
    #         TRUE
    #       },
    #       error = function(e) {FALSE}
    #     )
    #   }
    # 
    #   if(!goodFile){
    #     url <- paste0("ftp://ftp.worldpop.org/GIS/AgeSex_structures/Global_2000_2020_1km/unconstrained/",
    #                   min(year,2020), "/", toupper(pop.abbrev), "/", pop.abbrev, "_",
    #                   sex, "_", age, "_",  min(year,2020), "_1km.tif")
    #     download.file(url, file, method = "libcurl",mode="wb")
    #   }
    # }
    # }
    
    
    
  }
}

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

# library(ggplot2)
# library(ggspatial)
# ggplot(data = poly.adm2.sf[23,]) +
#   annotation_map_tile(type = "osm") +
#   geom_sf() +
#   theme_void()


################################################################
#########   prepare function and dictionary (each row is one admin2 region)
################################################################

#pop_ras <- worldpop

#adm.shp <- country_shp_analysis[[paste0('Admin-',GADM.admin1.level)]]
# 
# natl.grid <- natl_grid_df
# 
# pop_ras <- worldpop
# admin_pop_dat <- adm_link

get_adm2_pop<-function(natl.grid, pop_ras, admin_pop_dat){
  
  pop_extracted  <- terra::extract(pop_ras, natl.grid[c('x','y')],ID=F)
  natl.grid$pop <- as.vector(pop_extracted[,1])
  
  adm.pop <- natl.grid %>%
    group_by(admin2.num) %>%
    summarise(adm2_pop = sum(pop,na.rm = TRUE))
  
  adm.pop <- adm.pop[!is.na(adm.pop$admin2.num),]
  
  admin_pop_dat <- admin_pop_dat %>% left_join(adm.pop,by='admin2.num')
  
}




# 
# tmp_adm2_pop <- get_adm_pop(adm.shp = country_shp_analysis[[paste0('Admin-',GADM.admin2.level)]], 
#                             pop_ras=worldpop,
#                             admin_pop_dat = adm_link)

################################################################
#########  get population fractions for total fertility rate
################################################################

#pop.year <- c(beg.year:end.year)

setwd(paste0(data.dir,'/worldpop/pop_frac/'))

if(file.exists(paste0('admin2_tf_pop_frac_', pop.year[1],'_',
                      pop.year[length(pop.year)],
                      '.rds', sep = ''))){
  
  adm2_pop_list <- readRDS(paste0('admin2_tf_pop_frac_', pop.year[1],'_',
                                  pop.year[length(pop.year)],
                                  '.rds', sep = ''))
}else{
  
  
  adm2_pop_list <- list()
  
  # read worldpop rasters, summarize and save admin-2 population
  for(year in pop.year){
    
    print(year)
    year.rep <- year # min(year,2020)
    
    f_15_name = paste0(pop.abbrev,'_',sex, '_', 15, '_',year.rep,'_1km.tif')
    f_20_name = paste0(pop.abbrev,'_',sex, '_', 20, '_',year.rep,'_1km.tif')
    f_25_name = paste0(pop.abbrev,'_',sex, '_', 25, '_',year.rep,'_1km.tif')
    f_30_name = paste0(pop.abbrev,'_',sex, '_', 30, '_',year.rep,'_1km.tif')
    f_35_name = paste0(pop.abbrev,'_',sex, '_', 35, '_',year.rep,'_1km.tif')
    f_40_name = paste0(pop.abbrev,'_',sex, '_', 40, '_',year.rep,'_1km.tif')
    f_45_name = paste0(pop.abbrev,'_',sex, '_', 45, '_',year.rep,'_1km.tif')
    
    setwd(paste0(data.dir,'/worldpop/pop_1km'))
    
    pop_f_15<- terra::rast(f_15_name)
    pop_f_20<- terra::rast(f_20_name)
    pop_f_25<- terra::rast(f_25_name)
    pop_f_30<- terra::rast(f_30_name)
    pop_f_35<- terra::rast(f_35_name)
    pop_f_40<- terra::rast(f_40_name)
    pop_f_45<- terra::rast(f_45_name)
    
    pop_f_15_49<- pop_f_15+pop_f_20+
      pop_f_25+pop_f_30+
      pop_f_35+pop_f_40+
      pop_f_45
    
    # admin 2 population fraction 
    adm2_pop<-get_adm2_pop(natl.grid=natl_grid_df,
                           pop_ras=pop_f_15_49,
                           admin_pop_dat=adm_link)
    
    # admin 1 population 
    adm2_pop<-adm2_pop %>% 
      group_by(admin1.char) %>% 
      mutate(adm1_pop = sum(adm2_pop))
    
    
    # fraction of admin2 w.r.t. admin1
    adm2_pop$adm2_frac<-adm2_pop$adm2_pop/
      adm2_pop$adm1_pop
    
    setwd(paste0(data.dir,'/worldpop/pop_frac/'))
    
    adm2_pop_list[[as.character(year)]] <- adm2_pop
    
    # sanity check, fraction for admin2 in each admin1 sum up to 1
    #print(aggregate(adm2_frac~admin1.char, data = adm2_pop, FUN = sum))    
    
    
  }
  setwd(paste0(data.dir,'/worldpop/pop_frac/'))
  
  saveRDS(adm2_pop_list, file = paste0('admin2_tf_pop_frac_', pop.year[1],'_',
                                       pop.year[length(pop.year)],
                                       '.rds', sep = ''))
}


# summarize
# load admin-2 population weights (with respect to each admin-1)

setwd(paste0(data.dir,'/worldpop/pop_frac/'))

if(file.exists(paste0('admin1_tf_pop_frac_frame.rds', sep = ''))&
   file.exists(paste0('admin2_tf_pop_frac_frame.rds', sep = ''))){
  
  adm1.pop.frame <- readRDS(file= paste0('admin1_tf_pop_frac_frame.rds', sep = ''))  
  adm2.pop.frame <- readRDS(file= paste0('admin2_tf_pop_frac_frame.rds', sep = '')) 
  
}else{
  
  
  adm1.pop.frame <- data.frame()
  adm2.pop.frame <- data.frame()
  
  for (i in 1:length(pop.year)){
    
    year <- pop.year[i]
    #setwd(data_dir)
    
    #load(paste(country,'/','worldpop/pop_frac/', 'admin2_tf_pop_frac_', year, '.rda', sep = ''))  
    
    adm2.pop <- adm2_pop_list[[paste0(year)]]
    ### admin-1 level population 
    
    adm1.pop<-adm2.pop[!duplicated(adm2.pop[,c('admin1.char')]),]
    
    adm1.pop = adm1.pop[sort(adm1.pop$admin1.num), ]
    
    adm1.pop<-adm1.pop[,c('admin1.name','admin1.char','admin1.num','adm1_pop')]
    
    adm1.pop$year <- year
    adm1.pop.frame <- rbind (adm1.pop.frame,adm1.pop)
    
    adm2.pop$year <- year
    adm2.pop.frame <- rbind(adm2.pop.frame,adm2.pop)
    
  }
  
  
  
  setwd(paste0(data.dir,'/worldpop/pop_frac/'))
  
  saveRDS(adm1.pop.frame,file= paste0('admin1_tf_pop_frac_frame.rds', sep = ''))  
  saveRDS(adm2.pop.frame,file= paste0('admin2_tf_pop_frac_frame.rds', sep = ''))  
}


# terra::plot(worldpop)
# 
# tmp_adm2 <- country_shp_analysis[['Admin-3']]
# 
# plot(tmp_adm2[23,], add=TRUE, col=NA, border="red", lwd=2)


################################################################
#########  get population fractions for age specific fertility rate
################################################################

#pop.year <- c(beg.year:end.year)

# read worldpop rasters, summarize and save admin-2 population
for(age in c(3:9)*5){
  
  adm2_age_pop_list <- list()
  
  setwd(paste0(data.dir,'/worldpop/pop_frac/'))
  
  
  if(file.exists(paste0('admin2_f_',age,'_pop_frac_', pop.year[1],'_',
                        pop.year[length(pop.year)],
                        '.rds', sep = ''))){
    
    
  }else{
    
    
    
    for(year in pop.year){
      
      print(year)
      
      year.rep <- min(year,2020)
      
      f_name =  paste0(pop.abbrev,'_', sex, '_', age, '_', year.rep,'_1km.tif')
      
      setwd(paste0(data.dir,'/worldpop/pop_1km'))
      
      pop_f<- terra::rast(f_name)
      
      
      #proj4string(pop_f) <- proj4string(poly.adm1)
      
      
      # admin 2 population fraction 
      adm2_pop<- get_adm2_pop(natl.grid=natl_grid_df,
                              pop_ras = pop_f,
                              admin_pop_dat = adm_link)
      # admin 1 population 
      adm2_pop<-adm2_pop %>% 
        group_by(admin1.char) %>% 
        mutate(adm1_pop = sum(adm2_pop))
      
      # fraction of admin2 w.r.t. admin1
      adm2_pop$adm2_frac<-adm2_pop$adm2_pop/
        adm2_pop$adm1_pop
      
      adm2_age_pop_list[[as.character(year)]] <- adm2_pop
      
      # sanity check, fraction for admin2 in each admin1 sum up to 1
      #print(aggregate(adm2_frac~admin1.char, data = adm2_pop, FUN = sum))    
      
      
    }
    setwd(paste0(data.dir,'/worldpop/pop_frac/'))
    
    saveRDS(adm2_age_pop_list, file = paste0('admin2_f_',age,'_pop_frac_', pop.year[1],'_',
                                             pop.year[length(pop.year)],
                                             '.rds', sep = ''))
  }
}



# summarize
# load admin-2 population weights (with respect to each admin-1)
setwd(paste0(data.dir,'/worldpop/pop_frac/'))


if(file.exists(paste0('admin1_f_asfr_pop_frac_', pop.year[1],'_',
                      pop.year[length(pop.year)],
                      '.rds', sep = ''))&
   file.exists(paste0('admin2_f_asfr_pop_frac_', pop.year[1],'_',
                      pop.year[length(pop.year)],
                      '.rds', sep = ''))){
  
  
  setwd(paste0(data.dir,'/worldpop/pop_frac/'))
  
  adm1.pop.frame <- readRDS(file = paste0('admin1_f_asfr_pop_frac_', pop.year[1],'_',
                                          pop.year[length(pop.year)],
                                          '.rds', sep = ''))
  
  adm2.pop.frame <- readRDS(file = paste0('admin2_f_asfr_pop_frac_', pop.year[1],'_',
                                          pop.year[length(pop.year)],
                                          '.rds', sep = ''))
  
  
}else{
  adm1.pop.frame <- data.frame()
  adm2.pop.frame <- data.frame()
  
  age_group <- c(3:9)*5
  
  for(age_int in 1:7){
    
    age <- age_group[age_int]
    #adm1.agegrp.pop.frame <- data.frame()
    
    setwd(paste0(data.dir,'/worldpop/pop_frac/'))
    
    adm2_age_pop_list <- readRDS(paste0('admin2_f_',age,'_pop_frac_', pop.year[1],'_',
                                        pop.year[length(pop.year)],
                                        '.rds', sep = ''))
    
    for (i in 1:length(pop.year)){
      
      year <- pop.year[i]
      
      adm2.pop <- adm2_age_pop_list[[paste0(year)]]
      adm2.pop$year <- year
      adm2.pop$agegrp <- paste0(age,'-',age+4)
      adm2.pop$agegrp.int <- age_int
      adm2.pop.frame <- rbind(adm2.pop.frame,adm2.pop)
      
      
      ### admin-1 level population 
      adm1.pop<-adm2.pop[!duplicated(adm2.pop[,c('admin1.char')]),]
      # create an ordered admin1 list
      adm1.pop = adm1.pop[sort(adm1.pop$admin1.num), ]
      
      adm1.pop<-adm1.pop[,c('admin1.name','admin1.char','admin1.num','adm1_pop')]
      adm1.pop$year <- year
      adm1.pop$agegrp <- paste0(age,'-',age+4)
      adm1.pop$agegrp.int <- age_int
      adm1.pop$admin1_frac <- adm1.pop$adm1_pop/sum(adm1.pop$adm1_pop)
      
      adm1.pop.frame <- rbind (adm1.pop.frame,adm1.pop)
      
      
    }
  }
  
  
  setwd(paste0(data.dir,'/worldpop/pop_frac/'))
  
  saveRDS(adm1.pop.frame, file = paste0('admin1_f_asfr_pop_frac_', pop.year[1],'_',
                                        pop.year[length(pop.year)],
                                        '.rds', sep = ''))
  
  saveRDS(adm2.pop.frame, file = paste0('admin2_f_asfr_pop_frac_', pop.year[1],'_',
                                        pop.year[length(pop.year)],
                                        '.rds', sep = ''))
  
}