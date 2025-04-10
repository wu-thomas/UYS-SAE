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
#library(future.apply)


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

if(!dir.exists(paths = paste0('INLA'))){ 
  dir.create(file.path('INLA'))
}

if(!dir.exists(paths = paste0('INLA/National'))){ 
  dir.create(file.path('INLA/National'))
}

if(!dir.exists(paths = paste0('INLA/Admin1'))){ 
  dir.create(file.path('INLA/Admin1'))
}

if(!dir.exists(paths = paste0('INLA/Admin2'))){ 
  dir.create(file.path('INLA/Admin2'))
}


################################################################
#########   load aggregation functions
################################################################


setwd(paste0(code.dir))
source('helper_functions/DataProcessing_helper.R')
source('helper_functions/helper_educ_INLA.R')


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

adm1.names = admin1_info$data[,c('admin1.name','admin1.char','admin1.num')]
adm1.names$region.int <- adm1.names$admin1.num
################################################################
######### load data
################################################################

### individual level
setwd(paste0(data.dir,'/prepared_dat'))
educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))


###########################################################################
#########  prepare fractions for aggregation (Admin-1)
###########################################################################

### whether use survey or worldpop for aggregation weights 
use.survey.frac = T
analysis.adm.level = 1
adm.num.col <- paste0("admin", analysis.adm.level, ".num")
adm.char.col <- paste0("admin", analysis.adm.level, ".char")
adm.name.col <- paste0("admin", analysis.adm.level, ".name")
adm.pop.col <- paste0("adm", analysis.adm.level, "_pop")

if(use.survey.frac ==T){
  
  pop_summary <- educ_dat_ind %>%
    mutate(urban_ind = as.numeric(urban=='urban')) %>%
    group_by(admin1.num,agegrp) %>%
    summarize(urban_pop = sum(weights*urban_ind,na.rm=T),
              rural_pop = sum(weights*(1-urban_ind),na.rm=T),
              .groups = "drop") %>%
    mutate(urban_frac = urban_pop/(urban_pop+rural_pop),
           rural_frac = rural_pop/(urban_pop+rural_pop))%>%
    ungroup %>%
    mutate(region.int = admin1.num)
  
  svy.year.UR.frac <- pop_summary %>%
    select(admin1.num,agegrp,urban_frac,rural_frac,region.int)
  
  
  svy.year.pop.U <- pop_summary %>%
    mutate(pop = urban_pop)%>%
    select(admin1.num,agegrp,urban_frac,pop,region.int)
  
  svy.year.pop.R <- pop_summary %>%
    mutate(pop = rural_pop)%>%
    select(admin1.num,agegrp,urban_frac,pop,region.int)
  
  svy.year.pop.overall <- pop_summary %>%
    mutate(pop = rural_pop+urban_pop)%>%
    select(admin1.num,agegrp,urban_frac,pop,region.int)
}else{
  
  pop.year <- c(beg.year:end.year)
  
  ### prepare UR fraction for the survey year
  setwd(paste0(data.dir,'/UR/Fractions'))
  
  adm.weight.frame <- readRDS(paste0('agegrp_admin',analysis.adm.level,'_urban_weights.rds'))
  svy.year.UR.frac <- adm.weight.frame[adm.weight.frame$year==min(2020,survey_year+survey_year_span),]
  
  svy.year.UR.frac <- svy.year.UR.frac[order(svy.year.UR.frac[[paste0('admin',analysis.adm.level,'.num')]],
                                             svy.year.UR.frac$agegrp),]
  
  svy.year.UR.frac$region.int <- svy.year.UR.frac[[paste0("admin",analysis.adm.level,".num")]]
  
  
  ### prepare aggregation weights from admin to national
  setwd(paste0(data.dir,'/worldpop/pop_frac'))
  
  adm.pop.frame <- readRDS(file = paste0('admin',analysis.adm.level,'_f_asfr_pop_frac_', pop.year[1],'_',
                                         pop.year[length(pop.year)],
                                         '.rds', sep = ''))
  
  svy.year.pop.overall <- adm.pop.frame[adm.pop.frame$year == min(2020,survey_year + survey_year_span), ]
  svy.year.pop.overall$pop <- svy.year.pop.overall[[adm.pop.col]]
  svy.year.pop.overall <- svy.year.pop.overall[order(svy.year.pop.overall[[adm.num.col]],
                                                     svy.year.pop.overall$agegrp), ]
  svy.year.pop.overall$region.int <- svy.year.pop.overall[[paste0("admin",analysis.adm.level,".num")]]
  
  
  # Separate urban and rural populations
  svy.year.pop.U <- svy.year.pop.overall[, c(adm.num.col, adm.char.col, adm.name.col, "agegrp", "agegrp.int", adm.pop.col)]
  svy.year.pop.R <- svy.year.pop.overall[, c(adm.num.col, adm.char.col, adm.name.col, "agegrp", "agegrp.int", adm.pop.col)]
  
  # Join with urban and rural fractions
  svy.year.pop.U <- svy.year.pop.U %>%
    left_join(svy.year.UR.frac[, c(adm.num.col, "agegrp", "urban_frac",'region.int')], by = c(adm.num.col, "agegrp"))
  
  svy.year.pop.R <- svy.year.pop.R %>%
    left_join(svy.year.UR.frac[, c(adm.num.col, "agegrp", "rural_frac",'region.int')], by = c(adm.num.col, "agegrp"))
  
  # Calculate population and drop redundant columns
  svy.year.pop.U <- svy.year.pop.U %>%
    mutate(pop = .data[[adm.pop.col]] * urban_frac) %>%
    select(-all_of(adm.pop.col))
  
  svy.year.pop.R <- svy.year.pop.R %>%
    mutate(pop = .data[[adm.pop.col]] * rural_frac) %>%
    select(-all_of(adm.pop.col))
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
######### set up space x time grid 
################################################################
N.time <- length(unique(educ_dat_ind$birth.year))
N.area <- length(unique(educ_dat_ind$admin1.num))

x <- expand.grid(1:N.time, 1:N.area)
time.area <- data.frame(birth.year.int = x[, 1], region.int = x[, 2], time.area = c(1:nrow(x)))


### prepare template for estimation grid  
comb.grid <- time.area
comb.grid$birth.year <- comb.grid$birth.year.int+min(educ_dat_ind$birth.year)-1
comb.grid$age.start <- (floor((survey_year+survey_year_span-
                                 comb.grid$birth.year)/5))*5
comb.grid$age.start[comb.grid$age.start>45] <- 45 
comb.grid$age.start[comb.grid$age.start<15] <- 15 

comb.grid$agegrp <- paste0(comb.grid$age.start,'-',comb.grid$age.start+4,sep='')


### prepare UR fraction data with space-time index 
ur.frac.grid <- merge(comb.grid,
                      svy.year.UR.frac[,c('agegrp','urban_frac','rural_frac','region.int')],
                      by=c('region.int','agegrp'),all.x=T)

ur.frac.grid <- ur.frac.grid[order(ur.frac.grid$time.area),]
ur.frac.grid$order.idx <- c(1:dim(ur.frac.grid)[1]) ### keep track of order of rows

### prepare overall population data with space-time index 
adm.pop.grid.overall <- merge(comb.grid,
                              svy.year.pop.overall[,c('agegrp','pop','region.int')],
                              by=c('region.int','agegrp'),all.x=T)

adm.pop.grid.overall <- adm.pop.grid.overall[order(adm.pop.grid.overall$time.area),]
adm.pop.grid.overall$order.idx <- c(1:dim(adm.pop.grid.overall)[1]) ### keep track of order of rows


### prepare urban population data with space-time index 
adm.pop.grid.U <- merge(comb.grid,
                        svy.year.pop.U[,c('agegrp','pop','region.int')],
                        by=c('region.int','agegrp'),all.x=T)

adm.pop.grid.U <- adm.pop.grid.U[order(adm.pop.grid.U$time.area),]
adm.pop.grid.U$order.idx <- c(1:dim(adm.pop.grid.U)[1]) ### keep track of order of rows


### prepare rural population data with space-time index 
adm.pop.grid.R <- merge(comb.grid,
                        svy.year.pop.R[,c('agegrp','pop','region.int')],
                        by=c('region.int','agegrp'),all.x=T)

adm.pop.grid.R <- adm.pop.grid.R[order(adm.pop.grid.R$time.area),]
adm.pop.grid.R$order.idx <- c(1:dim(adm.pop.grid.R)[1]) ### keep track of order of rows



################################################################
######### set up model parameters
################################################################


#trend.list <-  c('rw1_main','rw1_iid','rw2_main','rw2_iid')

trend.list <- c('rw1_iid')

### run all models

for (birth.cohort.trend in trend.list){
  
  # birth.cohort.trend = 'rw1_iid'
  rw.order = as.numeric(substring(birth.cohort.trend,3,3))
  main_or_iid = strsplit(birth.cohort.trend, '_')[[1]][2]
  
  if(main_or_iid=='iid'){
    time.trend.type = 'bym'
  }else{
    time.trend.type = 'main'
  }
  
  ################################################################
  ######### INLA, Admin-1, rural
  ################################################################
  
  setwd(paste0(res.dir,'/INLA/Admin1'))
  
  ### rural
  if(file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_R_model.rds'))&
     file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_R_est_draws.rds'))&
     file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_R_res.rds'))){
    
    adm1.fixed.R.fitted <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_R_model.rds'))
    adm1.fixed.R.draws <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_R_est_draws.rds'))
    adm1.fixed.R.res <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_R_res.rds'))
    
  }else{
    educ.adj.admin1.R <- merge_adm_info(data = educ.adj.R,
                                        adm1.info = admin1_info$data,
                                        adm2.info = admin2_info$data,
                                        cluster.info = cluster.info)
    ### admin1 fixed effects, rural
    
    ## adjust Dar es Salaam, for Tanzania
    if(country=='Tanzania'&survey_year==2022){
      pseudo_obs <- educ.adj.admin1.R[1,]
      pseudo_obs$Y= NA
      pseudo_obs$admin1.num =2
      pseudo_obs$admin1.char ='admin1_2'
      pseudo_obs$admin1.name =NA
      educ.adj.admin1.R <- rbind(educ.adj.admin1.R,pseudo_obs)
    }
    
      
    adm1.fixed.R.fitted <- getEduc.INLA(data = educ.adj.admin1.R, 
                                        rw.order = rw.order,
                                        subnational = T,
                                        region.num.var = 'admin1.num',
                                        #Amat = Amat_adm1,
                                        spatial.effect = c('fixed','bym2')[1],
                                        time.trend = time.trend.type, # c('main','bym','bym2')[3]
                                        birth.cohort.specific = T,
                                        proj.years = 0,
                                        adm.names = admin1_info$data[,c('admin1.name','admin1.char','admin1.num')])
    
    saveRDS(adm1.fixed.R.fitted,paste0('adm1_INLA_fixed_RW',rw.order,'_R_model.rds'))
    
    ### posterior draws
    adm1.fixed.R.draws <- getEduc.INLA.postsamps(est.grid= ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                               INLA.mod=adm1.fixed.R.fitted$INLA.mod,
                                               nSamp=1000)
    saveRDS(adm1.fixed.R.draws,paste0('adm1_INLA_fixed_RW',rw.order,'_R_est_draws.rds'))

    ### result summary
    adm1.fixed.R.res <- summarize.Educ.INLA.postsamps(post.draws = adm1.fixed.R.draws,
                                                           est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                                           adm.names = adm1.names,
                                                           subnational =T,
                                                           nSamp=1000,
                                                           start.year = min(educ_dat_ind$birth.year)
    )
    saveRDS(adm1.fixed.R.res,paste0('adm1_INLA_fixed_RW',rw.order,'_R_res.rds'))
    
    
    
  }
  
  ################################################################
  ######### INLA, Admin-1, urban
  ################################################################
  
  setwd(paste0(res.dir,'/INLA/Admin1'))
  
  ### urban
  if(file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_U_model.rds'))&
     file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_U_est_draws.rds'))&
     file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_U_res.rds'))){
    
    adm1.fixed.U.fitted <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_U_model.rds'))
    adm1.fixed.U.draws <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_U_est_draws.rds'))
    adm1.fixed.U.res <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_U_res.rds'))
    
  }else{
    
    educ.adj.admin1.U <- merge_adm_info(data = educ.adj.U,
                                        adm1.info = admin1_info$data,
                                        adm2.info = admin2_info$data,
                                        cluster.info = cluster.info)
    ### admin1 fixed effects, urban
    adm1.fixed.U.fitted <- getEduc.INLA(data = educ.adj.admin1.U, 
                                        rw.order = rw.order,
                                        subnational = T,
                                        region.num.var = 'admin1.num',
                                        #Amat = Amat_adm1,
                                        spatial.effect = c('fixed','bym2')[1],
                                        time.trend = time.trend.type, # c('main','bym','bym2')[3]
                                        birth.cohort.specific = T,
                                        proj.years = 0,
                                        adm.names = admin1_info$data[,c('admin1.name','admin1.char','admin1.num')])
    
    saveRDS(adm1.fixed.U.fitted,paste0('adm1_INLA_fixed_RW',rw.order,'_U_model.rds'))
    
    ### posterior draws
    adm1.fixed.U.draws <- getEduc.INLA.postsamps(est.grid= ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                                 INLA.mod=adm1.fixed.U.fitted$INLA.mod,
                                                 nSamp=1000)
    saveRDS(adm1.fixed.U.draws,paste0('adm1_INLA_fixed_RW',rw.order,'_U_est_draws.rds'))
    
    ### result summary
    adm1.fixed.U.res <- summarize.Educ.INLA.postsamps(post.draws = adm1.fixed.U.draws,
                                                      est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                                      adm.names = adm1.names,
                                                      subnational =T,
                                                      nSamp=1000,
                                                      start.year = min(educ_dat_ind$birth.year)
    )
    saveRDS(adm1.fixed.U.res,paste0('adm1_INLA_fixed_RW',rw.order,'_U_res.rds'))
    
    
  }
  
  ################################################################
  ######### aggregate, Admin-1, overall
  ################################################################
  
  setwd(paste0(res.dir,'/INLA/Admin1'))
  
  ### overall
  if(file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_overall_draws.rds'))&
     file.exists(paste0('adm1_INLA_fixed_RW',rw.order,'_overall_res.rds'))){
    
    adm1.fixed.overall.draws <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_overall_draws.rds'))
    adm1.fixed.overall.res <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_overall_res.rds'))
    
  }else{
    
    adm1.fixed.overall.draws <- aggre.Educ.UR(post.draws.U = adm1.fixed.U.draws,
                                       post.draws.R = adm1.fixed.R.draws,
                                       UR.frac = ur.frac.grid,
                                       nSamp = 1000)
    
    saveRDS(adm1.fixed.overall.draws,paste0('adm1_INLA_fixed_RW',rw.order,'_overall_draws.rds'))
    
    adm1.fixed.overall.res <- summarize.Educ.INLA.postsamps(post.draws = adm1.fixed.overall.draws,
                                                            est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                                            adm.names = adm1.names,
                                                            subnational =T,
                                                            nSamp=1000,
                                                            start.year = min(educ_dat_ind$birth.year)
    )
    saveRDS(adm1.fixed.overall.res,paste0('adm1_INLA_fixed_RW',rw.order,'_overall_res.rds'))
    
  }
  
  ################################################################
  ######### aggregate, national
  ################################################################
  

  ### aggregated overall national
  setwd(paste0(res.dir,'/INLA/National'))
  if(file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_overall_draws.rds'))&
     file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_overall_res.rds'))){
    
    natl.overall.aggre.draws <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_overall_draws.rds'))
    natl.overall.aggre.res <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_overall_res.rds'))

  }else{
    
    adm.aggre.natl.grid.overall <- adm.pop.grid.overall %>% 
      mutate(group = paste(birth.year.int)) %>%
      group_by(group) %>% 
      mutate(group.pop = sum(pop)) %>%
      ungroup() %>%
      mutate(aggre.weight =pop/group.pop) 
    
    natl.overall.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.natl.grid.overall,
                                                   post.draws = adm1.fixed.overall.draws,
                                                   new_column_names = c("birth.year.int") ,
                                                   nSamp=1000)
    
    saveRDS(natl.overall.aggre.draws,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_overall_draws.rds'))
    
    natl.overall.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = natl.overall.aggre.draws,
                                                                   est.grid = natl.overall.aggre.draws$est.grid,
                                                                   subnational =F,
                                                                   nSamp=1000,
                                                                   start.year = min(educ_dat_ind$birth.year)
    )
    saveRDS(natl.overall.aggre.res,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_overall_res.rds'))
    
    
    
    
  }
  
  
  
  ### aggregated urban national
  setwd(paste0(res.dir,'/INLA/National'))
  
  if(file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_U_draws.rds'))&
     file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_U_res.rds'))){
    
    natl.U.aggre.draws <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_U_draws.rds'))
    natl.U.aggre.res <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_U_res.rds'))
    
  }else{
    
    adm.aggre.natl.grid.U <- adm.pop.grid.U %>% 
      mutate(group = paste(birth.year.int)) %>%
      group_by(group) %>% 
      mutate(group.pop = sum(pop)) %>%
      ungroup() %>%
      mutate(aggre.weight =pop/group.pop) 
    
    natl.U.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.natl.grid.U,
                                             post.draws = adm1.fixed.U.draws,
                                             new_column_names = c("birth.year.int") ,
                                             nSamp=1000)
    
    saveRDS(natl.U.aggre.draws,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_U_draws.rds'))
    
    natl.U.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = natl.U.aggre.draws,
                                                      est.grid = natl.U.aggre.draws$est.grid,
                                                      subnational =F,
                                                      nSamp=1000,
                                                      start.year = min(educ_dat_ind$birth.year)
    )
    saveRDS(natl.U.aggre.res,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_U_res.rds'))
    
    
    
    
  }  
    
  ### aggregated rural national
  setwd(paste0(res.dir,'/INLA/National'))
  
  if(file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_R_draws.rds'))&
     file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_R_res.rds'))){
    
    natl.R.aggre.draws <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_R_draws.rds'))
    natl.R.aggre.res <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_R_res.rds'))
    
  }else{
    
    adm.aggre.natl.grid.R <- adm.pop.grid.R %>% 
      mutate(group = paste(birth.year.int)) %>%
      group_by(group) %>% 
      mutate(group.pop = sum(pop)) %>%
      ungroup() %>%
      mutate(aggre.weight =pop/group.pop) 
    
    natl.R.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.natl.grid.R,
                                             post.draws = adm1.fixed.R.draws,
                                             new_column_names = c("birth.year.int") ,
                                             nSamp=1000)
    
    saveRDS(natl.R.aggre.draws,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_R_draws.rds'))
    
    natl.R.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = natl.R.aggre.draws,
                                                      est.grid = natl.R.aggre.draws$est.grid,
                                                      subnational =F,
                                                      nSamp=1000,
                                                      start.year = min(educ_dat_ind$birth.year)
    )
    saveRDS(natl.R.aggre.res,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_R_res.rds'))
    
    
    
    
  }
  
  
  ### aggregated UR difference national
  setwd(paste0(res.dir,'/INLA/National'))
  
  if(file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_UR_diff_draws.rds'))&
     file.exists(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_UR_diff_res.rds'))){
    
    natl.UR.diff.aggre.draws <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_UR_diff_draws.rds'))
    natl.UR.diff.aggre.res <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_UR_diff_res.rds'))
    
  }else{
    
    natl.UR.diff.aggre.draws <- cal.Educ.UR.diff(post.draws.U = natl.U.aggre.draws,
                                                 post.draws.R = natl.R.aggre.draws,
                                                 est.grid = natl.R.aggre.draws$est.grid,
                                                 nSamp = 1000)
    
    saveRDS(natl.UR.diff.aggre.draws,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_UR_diff_draws.rds'))
    
    natl.UR.diff.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = natl.UR.diff.aggre.draws,
                                                            est.grid = natl.UR.diff.aggre.draws$est.grid,
                                                            subnational =F,
                                                            nSamp=1000,
                                                            start.year = min(educ_dat_ind$birth.year)
    )
    saveRDS(natl.UR.diff.aggre.res,paste0('natl_aggre_adm1_fixed_RW',rw.order,'_UR_diff_res.rds'))
    
    
    
    
  }
  
  

}
