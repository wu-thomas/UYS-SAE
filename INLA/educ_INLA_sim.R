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

### directory to store simulated censorship data
setwd(paste0(res.dir,'/INLA'))

for(age_group in c(25,30,35,40)){
  
  sim_path <- paste0('sim_censored_',age_group,'_',age_group+4)
  
  if(!dir.exists(paths = paste0(sim_path))){ 
    dir.create(file.path(paste0(sim_path)))
  }
  
  for(rep_idx in 1:10){
    if(!dir.exists(paths = paste0(sim_path,'/rep_',rep_idx))){ 
      dir.create(file.path(paste0(sim_path,'/rep_',rep_idx)))
    }
    
    if(!dir.exists(paths = paste0(sim_path,'/rep_',rep_idx,'/National'))){ 
      dir.create(file.path(paste0(sim_path,'/rep_',rep_idx,'/National')))
    }
    
    if(!dir.exists(paths = paste0(sim_path,'/rep_',rep_idx,'/Admin1'))){ 
      dir.create(file.path(paste0(sim_path,'/rep_',rep_idx,'/Admin1')))
    }
    
    if(!dir.exists(paths = paste0(sim_path,'/rep_',rep_idx,'/Admin2'))){ 
      dir.create(file.path(paste0(sim_path,'/rep_',rep_idx,'/Admin2')))
    }
    
  }
  
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



analysis.adm.level = 1
Amat.level <- GADM.admin1.level
rw.order =1

adm.num.col <- paste0("admin", analysis.adm.level, ".num")
adm.char.col <- paste0("admin", analysis.adm.level, ".char")
adm.name.col <- paste0("admin", analysis.adm.level, ".name")
adm.pop.col <- paste0("adm", analysis.adm.level, "_pop")


################################################################
######### load data
################################################################

### individual level
setwd(paste0(data.dir,'/prepared_dat'))
educ_dat_ind_all <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))


###########################################################################
#########  prepare fractions for aggregation (Admin-1)
###########################################################################

### whether use survey or worldpop for aggregation weights 
use.survey.frac = F

if(analysis.adm.level==1 & use.survey.frac ==T){
  
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
#########   prepare neighborhood graph, spatial structure
################################################################

adm.nb <- spdep::poly2nb(country_shp_analysis[[paste0('Admin-',Amat.level)]], queen=F)
Amat <- spdep::nb2mat(adm.nb, style="B",zero.policy=TRUE)
# which(rowSums(Amat)==0)

## !! country and admin level specific adjustment
if(analysis.adm.level==2){
  ## connect Kaskazini Pemba Micheweni (north side of the island) to Tanga Mkinga
  Amat[40,183] <-  Amat[183,40] <- 1
  
  ## connect Kusini Pemba Mkoani (south side of the island) to Tanga Pangani 
  Amat[66,185] <-  Amat[185,66] <- 1
  
  ## connect Mjini Magharibi Magharibi A, B and Mjini with Pwani Bagamoyo
  Amat[96,127] <-  Amat[127,96] <- 1
  Amat[97,127] <-  Amat[127,97] <- 1
  Amat[98,127] <-  Amat[127,98] <- 1
  
  ## connect Kaskazini Unguja Kaskazini 'A' and 'B' and Mjini with Tanga Pangani 
  Amat[42,185] <-  Amat[185,42] <- 1
  Amat[43,185] <-  Amat[185,43] <- 1
  
  ## connect Pwani Mafia to Pwani Rufiji
  Amat[133,131] <-  Amat[131,133] <- 1
}

if(analysis.adm.level==1){
  ## connect Kaskazini Pemba and Kusini Pemba (northern island) to Tanga 
  Amat[7,31] <-  Amat[31,7] <- 1
  Amat[12,31] <-  Amat[31,12] <- 1
  
  ## connect Kaskazini Unguja (northern part of the middle island) to Tanga  
  Amat[8,31] <-  Amat[31,8] <- 1
  
  ## connect Kusini Unguja and Mjini Magharibi (southern part of the middle island) with Pwani
  Amat[13,23] <-  Amat[23,13] <- 1
  Amat[18,23] <-  Amat[23,18] <- 1
  
}


# poly.adm1 <- country_shp_analysis[[paste0('Admin-',GADM.admin1.level)]]
# tmp.Amat <- getAmat(poly.adm2, paste0(poly.adm2$NAME_1,' ',poly.adm2$NAME_2))
# tmp.Amat[133,131] <-  tmp.Amat[131,133] <- 1
# 
# which(rowSums(tmp.Amat)==0)

sp.prec.mx <- as.matrix(-Amat)
diag(sp.prec.mx) <- -rowSums(sp.prec.mx)

#quick check, sum to zero
apply(sp.prec.mx,1,sum)
apply(sp.prec.mx,2,sum)

N.area <- dim(sp.prec.mx)[1]
scaled.sp.prec.mx <- INLA::inla.scale.model(sp.prec.mx, constr = list(A = matrix(1, 
                                                                                 1, dim(sp.prec.mx)[1]), e = 0))


###############################################################################
#########   prepare temporal trend
###############################################################################


N.time <- length(unique(educ_dat_ind_all$birth.year))

inla.rw = utils::getFromNamespace("inla.rw", 
                                  "INLA")

RW.time.prec <- inla.rw(n = N.time, order = rw.order, scale.model = FALSE, # set scale.model  = F because we'll scale in the formula
                        sparse = TRUE)

scaled.RW.time.prec <- inla.rw(n = N.time, order = rw.order, scale.model = T, # set scale.model  = F because we'll scale in the formula
                               sparse = TRUE)




################################################################
######### set up space x time grid 
################################################################

N.time <- length(unique(educ_dat_ind_all$birth.year))
N.area <- dim(adm1.names)[1]

x <- expand.grid(1:N.time, 1:N.area)
time.area <- data.frame(birth.year.int = x[, 1], region.int = x[, 2], time.area = c(1:nrow(x)))


### prepare template for estimation grid  
comb.grid <- time.area
comb.grid$birth.year <- comb.grid$birth.year.int+min(educ_dat_ind_all$birth.year)-1
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


########################################
######  prepare interaction terms
########################################

### space x time 
# Kronecker product between ICAR x RW

R.space.time <- scaled.sp.prec.mx %x% scaled.RW.time.prec

setwd(paste0(data.dir,'/prepared_dat'))

if(file.exists(paste0('admin',analysis.adm.level,'_BYM2_RW',rw.order,'_typeIV_constraint.rds'))){
  
  n.constr.space.time  <- N.time*N.area -(N.time - rw.order)*(N.area - 1)
  constr.space.time <- readRDS(file=paste0('admin',analysis.adm.level,'_BYM2_RW',rw.order,'_typeIV_constraint.rds'))
  
}else{
  
  ### calculate constraints using eigen values 
  int.eigen.space.time <- eigen(R.space.time)
  n.constr.space.time  <- N.time*N.area -(N.time - rw.order)*(N.area - 1)
  
  A.space.time   <- t(matrix(int.eigen.space.time$vectors[,(dim(R.space.time )[1]-n.constr.space.time +1):dim(R.space.time )[1]],
                             ncol = n.constr.space.time ))
  
  constr.space.time   <- list(A = A.space.time , e = rep(0, dim(A.space.time )[1]))
  
  # check whether the number of constraints align with number of 0 eigen values 
  #int.eigen.space.time$values[(N.time*N.area-n.constr.space.time):(N.time*N.area-n.constr.space.time+1)]
  
  setwd(paste0(data.dir,'/prepared_dat'))
  saveRDS(constr.space.time,paste0('admin',analysis.adm.level,'_BYM2_RW',rw.order,'_typeIV_constraint.rds'))
  
}

################################################################
######### formula and hyperparameter specification
################################################################

nSamp = 1000

# Prior parameters 
overdisp.mean = 0
overdisp.prec = 0.4
family = c('betabinomial','binomial')[1]
control.predictor = list(compute = TRUE)
control.compute =  list(config = TRUE)

a <- 1
b <- 0.01

BYM2Prior <- list(
  phi = list(
    prior = "pc",
    param = c(0.5, 2/3)),
  prec = list(
    prior = "pc.prec",
    param = c(a, b)))


### base formula
formula <- Y ~ 0 + grade

### add spatial main
formula <- update(formula, ~. + f(region.int, model = "bym2", 
                                  scale.model = TRUE,
                                  constr = TRUE,
                                  graph = scaled.sp.prec.mx,
                                  hyper = BYM2Prior))

### add temporal main
formula <- update(formula, ~. + f(birth.year.int, model = "bym", 
                                  scale.model = TRUE, constr = TRUE,
                                  graph = RW.time.prec))


### add type IV interaction
formula <- update(formula, ~. +   f(time.area, 
                                    model = "generic0", Cmatrix = R.space.time,
                                    extraconstr = constr.space.time,
                                    rankdef = n.constr.space.time, 
                                    param = c(1, 0.01))
)

################################################################
######### load data
################################################################

### original data or simulated censored
for(rep_idx in c(1:10)){
  
  print(rep_idx)
  
  for(sim_age_group in c(25,30,35,40)){ ### 9999 original data, no simulated censoring
    
    print(sim_age_group)
    
    res_folder_suffix = paste0('sim_censored_',sim_age_group,'_',sim_age_group+4)
    
    ## load individual level data
    setwd(paste0(data.dir,'/prepared_dat/sim_censored/rep_',rep_idx))
    educ_dat_ind <- readRDS( paste0('educ_sim_',sim_age_group,'_',sim_age_group+4,'_overall.rds'))
    
    ################################################################
    ######### prepare analysis data set
    ################################################################
    
    ### overall
    # educ.adj.overall <- rbind(getEduc_splitted_sim(data = educ_dat_ind[educ_dat_ind$birth.year<=survey_year-24,],
    #                                                compact = T,
    #                                                account.censor=T,
    #                                                compact.by =  c('v001','v022', "weights","birth.year")),
    #                           getEduc_splitted(data = educ_dat_ind[educ_dat_ind$birth.year>survey_year-24,],
    #                                            compact = T,
    #                                            account.censor=T,
    #                                            compact.by =  c('v001','v022', "weights","birth.year"))
    # )
    # 
    # educ.adj.overall$strata <- educ.adj.overall$v022
    
    ### urban/rural specific
    educ.adj.U <- rbind(getEduc_splitted_sim(data = educ_dat_ind[(educ_dat_ind$birth.year<=survey_year-24)&
                                                                   educ_dat_ind$urban=='urban',],
                                             compact = T,
                                             account.censor=T,
                                             compact.by =  c('v001','v022', "weights","birth.year")),
                        getEduc_splitted(data = educ_dat_ind[(educ_dat_ind$birth.year>survey_year-24)&
                                                               educ_dat_ind$urban=='urban',],
                                         compact = T,
                                         account.censor=T,
                                         compact.by =  c('v001','v022', "weights","birth.year"))
    )
    
    educ.adj.U$strata <- educ.adj.U$v022
    educ.adj.U$birth.year.int <- educ.adj.U$birth.year-min(educ.adj.U$birth.year)+1

    
    # educ.adj.R <- getEduc_splitted_sim(data = educ_dat_ind[educ_dat_ind$urban=='rural',],
    #                                          compact = T,
    #                                          account.censor=T,
    #                                          compact.by =  c('v001','v022', "weights","birth.year"))  
    # 
    educ.adj.R <- rbind(getEduc_splitted_sim(data = educ_dat_ind[(educ_dat_ind$birth.year<=survey_year-24)&
                                                                   educ_dat_ind$urban=='rural',],
                                             compact = T,
                                             account.censor=T,
                                             compact.by =  c('v001','v022', "weights","birth.year")),
                        getEduc_splitted(data = educ_dat_ind[(educ_dat_ind$birth.year>survey_year-24)&
                                                               educ_dat_ind$urban=='rural',],
                                         compact = T,
                                         account.censor=T,
                                         compact.by =  c('v001','v022', "weights","birth.year"))
    )
    
    educ.adj.R$birth.year.int <- educ.adj.R$birth.year-min(educ.adj.R$birth.year)+1
    educ.adj.R$strata <- educ.adj.R$v022

    
    
    
    ################################################################
    ######### set up model parameters
    ################################################################
    
    trend.list <-  c('rw1_iid')
    
    ### run all models
    
    
    
    # birth.cohort.trend <- c('rw1_iid')
    # rw.order = as.numeric(substring(birth.cohort.trend,3,3))
    # main_or_iid = strsplit(birth.cohort.trend, '_')[[1]][2]
    # 
    # if(main_or_iid=='iid'){
    #   time.trend.type = 'bym'
    # }else{
    #   time.trend.type = 'main'
    # }
    
    ################################################################
    ######### INLA, Admin-1, rural
    ################################################################
    
    setwd(paste0(res.dir,'/INLA/',res_folder_suffix,'/rep_',rep_idx,'/Admin1'))
    
    ### rural
    educ.adj.admin1.R <- merge_adm_info(data = educ.adj.R,
                                        adm1.info = admin1_info$data,
                                        adm2.info = admin2_info$data,
                                        cluster.info = cluster.info)
    
    ## adjust Dar es Salaam, for Tanzania
    if(country=='Tanzania'&survey_year==2022){
      pseudo_obs <- educ.adj.admin1.R[1,]
      pseudo_obs$Y= NA
      pseudo_obs$admin1.num =2
      pseudo_obs$admin1.char ='admin1_2'
      pseudo_obs$admin1.name =NA
      educ.adj.admin1.R <- rbind(educ.adj.admin1.R,pseudo_obs)
    }
    
    educ.adj.admin1.R$region.int <- educ.adj.admin1.R[[paste0("admin",analysis.adm.level,".num")]]
    educ.adj.admin1.R <- educ.adj.admin1.R %>% left_join(time.area, c("birth.year.int",'region.int'))
    educ.adj.admin1.R <- educ.adj.admin1.R[order(educ.adj.admin1.R$birth.year.int, educ.adj.admin1.R$region.int),]
    
    
    ### rural model 
    adm1.R.fitted <- INLA::inla(formula= formula, 
                                family = family, 
                                control.compute =  list(config = TRUE), 
                                control.family =  list(hyper = list(rho = list(param = c(overdisp.mean, overdisp.prec)))), 
                                data = educ.adj.admin1.R, 
                                control.predictor = list(compute = TRUE),
                                Ntrials = educ.adj.admin1.R$total, 
                                verbose = F)
    
    #saveRDS(adm1.fixed.R.fitted,paste0('adm1_INLA_fixed_RW',rw.order,'_R_model.rds'))
    
    ### rural parameter sample 
    R.param.sample = inla.posterior.sample(n = nSamp, result = adm1.R.fitted)
    
    ### rural posterior draws
    adm1.R.draws <- getEduc.INLA.postsamps(est.grid= ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                           INLA.mod=adm1.R.fitted , 
                                           post.sample=R.param.sample, nSamp=1000)
    
    
    #saveRDS(adm1.R.draws,paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV_','R_est_draws.rds'))
    
    ### result summary
    # adm1.R.res <- summarize.Educ.INLA.postsamps(post.draws = adm1.R.draws,
    #                                                   est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
    #                                                   adm.names = adm1.names,
    #                                                   subnational =T,
    #                                                   nSamp=1000,
    #                                                   start.year = min(educ_dat_ind$birth.year)
    # )
    # saveRDS(adm1.R.res,paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_R_res.rds'))
    # 
    
    
    
    ################################################################
    ######### INLA, Admin-1, urban
    ################################################################
    
    ### urban 
    educ.adj.admin1.U <- merge_adm_info(data = educ.adj.U,
                                        adm1.info = admin1_info$data,
                                        adm2.info = admin2_info$data,
                                        cluster.info = cluster.info)
    
    educ.adj.admin1.U$region.int <- educ.adj.admin1.U[[paste0("admin",analysis.adm.level,".num")]]
    educ.adj.admin1.U <- educ.adj.admin1.U %>% left_join(time.area, c("birth.year.int",'region.int'))
    educ.adj.admin1.U <- educ.adj.admin1.U[order(educ.adj.admin1.U$birth.year.int, educ.adj.admin1.U$region.int),]
    
    
    ### urban model 
    adm1.U.fitted <- INLA::inla(formula= formula, 
                                family = family, 
                                control.compute =  list(config = TRUE), 
                                control.family =  list(hyper = list(rho = list(param = c(overdisp.mean, overdisp.prec)))), 
                                data = educ.adj.admin1.U, 
                                control.predictor = list(compute = TRUE),
                                Ntrials = educ.adj.admin1.U$total, 
                                verbose = F)
    
    #saveRDS(adm1.fixed.R.fitted,paste0('adm1_INLA_fixed_RW',rw.order,'_U_model.rds'))
    
    ### urban parameter sample 
    U.param.sample = inla.posterior.sample(n = nSamp, result = adm1.U.fitted)
    
    ### urban posterior draws
    adm1.U.draws <- getEduc.INLA.postsamps(est.grid= ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                           INLA.mod=adm1.U.fitted , 
                                           post.sample=U.param.sample, nSamp=1000)
    
    
    #saveRDS(adm1.U.draws,paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV_','U_est_draws.rds'))
    
    
    
    ################################################################
    ######### aggregate, Admin-1, overall
    ################################################################
    
    setwd(paste0(res.dir,'/INLA/',res_folder_suffix,'/rep_',rep_idx,'/Admin1'))
    
    ### overall
      
    adm1.overall.draws <- aggre.Educ.UR(post.draws.U = adm1.U.draws,
                                              post.draws.R = adm1.R.draws,
                                              UR.frac = ur.frac.grid,
                                              nSamp = 1000)
    
    #saveRDS(adm1.fixed.overall.draws,paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV_overall_draws.rds'))
    
    # adm1.fixed.overall.res <- summarize.Educ.INLA.postsamps(post.draws = adm1.fixed.overall.draws,
    #                                                         est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
    #                                                         adm.names = adm1.names,
    #                                                         subnational =T,
    #                                                         nSamp=1000,
    #                                                         start.year = min(educ_dat_ind$birth.year)
    # )
    # saveRDS(adm1.fixed.overall.res,paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV_overall_res.rds'))
    # 
    # 
    
    ################################################################
    ######### aggregate, national
    ################################################################
    
    setwd(paste0(res.dir,'/INLA/',res_folder_suffix,'/rep_',rep_idx,'/National'))
    
    ### overall (aggregated from fixed admin-1)
    if(file.exists(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_overall_draws.rds'))&
       file.exists(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_overall_res.rds'))){
      
      natl.overall.aggre.draws <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_overall_draws.rds'))
      natl.overall.aggre.res <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_overall_res.rds'))
      
    }else{
      
      adm.aggre.natl.grid.overall <- adm.pop.grid.overall %>% 
        mutate(group = paste(birth.year.int)) %>%
        group_by(group) %>% 
        mutate(group.pop = sum(pop)) %>%
        ungroup() %>%
        mutate(aggre.weight =pop/group.pop) 
      
      natl.overall.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.natl.grid.overall,
                                                     post.draws = adm1.overall.draws,
                                                     new_column_names = c("birth.year.int") ,
                                                     nSamp=1000)
      
      saveRDS(natl.overall.aggre.draws,paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_overall_draws.rds'))
      
      natl.overall.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = natl.overall.aggre.draws,
                                                              est.grid = natl.overall.aggre.draws$est.grid,
                                                              subnational =F,
                                                              nSamp=1000,
                                                              start.year = min(educ_dat_ind$birth.year)
      )
      saveRDS(natl.overall.aggre.res,paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_overall_res.rds'))
      
      
      
      
    }
    
    
    ### aggregated urban national
    setwd(paste0(res.dir,'/INLA/',res_folder_suffix,'/rep_',rep_idx,'/National'))
    
    if(file.exists(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_U_draws.rds'))&
       file.exists(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_U_res.rds'))){
      
      natl.U.aggre.draws <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_U_draws.rds'))
      natl.U.aggre.res <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_U_res.rds'))
      
    }else{
      
      adm.aggre.natl.grid.U <- adm.pop.grid.U %>% 
        mutate(group = paste(birth.year.int)) %>%
        group_by(group) %>% 
        mutate(group.pop = sum(pop)) %>%
        ungroup() %>%
        mutate(aggre.weight =pop/group.pop) 
      
      natl.U.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.natl.grid.U,
                                               post.draws = adm1.U.draws,
                                               new_column_names = c("birth.year.int") ,
                                               nSamp=1000)
      
      saveRDS(natl.U.aggre.draws,paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_U_draws.rds'))
      
      natl.U.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = natl.U.aggre.draws,
                                                        est.grid = natl.U.aggre.draws$est.grid,
                                                        subnational =F,
                                                        nSamp=1000,
                                                        start.year = min(educ_dat_ind$birth.year)
      )
      saveRDS(natl.U.aggre.res,paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_U_res.rds'))
      
      
      
      
    }  
    
    ### aggregated rural national
    setwd(paste0(res.dir,'/INLA/',res_folder_suffix,'/rep_',rep_idx,'/National'))
    
    if(file.exists(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_R_draws.rds'))&
       file.exists(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_R_res.rds'))){
      
      natl.R.aggre.draws <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_R_draws.rds'))
      natl.R.aggre.res <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_R_res.rds'))
      
    }else{
      
      adm.aggre.natl.grid.R <- adm.pop.grid.R %>% 
        mutate(group = paste(birth.year.int)) %>%
        group_by(group) %>% 
        mutate(group.pop = sum(pop)) %>%
        ungroup() %>%
        mutate(aggre.weight =pop/group.pop) 
      
      natl.R.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.natl.grid.R,
                                               post.draws = adm1.R.draws,
                                               new_column_names = c("birth.year.int") ,
                                               nSamp=1000)
      
      saveRDS(natl.R.aggre.draws,paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_R_draws.rds'))
      
      natl.R.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = natl.R.aggre.draws,
                                                        est.grid = natl.R.aggre.draws$est.grid,
                                                        subnational =F,
                                                        nSamp=1000,
                                                        start.year = min(educ_dat_ind$birth.year)
      )
      saveRDS(natl.R.aggre.res,paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV_R_res.rds'))
      
      
      
      
    }
    
    
    
    
  }
  
  
}