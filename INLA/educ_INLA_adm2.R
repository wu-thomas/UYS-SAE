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
library(INLA)

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
#########   set up admin level and time trend
################################################################

Amat.level <- GADM.admin2.level
analysis.adm.level <- 2

rw.order <- 1

adm.num.col <- paste0("admin", analysis.adm.level, ".num")
adm.char.col <- paste0("admin", analysis.adm.level, ".char")
adm.name.col <- paste0("admin", analysis.adm.level, ".name")
adm.pop.col <- paste0("adm", analysis.adm.level, "_pop")


################################################################
#########   create directories
################################################################

setwd(paste0(res.dir))

if(!dir.exists(paths = paste0('INLA'))){ 
  dir.create(file.path('INLA'))
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

if(analysis.adm.level==1){
  adm.names = admin1_info$data[,c('admin1.name','admin1.char','admin1.num')]
  adm.names$region.int <- adm.names$admin1.num
}

if(analysis.adm.level==2){
  adm.names = admin2_info$data[,c('admin2.name','admin2.char','admin2.num')]
  adm.names$region.int <- adm.names$admin2.num
}

################################################################
######### load data
################################################################

### individual level
setwd(paste0(data.dir,'/prepared_dat'))
educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))


###########################################################################
#########  prepare U/R fractions for aggregation 
###########################################################################

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

educ.adj.U <- merge_adm_info(data = educ.adj.U,
                             adm1.info = admin1_info$data,
                             adm2.info = admin2_info$data,
                             cluster.info = cluster.info)

educ.adj.U$strata <- educ.adj.U$v022
educ.adj.U$region.int <- educ.adj.U[[paste0("admin",analysis.adm.level,".num")]]


educ.adj.R <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='rural',],
                               compact = T, 
                               account.censor=T,
                               compact.by =  c('v001','v022', "weights","birth.year"))

educ.adj.R <- merge_adm_info(data = educ.adj.R,
                             adm1.info = admin1_info$data,
                             adm2.info = admin2_info$data,
                             cluster.info = cluster.info)

educ.adj.R$strata <- educ.adj.R$v022
educ.adj.R$region.int <- educ.adj.R[[paste0("admin",analysis.adm.level,".num")]]


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


N.time <- length(unique(educ.adj.overall$birth.year.int))

inla.rw = utils::getFromNamespace("inla.rw", 
                                  "INLA")

RW.time.prec <- inla.rw(n = N.time, order = rw.order, scale.model = FALSE, # set scale.model  = F because we'll scale in the formula
                        sparse = TRUE)

scaled.RW.time.prec <- inla.rw(n = N.time, order = rw.order, scale.model = T, # set scale.model  = F because we'll scale in the formula
                               sparse = TRUE)




################################################################
######### set up space x time grid 
################################################################

x <- expand.grid(1:N.time, 1:N.area)
time.area <- data.frame(birth.year.int = x[, 1], region.int = x[, 2], time.area = c(1:nrow(x)))

### merge with educ data 
educ.adj.U <- educ.adj.U %>% left_join(time.area, c("birth.year.int",'region.int'))
educ.adj.U <- educ.adj.U[order(educ.adj.U$birth.year.int, educ.adj.U$region.int),]

educ.adj.R <- educ.adj.R %>% left_join(time.area, c("birth.year.int",'region.int'))
educ.adj.R <- educ.adj.R[order(educ.adj.R$birth.year.int, educ.adj.R$region.int),]


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
adm.pop.grid.overall <- adm.pop.grid.overall %>% 
  mutate(pop=case_when(pop<0.01~0.01,
                       TRUE~pop))

### prepare urban population data with space-time index 
adm.pop.grid.U <- merge(comb.grid,
                        svy.year.pop.U[,c('agegrp','pop','region.int')],
                        by=c('region.int','agegrp'),all.x=T)

adm.pop.grid.U <- adm.pop.grid.U[order(adm.pop.grid.U$time.area),]
adm.pop.grid.U$order.idx <- c(1:dim(adm.pop.grid.U)[1]) ### keep track of order of rows
adm.pop.grid.U <- adm.pop.grid.U %>% 
  mutate(pop=case_when(pop<0.01~0.01,
                       TRUE~pop))


### prepare rural population data with space-time index 
adm.pop.grid.R <- merge(comb.grid,
                        svy.year.pop.R[,c('agegrp','pop','region.int')],
                        by=c('region.int','agegrp'),all.x=T)

adm.pop.grid.R <- adm.pop.grid.R[order(adm.pop.grid.R$time.area),]
adm.pop.grid.R$order.idx <- c(1:dim(adm.pop.grid.R)[1]) ### keep track of order of rows
adm.pop.grid.R <- adm.pop.grid.R %>% 
  mutate(pop=case_when(pop<0.01~0.01,
                       TRUE~pop))

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
######### fit model
################################################################

nSamp <- 1000

### Fit model 
setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','U_model.rds'))&
   file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','R_model.rds')) ){
  
  educ.INLA.U <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                'RW',rw.order,'_typeIV_','U_model.rds'))
  educ.INLA.R <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                'RW',rw.order,'_typeIV_','R_model.rds'))
  
}else{
  
  educ.INLA.U <- INLA::inla(formula= formula, 
                            family = family, 
                            control.compute =  list(config = TRUE), 
                            control.family =  list(hyper = list(rho = list(param = c(overdisp.mean, overdisp.prec)))), 
                            data = educ.adj.U, 
                            control.predictor = list(compute = TRUE),
                            Ntrials = educ.adj.U$total, 
                            verbose = F)
  
  saveRDS(educ.INLA.U,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                  'RW',rw.order,'_typeIV_','U_model.rds'))
  
  #summary(educ.INLA.U)
  
  
  educ.INLA.R <- INLA::inla(formula= formula, 
                            family = family, 
                            control.compute =  list(config = TRUE), 
                            control.family =  list(hyper = list(rho = list(param = c(overdisp.mean, overdisp.prec)))), 
                            data = educ.adj.R, 
                            control.predictor = list(compute = TRUE),
                            Ntrials = educ.adj.R$total, 
                            verbose = F)
  
  #summary(educ.INLA.R)
  saveRDS(educ.INLA.R,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                  'RW',rw.order,'_typeIV_','R_model.rds'))
  
}

################################################################
######### obtain posterior sample
################################################################

### get posterior for model parameters

setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','U_param_post.rds'))&
   file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','R_param_post.rds')) ){
  
  U.param.sample <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                   'RW',rw.order,'_typeIV_','U_param_post.rds'))
  R.param.sample <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                   'RW',rw.order,'_typeIV_','R_param_post.rds'))
  
}else{
  # Draw posterior samples for model parameters
  U.param.sample = inla.posterior.sample(n = nSamp, result = educ.INLA.U)
  saveRDS(U.param.sample,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                     'RW',rw.order,'_typeIV_','U_param_post.rds'))
  
  R.param.sample = inla.posterior.sample(n = nSamp, result = educ.INLA.R)
  saveRDS(R.param.sample,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                     'RW',rw.order,'_typeIV_','R_param_post.rds'))
}



### get posterior draws for estimates 

setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','U_est_draws.rds'))&
   file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','R_est_draws.rds')) ){
  
  U.est.draws <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                'RW',rw.order,'_typeIV_','U_est_draws.rds'))
  R.est.draws <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                'RW',rw.order,'_typeIV_','R_est_draws.rds'))
  
}else{
  # Draw posterior samples
  U.est.draws = getEduc.INLA.postsamps(est.grid= ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                       INLA.mod=educ.INLA.U , 
                                       post.sample=U.param.sample, nSamp=1000)
  
  saveRDS(U.est.draws,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                  'RW',rw.order,'_typeIV_','U_est_draws.rds'))
  
  R.est.draws = getEduc.INLA.postsamps(est.grid= ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                       INLA.mod=educ.INLA.R , 
                                       post.sample=R.param.sample, nSamp=1000)
  
  saveRDS(R.est.draws,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                  'RW',rw.order,'_typeIV_','R_est_draws.rds'))
}


################################################################
######### aggregate to subnational overall estimates 
################################################################

setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','overall_est_draws.rds'))){
  
  overall.est.draws <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                      'RW',rw.order,'_typeIV_','overall_est_draws.rds'))
  
}else{
  # Draw posterior samples
  overall.est.draws <- aggre.Educ.UR(post.draws.U = U.est.draws,
                                     post.draws.R = R.est.draws,
                                     UR.frac = ur.frac.grid,
                                     nSamp = 1000)
  
  saveRDS(overall.est.draws,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                        'RW',rw.order,'_typeIV_','overall_est_draws.rds'))
}



################################################################
######### summarize subnational yearly results 
################################################################


setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','U_res.rds'))&
   file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','R_res.rds'))&
   file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','overall_res.rds'))){
  
  U.est.summarized <- readRDS(file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                          'RW',rw.order,'_typeIV_','U_res.rds'))
  R.est.summarized <- readRDS(file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                          'RW',rw.order,'_typeIV_','R_res.rds'))
  overall.est.summarized <- readRDS(file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                                'RW',rw.order,'_typeIV_','overall_res.rds'))
  
  
}else{
  
  ### urban
  U.est.summarized <- summarize.Educ.INLA.postsamps(post.draws = U.est.draws,
                                                    est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                                    adm.names = adm.names,
                                                    subnational =T,
                                                    nSamp=1000,
                                                    start.year = min(educ_dat_ind$birth.year)
  )
  
  saveRDS(U.est.summarized,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                       'RW',rw.order,'_typeIV_','U_res.rds'))
  ### rural
  R.est.summarized <- summarize.Educ.INLA.postsamps(post.draws = R.est.draws,
                                                    est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                                    adm.names = adm.names,
                                                    subnational =T,
                                                    nSamp=1000,
                                                    start.year = min(educ_dat_ind$birth.year)
  )
  saveRDS(R.est.summarized,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                       'RW',rw.order,'_typeIV_','R_res.rds'))
  
  ### overall
  overall.est.summarized <- summarize.Educ.INLA.postsamps(post.draws = overall.est.draws,
                                                          est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                                          adm.names = adm.names,
                                                          subnational =T,
                                                          nSamp=1000,
                                                          start.year = min(educ_dat_ind$birth.year)
  )
  saveRDS(overall.est.summarized,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                             'RW',rw.order,'_typeIV_','overall_res.rds'))
  
}



################################################################
######### aggregate to 5-year agegrp subnational
################################################################

### overall
setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','overall_5yr_res.rds'))){
  
  overall.5yr.aggre.res <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                          'RW',rw.order,'_typeIV_','overall_5yr_res.rds'))
}else{
  
  adm.aggre.agegrp.grid.overall <- adm.pop.grid.overall %>% 
    mutate(group = paste(region.int,agegrp,sep='*')) %>%
    group_by(group) %>% 
    mutate(group.pop = sum(pop)) %>%
    ungroup() %>%
    mutate(aggre.weight =pop/group.pop) 
  
  overall.5yr.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.agegrp.grid.overall,
                                                post.draws = overall.est.draws,
                                                new_column_names = c("region.int",'agegrp') ,
                                                nSamp=1000)
  
  overall.5yr.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = overall.5yr.aggre.draws,
                                                         est.grid = overall.5yr.aggre.draws$est.grid,
                                                         adm.names = adm.names,
                                                         subnational =T,
                                                         nSamp=1000,
                                                         start.year = min(educ_dat_ind$birth.year)
  )
  
  saveRDS(overall.5yr.aggre.res,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                            'RW',rw.order,'_typeIV_','overall_5yr_res.rds'))
  
}



### rural
setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','R_5yr_res.rds'))){
  
  R.5yr.aggre.res <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                    'RW',rw.order,'_typeIV_','R_5yr_res.rds'))
}else{
  
  adm.aggre.agegrp.grid.R <- adm.pop.grid.R %>% 
    mutate(group = paste(region.int,agegrp,sep='*')) %>%
    group_by(group) %>% 
    mutate(group.pop = sum(pop)) %>%
    ungroup() %>%
    mutate(aggre.weight =pop/group.pop) 
  
  R.5yr.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.agegrp.grid.R,
                                          post.draws = R.est.draws,
                                          new_column_names = c("region.int",'agegrp') ,
                                          nSamp=1000)
  
  R.5yr.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = R.5yr.aggre.draws,
                                                   est.grid = R.5yr.aggre.draws$est.grid,
                                                   adm.names = adm.names,
                                                   subnational =T,
                                                   nSamp=1000,
                                                   start.year = min(educ_dat_ind$birth.year)
  )
  
  saveRDS(R.5yr.aggre.res,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                      'RW',rw.order,'_typeIV_','R_5yr_res.rds'))
  
}


### urban
setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','U_5yr_res.rds'))){
  
  U.5yr.aggre.res <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                    'RW',rw.order,'_typeIV_','U_5yr_res.rds'))
  
}else{
  
  adm.aggre.agegrp.grid.U <- adm.pop.grid.U %>% 
    mutate(group = paste(region.int,agegrp,sep='*')) %>%
    group_by(group) %>% 
    mutate(group.pop = sum(pop)) %>%
    ungroup() %>%
    mutate(aggre.weight =pop/group.pop) 
  
  U.5yr.aggre.draws <- aggre.Educ.general(aggre.grid = adm.aggre.agegrp.grid.U,
                                          post.draws = U.est.draws,
                                          new_column_names = c("region.int",'agegrp') ,
                                          nSamp=1000)
  
  U.5yr.aggre.res <- summarize.Educ.INLA.postsamps(post.draws = U.5yr.aggre.draws,
                                                   est.grid = U.5yr.aggre.draws$est.grid,
                                                   adm.names = adm.names,
                                                   subnational =T,
                                                   nSamp=1000,
                                                   start.year = min(educ_dat_ind$birth.year)
  )
  
  saveRDS(U.5yr.aggre.res,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                      'RW',rw.order,'_typeIV_','U_5yr_res.rds'))
  
}


################################################################
######### subnational urban/rural difference 
################################################################

setwd(paste0(res.dir,'/INLA/Admin',analysis.adm.level))

if(file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','UR_diff_res.rds'))&
   file.exists(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                      'RW',rw.order,'_typeIV_','UR_diff_5yr_res.rds'))){
  
  UR.diff.est.summarized <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                           'RW',rw.order,'_typeIV_','UR_diff_res.rds'))
  
  UR.diff.5yr.est.summarized <- readRDS(paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                               'RW',rw.order,'_typeIV_','UR_diff_5yr_res.rds'))
  
}else{
  
  ### yearly
  UR.diff.est.draws <- cal.Educ.UR.diff(post.draws.U = U.est.draws,
                                        post.draws.R = R.est.draws,
                                        est.grid = ur.frac.grid[,c('region.int','birth.year.int','time.area','order.idx')],
                                        nSamp = 1000)
  
  
  UR.diff.est.summarized <- summarize.Educ.INLA.postsamps(post.draws = UR.diff.est.draws,
                                                          est.grid = UR.diff.est.draws$est.grid,
                                                          adm.names = adm.names,
                                                          subnational =T,
                                                          nSamp=1000,
                                                          start.year = min(educ_dat_ind$birth.year)
  )
  
  saveRDS(UR.diff.est.summarized,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                             'RW',rw.order,'_typeIV_','UR_diff_res.rds'))
  
  
  ### 5-year group
  UR.diff.5yr.est.draws <- cal.Educ.UR.diff(post.draws.U = U.5yr.aggre.draws,
                                            post.draws.R = R.5yr.aggre.draws,
                                            est.grid = U.5yr.aggre.draws$est.grid,
                                            nSamp = 1000)
  
  saveRDS(UR.diff.5yr.est.draws,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                            'RW',rw.order,'_typeIV_','UR_diff_5yr_est_draws.rds'))
  
  UR.diff.5yr.est.summarized <- summarize.Educ.INLA.postsamps(post.draws = UR.diff.5yr.est.draws,
                                                              est.grid = UR.diff.5yr.est.draws$est.grid,
                                                              adm.names = adm.names,
                                                              subnational =T,
                                                              nSamp=1000,
                                                              start.year = min(educ_dat_ind$birth.year)
  )
  
  saveRDS(UR.diff.5yr.est.summarized,file=paste0('adm',analysis.adm.level,'_INLA_BYM2_',
                                                 'RW',rw.order,'_typeIV_','UR_diff_5yr_res.rds'))
  
  
  
}
