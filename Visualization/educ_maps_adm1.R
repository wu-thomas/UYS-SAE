#################################################
###### Load Packages
#################################################
rm(list = ls())

# Load libraries and info ----------------------------------------------------------
library(dplyr)
library(labelled)
library(survey)
library(haven)
library(rdhs)
library(data.table)

library(SUMMER)
library(ggplot2)
library(RColorBrewer)
library(xtable)
#library(geofacet)

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

#dir.create(file.path('.', 'Education'))
# if(!dir.exists(paths = paste0('Education'))){
#   dir.create(file.path('.', 'Education'))
# }

if(!dir.exists(paths = paste0('Visualization'))){
  dir.create(file.path('Visualization'))
}

if(!dir.exists(paths = paste0('Visualization/National'))){
  dir.create(file.path('Visualization/National'))
}


if(!dir.exists(paths = paste0('Visualization/Admin1'))){
  dir.create(file.path('Visualization/Admin1'))
}



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

### prepare polygon files
toplot.poly.adm1 <- country_shp_smoothed[['Admin-1']]
toplot.poly.adm1$Internal <- c(1:dim(toplot.poly.adm1)[1])
toplot.poly.adm1$admin1.char <- paste0('admin1_',toplot.poly.adm1$Internal)

toplot.poly.adm2 <- country_shp_smoothed[['Admin-2']]
toplot.poly.adm2$Internal <- c(1:dim(toplot.poly.adm2)[1])
toplot.poly.adm2$admin2.char <- paste0('admin2_',toplot.poly.adm2$Internal)

### prepare admin names
adm1.names = admin1_info$data[,c('admin1.name','admin1.char','admin1.num')]
adm1.names$region.int <- adm1.names$admin1.num

adm2.names = admin2_info$data[,c('admin2.name','admin2.char','admin2.num')]
adm2.names$region.int <- adm2.names$admin2.num

### grid for geofacet
# poly.adm1.grid <- grid_auto(toplot.poly.adm1[!toplot.poly.adm1$Internal%in%c(7,8,12,13,18),
#                                              c('Internal','geometry')],names='Internal',
#                             seed=20)
#
# poly.adm1.grid <- poly.adm1.grid %>% rename(code = name_Internal)
# poly.adm1.grid <- rbind(poly.adm1.grid,
#                         data.frame(code = c(7,12,8,18,13),
#                                    row = c(1,2,4,5,6),
#                                    col=c(7,7,7,7,7)))
#
# poly.adm1.grid <- poly.adm1.grid%>%mutate(name=code)
# #grid_preview(poly.adm1.grid,label='code')


################################################################
######### prepare figures for specific version of INLA model
################################################################

### original data


### all versions of time trend (main only vs +iid, rw1 vs rw2)
trend.list <-  c('rw1_main','rw1_iid','rw2_main','rw2_iid')

birth.cohort.trend <- trend.list[2]
plot.file.suffix <- paste0('_INLA_',trend.list[2])
rw.order = as.numeric(substring(birth.cohort.trend,3,3))


################################################################
######### load Admin-1 estimates
################################################################

### load INLA estimates
setwd(paste0(res.dir,'/INLA/Admin1'))

### yearly education
adm1.overall.INLA.yearly.res <- readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_overall_res.rds'))
adm1.overall.INLA.yearly.educ <- adm1.overall.INLA.yearly.res$educ_years

### education years by age group
adm1.overall.INLA.5yr.res <- readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_overall_5yr_res.rds'))
adm1.overall.INLA.educ.5yr <- adm1.overall.INLA.5yr.res$educ_years


### process UR difference
adm1.UR.diff.INLA.res <- readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_UR_diff_5yr_res.rds'))

adm1.UR.diff.INLA.educ.5yrs <- adm1.UR.diff.INLA.res$educ_years
adm1.UR.diff.INLA.educ.5yrs <- adm1.UR.diff.INLA.educ.5yrs %>%
  mutate(mean= case_when(region.int==2 ~ NA,
                         TRUE~mean))
adm1.UR.diff.INLA.educ.5yrs <- adm1.UR.diff.INLA.educ.5yrs %>%
  mutate(mean= case_when(mean<0 ~ 0,
                         TRUE~mean))

### calculate exceedance probabilities for UR difference
adm1.UR.diff.INLA.draws <- readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_UR_diff_5yr_est_draws.rds'))
adm1.UR.diff.INLA.draws.educ <- adm1.UR.diff.INLA.draws$educ_year_draws

exceed_cut_off = 2
adm1.UR.diff.exceed.vec <- rowMeans(t(adm1.UR.diff.INLA.draws.educ) > exceed_cut_off)
adm1.UR.diff.INLA.exceed <- adm1.UR.diff.INLA.draws$est.grid
adm1.UR.diff.INLA.exceed$exceed_prob <- adm1.UR.diff.exceed.vec
adm1.UR.diff.INLA.exceed$admin1.num <- adm1.UR.diff.INLA.exceed$region.int



################################################################
######### plot admin1, 5yr age group
################################################################

g.adm1.educ.5yr <- SUMMER::mapPlot(data = adm1.overall.INLA.educ.5yr, geo = toplot.poly.adm1,
                                       variables = c("agegrp"),
                                       size=0.1,ylim= c(min(adm1.overall.INLA.educ.5yr$mean),max(adm1.overall.INLA.educ.5yr$mean)),
                                       values = c("mean"), by.data = "admin1.num", by.geo = "Internal",
                                       direction = -1, legend.label = "Ultimate Years of Schooling",
                                       is.long = TRUE, ncol = 4)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),
         legend.text=element_text(size=16),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18),
         strip.text.x = element_text(size = 17))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title='Ultimate Years of Schooling',
                                label.position = "bottom"))+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "Spectral"))


### save plot
setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("overall_adm1_5yrs_map.pdf"))){

  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.educ.5yr, width=11, height = 8, file = paste0("overall_adm1_5yrs_map.pdf"))

}


################################################################
######### plot UR difference UYS, 5yr age group
################################################################

g.adm1.UR.diff.5yrs <- SUMMER::mapPlot(data = adm1.UR.diff.INLA.educ.5yrs, geo = toplot.poly.adm1,
                               variables = c("agegrp"),
                           size=0.1,ylim= c(min(adm1.UR.diff.INLA.educ.5yrs$mean),max(adm1.UR.diff.INLA.educ.5yrs$mean)),
                           values = c("mean"), by.data = "admin1.num", by.geo = "Internal",
                           direction = 1, legend.label = "UR Difference",
                           is.long = TRUE, ncol = 4)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),
         legend.text=element_text(size=14),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18),
         strip.text.x = element_text(size = 15))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title='Education Gap (Urban minus Rural)',
                                label.position = "bottom"))+
  scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       breaks = c(0, 1, 2, 3, 4),
                       labels = c(expression(phantom(x) <=0), "1", "2", "3", "4"))

  # scale_fill_viridis_c(
  #   option = "D",
  #   direction = -1,
  #   breaks = c(0, 1, 2, 3, 4),
  #   labels = c(expression(phantom(x) <=0), "1", "2", "3", "4")  # Ensures spacing is correct
  # )

#Sys.setlocale("LC_ALL", "en_US.UTF-8")

### save plot
setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("UR_diff_adm1_5yrs_map.pdf"))){

  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.UR.diff.5yrs, width=11, height = 8, file = paste0("UR_diff_adm1_5yrs_map.pdf"))

}




################################################################
######### plot UR difference exceedance prob, 5yr age group
################################################################

adm1.UR.diff.INLA.exceed <- adm1.UR.diff.INLA.exceed %>%
  mutate(exceed_prob= case_when(admin1.num==2~NA,
                                TRUE~exceed_prob))

g.adm1.UR.diff.5yrs.exceed <- SUMMER::mapPlot(data = adm1.UR.diff.INLA.exceed, geo = toplot.poly.adm1,
                                       variables = c("agegrp"),
                                       size=0.1,ylim= c(min(adm1.UR.diff.INLA.educ.5yrs$mean),max(adm1.UR.diff.INLA.educ.5yrs$mean)),
                                       values = c("exceed_prob"), by.data = "admin1.num", by.geo = "Internal",
                                       direction = 1, legend.label = "UR Difference",
                                       is.long = TRUE, ncol = 4)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),
         legend.text=element_text(size=14),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18),
         strip.text.x = element_text(size = 17))+
  ggplot2::scale_fill_gradientn(colours=rev(viridisLite::rocket(10)[3:10]),
                                labels = scales::label_number(accuracy = 0.2))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title=paste0('Probability of Education Gap Exceeding ',exceed_cut_off,' Years'),
                                label.position = "bottom"))

#Sys.setlocale("LC_ALL", "en_US.UTF-8")

### save plot
setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("UR_diff_adm1_5yrs_exceedance_map.pdf"))){

  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.UR.diff.5yrs.exceed, width=11, height = 8, file = paste0("UR_diff_adm1_5yrs_exceedance_map.pdf"))

}



################################################################
######### plot difference in uncertainties across methods
################################################################

### create directories to store results
setwd(paste0(res.dir))


if(!dir.exists(paths = paste0('Visualization/Admin1'))){
  dir.create(file.path('Visualization/Admin1'))
}

for(tmp_agegrp in paste0(c(1:7)*5+10,'-',c(1:7)*5+14)){
  if(!dir.exists(paths = paste0('Visualization/Admin1/',tmp_agegrp))){
    dir.create(file.path('Visualization/Admin1/',tmp_agegrp))
  }
}

### load INLA estimates
setwd(paste0(res.dir,'/INLA/Admin1'))

### education years by age group
adm1.overall.INLA.5yr.res <- readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_overall_5yr_res.rds'))
adm1.overall.INLA.5yr.UYS <- adm1.overall.INLA.5yr.res$educ_years
adm1.overall.INLA.5yr.UYS <- adm1.overall.INLA.5yr.UYS %>%
  select(mean,lower,upper,admin1.char,agegrp)
adm1.overall.INLA.5yr.UYS$method <- 'Spatial Model'

### load GLM
setwd(paste0(res.dir,'/GLM'))
adm1.overall.GLM.5yr.res <- readRDS(paste0('adm1_5yr_GLM_overall.rds'))
adm1.overall.GLM.5yr.UYS <- adm1.overall.GLM.5yr.res$educ_years
adm1.overall.GLM.5yr.UYS <- adm1.overall.GLM.5yr.UYS %>%
  mutate(agegrp_start=survey_year-as.numeric(birth.year.group)-2)
adm1.overall.GLM.5yr.UYS <- adm1.overall.GLM.5yr.UYS %>%
  mutate(agegrp = paste0(agegrp_start,'-',agegrp_start+4))
adm1.overall.GLM.5yr.UYS <- adm1.overall.GLM.5yr.UYS %>%
  select(mean,lower,upper,admin1.char,agegrp)
adm1.overall.GLM.5yr.UYS$method <- 'Survey GLM'

### load unadjusted direct estimates
setwd(paste0(res.dir,'/Direct_unadj/'))
adm1.overall.direct.unadj.agegrp <- readRDS('adm1_agegrp_direct_overall.rds')
adm1.overall.direct.unadj.5yr.UYS <- adm1.overall.direct.unadj.agegrp$educ_years
adm1.overall.direct.unadj.5yr.UYS <- adm1.overall.direct.unadj.5yr.UYS %>%
  mutate(agegrp_start=survey_year-as.numeric(group)-2)
adm1.overall.direct.unadj.5yr.UYS <- adm1.overall.direct.unadj.5yr.UYS %>%
  mutate(agegrp = paste0(agegrp_start,'-',agegrp_start+4))
adm1.overall.direct.unadj.5yr.UYS <- adm1.overall.direct.unadj.5yr.UYS %>%
  select(mean,lower,upper,admin1.char,agegrp)
adm1.overall.direct.unadj.5yr.UYS$method <- 'Naive Weighted'


### merge data
adm1.overall.5yr.all.method <- rbind(adm1.overall.INLA.5yr.UYS,
                                     adm1.overall.GLM.5yr.UYS,
                                     adm1.overall.direct.unadj.5yr.UYS)
adm1.overall.5yr.all.method <- adm1.overall.5yr.all.method %>%
  mutate(CI.width = upper-lower) %>%
  left_join(adm1.names,by='admin1.char')




#### tabulate admin-1 by method, uncertainty
summary_adm1_uncertainty <- adm1.overall.5yr.all.method %>%
  group_by(agegrp,method) %>%
  summarise(
    Admin1 = sprintf("%.2f (%.2f, %.2f)", median(CI.width, na.rm = TRUE),
                     quantile(CI.width, 0.10, na.rm = TRUE),
                     quantile(CI.width, 0.90, na.rm = TRUE))
  )
latex_table_uncertainty <- xtable(summary_adm1_uncertainty, digits = 2)



### truncate the maximum to display at 3, extreme values in direct estimates disrupt color schemes
adm1.overall.5yr.all.method <- adm1.overall.5yr.all.method %>%
  mutate(CI.width= case_when(CI.width>3 ~ 3,
                         TRUE~CI.width))

adm1.overall.5yr.all.method$method <- factor(adm1.overall.5yr.all.method$method,
                               levels = c("Naive Weighted",'Survey GLM','Spatial Model'))

for(tmp_agegrp in paste0(c(1:7)*5+10,'-',c(1:7)*5+14)){

  #tmp_agegrp = '25-29'

  adm1.overall.5yr.all.method.agegrp = adm1.overall.5yr.all.method %>%
    filter(agegrp==tmp_agegrp)

  g.adm1.agegrp.CI.width <- SUMMER::mapPlot(data = adm1.overall.5yr.all.method.agegrp, geo = toplot.poly.adm1,
                                            variables = c("method"),
                                            size=0.1,ylim= c(min(adm1.overall.5yr.all.method.agegrp$CI.width),max(adm1.overall.5yr.all.method.agegrp$CI.width)),
                                            values = c("CI.width"), by.data = "admin1.num", by.geo = "Internal",
                                            direction = -1, legend.label = "Width of 95% CI",
                                            is.long = TRUE, ncol = 4)+
    theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),
           legend.text=element_text(size=14),
           legend.key.width = unit(2,'cm'),legend.title = element_text(size=18),
           strip.text.x = element_text(size = 16))+
    guides(fill = guide_colourbar(title.position = "top",
                                  title.hjust = .5,
                                  title='Width of 95% CI',
                                  label.position = "bottom"))+
    scale_fill_gradientn(
      colours = rev(viridisLite::plasma(10)[3:10]),  # Use the specified plasma color range
      breaks = c(0, 1, 2, 3),
      labels = c('0', "1", "2", expression(phantom(x) >=3))  # Ensures spacing is correct
    )


  setwd(paste0(res.dir,'/Visualization/Admin1/',tmp_agegrp))
  if(file.exists(paste0("overall_adm1_CI_width_",tmp_agegrp ,".pdf"))){

  #  setwd(paste0(res.dir,'/Visualization/Admin1/',tmp_agegrp))
    ggsave(g.adm1.agegrp.CI.width, width=12, height = 8,
           file = paste0("overall_adm1_CI_width_",tmp_agegrp,".pdf"))
  }

}











################################################################
######### load Admin-2 estimates
################################################################

### load INLA estimates
setwd(paste0(res.dir,'/INLA/Admin2'))

### yearly education
adm2.overall.INLA.yearly.res <- readRDS(paste0('adm2_INLA_BYM2_RW',rw.order,'_typeIV','_overall_res.rds'))
adm2.overall.INLA.yearly.educ <- adm2.overall.INLA.yearly.res$educ_years

### education years by age group
adm2.overall.INLA.5yr.res <- readRDS(paste0('adm2_INLA_BYM2_RW',rw.order,'_typeIV','_overall_5yr_res.rds'))
adm2.overall.INLA.educ.5yr <- adm2.overall.INLA.5yr.res$educ_years

### process UR difference
adm2.UR.diff.INLA.res <- readRDS(paste0('adm2_INLA_BYM2_RW',rw.order,'_typeIV','_UR_diff_5yr_res.rds'))

adm2.UR.diff.INLA.educ.5yrs <- adm2.UR.diff.INLA.res$educ_years

adm2.UR.diff.INLA.educ.5yrs <- adm2.UR.diff.INLA.educ.5yrs %>%
  mutate(mean= case_when(region.int%in% c(10,11,12) ~ NA,
                         TRUE~mean))
adm2.UR.diff.INLA.educ.5yrs <- adm2.UR.diff.INLA.educ.5yrs %>%
  mutate(mean= case_when(mean<0 ~ 0,
                         TRUE~mean))

adm2.UR.diff.INLA.educ.5yrs <- adm2.UR.diff.INLA.educ.5yrs %>%
  mutate(mean= case_when(mean>5 ~ 5,
                         TRUE~mean))

setwd(paste0(res.dir,'/INLA/Admin2'))

### calculate exceedance probabilities for UR difference
adm2.UR.diff.INLA.draws <- readRDS(paste0('adm2_INLA_BYM2_RW',rw.order,'_typeIV','_UR_diff_5yr_est_draws.rds'))
adm2.UR.diff.INLA.draws.educ <- adm2.UR.diff.INLA.draws$educ_year_draws

exceed_cut_off = 2
adm2.UR.diff.exceed.vec <- rowMeans(t(adm2.UR.diff.INLA.draws.educ) > exceed_cut_off)
adm2.UR.diff.INLA.exceed <- adm2.UR.diff.INLA.draws$est.grid
adm2.UR.diff.INLA.exceed$exceed_prob <- adm2.UR.diff.exceed.vec
adm2.UR.diff.INLA.exceed$admin2.num <- adm2.UR.diff.INLA.exceed$region.int




################################################################
######### plot admin2, 5yr age group
################################################################

g.adm2.educ.5yr <- SUMMER::mapPlot(data = adm2.overall.INLA.educ.5yr, geo = toplot.poly.adm2,
                                   variables = c("agegrp"),
                                   size=0.1,ylim= c(min(adm2.overall.INLA.educ.5yr$mean),max(adm2.overall.INLA.educ.5yr$mean)),
                                   values = c("mean"), by.data = "admin2.char", by.geo = "admin2.char",
                                   direction = -1, legend.label = "Ultimate Years of Schooling",
                                   is.long = TRUE, ncol = 4)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),
         legend.text=element_text(size=16),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18),
         strip.text.x = element_text(size = 17))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title='Ultimate Years of Schooling',
                                label.position = "bottom"))+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "Spectral"))


### save plot
setwd(paste0(res.dir,'/Visualization/Admin2'))

if(!file.exists(paste0("overall_adm2_5yrs_map.pdf"))){

  setwd(paste0(res.dir,'/Visualization/Admin2'))
  ggsave(g.adm2.educ.5yr, width=11, height = 8, file = paste0("overall_adm2_5yrs_map.pdf"))

}




################################################################
######### plot adm1 and adm2 together
################################################################


### prepare data
tmp_agegrp <- '15-19'

adm1.overall.INLA.UYS.5yr.agegrp <- adm1.overall.INLA.educ.5yr %>%
  filter(agegrp==tmp_agegrp)
adm1.overall.INLA.UYS.5yr.agegrp$method <- 'Admin-1'

adm2.overall.INLA.UYS.5yr.agegrp <- adm2.overall.INLA.educ.5yr %>%
  filter(agegrp==tmp_agegrp)
adm2.overall.INLA.UYS.5yr.agegrp$method <- 'Admin-2'

UYS.range.tmp <- c(min(c(adm1.overall.INLA.UYS.5yr.agegrp$mean,adm2.overall.INLA.UYS.5yr.agegrp$mean)),
                   max(c(adm1.overall.INLA.UYS.5yr.agegrp$mean,adm2.overall.INLA.UYS.5yr.agegrp$mean)))

### Admin-1 single map

g.adm1.5yr.INLA.agegrp <- SUMMER::mapPlot(adm1.overall.INLA.UYS.5yr.agegrp,
                                        variables = "method", values = "mean",
                                        by.data = "admin1.num", geo = toplot.poly.adm1,
                                        by.geo = "Internal", is.long = TRUE,
                                        removetab = T)+
  ggplot2::theme (legend.text=ggplot2::element_text(size=16),
                  legend.title = ggplot2::element_text(size=18),
                  strip.text.x = ggplot2::element_text(size = 16),
                  legend.key.height = ggplot2::unit(0.6,'cm'),
                  legend.key.width = ggplot2::unit(2,'cm'))+
  ggplot2::scale_fill_gradientn(colours= RColorBrewer::brewer.pal(11, "Spectral"), #rev(viridisLite::viridis(10)),
                                limits=UYS.range.tmp,
                                labels = scales::label_number(accuracy = 2),
                                guide = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5),
                                name=paste0('Ultimate Years of Schooling'))



### Admin-2 single map
g.adm2.5yr.INLA.agegrp <- SUMMER::mapPlot(adm2.overall.INLA.UYS.5yr.agegrp,
                                 variables = "method", values = "mean",
                              by.data = "admin2.num", geo = toplot.poly.adm2,
                              by.geo = "Internal", is.long = TRUE,
                              removetab = T)+
  ggplot2::theme (legend.text=ggplot2::element_text(size=16),
                  legend.title = ggplot2::element_text(size=18),
                  strip.text.x = ggplot2::element_text(size = 16),
                  legend.key.height = ggplot2::unit(0.6,'cm'),
                  legend.key.width = ggplot2::unit(2,'cm'))+
  ggplot2::scale_fill_gradientn(colours=RColorBrewer::brewer.pal(11, "Spectral"), #rev(viridisLite::viridis(10)),
                                limits=UYS.range.tmp,
                                labels = scales::label_number(accuracy = 2),
                                guide = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5),
                                name=paste0('Ultimate Years of Schooling'))


### map together
g.adm1.adm2 <- patchwork::wrap_plots(list(g.adm1.5yr.INLA.agegrp,g.adm2.5yr.INLA.agegrp), ncol = 2)+
  patchwork::plot_layout(guides = "collect") & ggplot2::theme(legend.position = "bottom")

### save plot
setwd(paste0(res.dir,'/Visualization/Admin2'))

if(!file.exists(paste0("overall_adm1_adm2_compare_5yrs_map.pdf"))){

  setwd(paste0(res.dir,'/Visualization/Admin2'))
  ggsave(g.adm1.adm2, width=11, height = 8, file = paste0("overall_adm1_adm2_compare_5yrs_map.pdf"))

}


################################################################
######### summarize admin-1/2 estimates, tabulation
################################################################

library(xtable)

summary_adm1_5yr <- adm1.overall.INLA.educ.5yr %>%
  group_by(agegrp) %>%
  summarise(
    Admin1 = sprintf("%.2f (%.2f, %.2f)", median(mean, na.rm = TRUE),
                     quantile(mean, 0.10, na.rm = TRUE),
                     quantile(mean, 0.90, na.rm = TRUE))
  )

# Compute the mean and (10, 90) percentile for each age group in Admin-2
summary_adm2_5yr <- adm2.overall.INLA.educ.5yr %>%
  group_by(agegrp) %>%
  summarise(
    Admin2 = sprintf("%.2f (%.2f, %.2f)", median(mean, na.rm = TRUE),
                     quantile(mean, 0.10, na.rm = TRUE),
                     quantile(mean, 0.90, na.rm = TRUE))
  )

# Merge the two summaries by age group
summary_combined <- full_join(summary_adm1_5yr, summary_adm2_5yr, by = "agegrp")

# Convert to LaTeX format
latex_table <- xtable(summary_combined, digits = 2)

# Print LaTeX table
print(latex_table, include.rownames = FALSE, booktabs = TRUE)





################################################################
######### boxplot admin-1/2 estimates, tabulation
################################################################

# Combine raw data from Admin1 and Admin2 into one dataset
adm1.overall.INLA.educ.5yr <- adm1.overall.INLA.educ.5yr %>%
  mutate(admin_level = "Admin1")

adm2.overall.INLA.educ.5yr <- adm2.overall.INLA.educ.5yr %>%
  mutate(admin_level = "Admin2")

# Merge datasets
combined_data <- bind_rows(adm1.overall.INLA.educ.5yr, adm2.overall.INLA.educ.5yr)


# Define a fixed y-axis range (adjust based on data distribution)
y_limits <- c(min(combined_data$mean, na.rm = TRUE)-0.75, max(combined_data$mean, na.rm = TRUE)+1.5)

# Create violin plot
adm1.adm2.violin.box <- ggplot(combined_data, aes(x = admin_level, y = mean, fill = admin_level)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot with transparency
  geom_boxplot(width = 0.15, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.5) +  # Overlay boxplot for median/IQR
  facet_wrap(~ agegrp, scales = "fixed",ncol=4) +  # Facet by age group with fixed scales
  labs(x = NULL, y = "UYS",  # Remove x-axis label
       fill = "Admin Level") +
  # theme_minimal() +
  scale_fill_manual(values = c("Admin1" = "#1f77b4", "Admin2" = "#ff7f0e")) +  # Blue & Orange for clarity
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank()) +
  coord_cartesian(ylim = y_limits)+
  scale_y_continuous(breaks = c(1:5)*2.5) +
  ggplot2::theme (legend.position = "bottom",
                  legend.text=ggplot2::element_text(size=14),
                  legend.title = ggplot2::element_text(size=14),
                  strip.text.x = ggplot2::element_text(size = 15),
                  axis.text.y = element_text(size = 14),
                  axis.title=element_text(size=15),
                  text = element_text(size = 15),
                  legend.key.height = ggplot2::unit(1,'cm'),
                  legend.key.width = ggplot2::unit(1.5,'cm'))



### save plot
setwd(paste0(res.dir,'/Visualization/Admin2'))

if(!file.exists(paste0("overall_adm1_adm2_compare_violin_box.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin2'))
  ggsave(adm1.adm2.violin.box, width=10, height = 8, file = paste0("overall_adm1_adm2_compare_violin_box.pdf"))
  
}




################################################################
######### plot adm2 UR difference UYS, 5yr age group
################################################################

g.adm2.UR.diff.5yrs <- SUMMER::mapPlot(data = adm2.UR.diff.INLA.educ.5yrs, geo = toplot.poly.adm2,
                                       variables = c("agegrp"),
                                       size=0.1,ylim= c(min(adm2.UR.diff.INLA.educ.5yrs$mean,na.rm=T),max(adm2.UR.diff.INLA.educ.5yrs$mean,na.rm=T)),
                                       values = c("mean"), by.data = "admin2.num", by.geo = "Internal",
                                       direction = 1, legend.label = "UR Difference",
                                       is.long = TRUE, ncol = 4)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),
         legend.text=element_text(size=14),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18),
         strip.text.x = element_text(size = 15))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title='Education Gap (Urban minus Rural)',
                                label.position = "bottom"))+
  scale_fill_viridis_c(
    option = "D",
    direction = -1,
    breaks = c(0, 1, 2, 3, 4,5),
    labels = c(expression(phantom(x) <=0), "1", "2", "3", "4",expression(phantom(x) >=5))  # Ensures spacing is correct
  )

#Sys.setlocale("LC_ALL", "en_US.UTF-8")

### save plot
setwd(paste0(res.dir,'/Visualization/Admin2'))

if(!file.exists(paste0("UR_diff_adm2_5yrs_map.pdf"))){

  setwd(paste0(res.dir,'/Visualization/Admin2'))
  ggsave(g.adm2.UR.diff.5yrs, width=11, height = 8, file = paste0("UR_diff_adm2_5yrs_map.pdf"))

}



################################################################
######### plot adm2 UR difference exceedance prob, 5yr age group
################################################################

adm2.UR.diff.INLA.exceed <- adm2.UR.diff.INLA.exceed %>%
  mutate(exceed_prob= case_when(admin2.num %in% c(10,11,12)~NA,
                                TRUE~exceed_prob))

g.adm2.UR.diff.5yrs.exceed <- SUMMER::mapPlot(data = adm2.UR.diff.INLA.exceed, geo = toplot.poly.adm2,
                                              variables = c("agegrp"),
                                              size=0.1,ylim= c(min(adm2.UR.diff.INLA.educ.5yrs$mean),max(adm2.UR.diff.INLA.educ.5yrs$mean)),
                                              values = c("exceed_prob"), by.data = "admin2.num", by.geo = "Internal",
                                              direction = 1, legend.label = "UR Difference",
                                              is.long = TRUE, ncol = 4)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),
         legend.text=element_text(size=14),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18),
         strip.text.x = element_text(size = 17))+
  ggplot2::scale_fill_gradientn(colours=rev(viridisLite::rocket(10)[3:10]),
                                labels = scales::label_number(accuracy = 0.2))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title=paste0('Probability of Education Gap Exceeding ',exceed_cut_off,' Years'),
                                label.position = "bottom"))

#Sys.setlocale("LC_ALL", "en_US.UTF-8")

### save plot
setwd(paste0(res.dir,'/Visualization/Admin2'))

if(!file.exists(paste0("UR_diff_adm2_5yrs_exceedance_map.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin2'))
  ggsave(g.adm2.UR.diff.5yrs.exceed, width=11, height = 8, file = paste0("UR_diff_adm2_5yrs_exceedance_map.pdf"))
  
}



