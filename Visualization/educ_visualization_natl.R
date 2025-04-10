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

################################################################
######### prepare figures for specific version of INLA model
################################################################

### original data
  
  
### all versions of time trend (main only vs +iid, rw1 vs rw2)
trend.list <-  c('rw1_main','rw1_iid','rw2_main','rw2_iid')

birth.cohort.trend <- trend.list[2]
plot.file.suffix <- ''
rw.order = as.numeric(substring(birth.cohort.trend,3,3))


################################################################
######### load national estimates
################################################################

### load unadjusted direct estimates
setwd(paste0(res.dir,'/Direct_unadj/'))

natl.U.direct.unadj.yearly <- readRDS('natl_yearly_direct_U.rds')
natl.R.direct.unadj.yearly <- readRDS('natl_yearly_direct_R.rds')
natl.overall.direct.unadj.yearly <- readRDS('natl_yearly_direct_overall.rds')

# harmonize column names
natl.U.direct.unadj.yearly.toplot <- natl.U.direct.unadj.yearly$educ_years
natl.U.direct.unadj.yearly.toplot$method <- 'Naive Weighted'
natl.R.direct.unadj.yearly.toplot <- natl.R.direct.unadj.yearly$educ_years
natl.R.direct.unadj.yearly.toplot$method <- 'Naive Weighted'
natl.overall.direct.unadj.yearly.toplot <- natl.overall.direct.unadj.yearly$educ_years
natl.overall.direct.unadj.yearly.toplot$method <- 'Naive Weighted'

### load modified direct estimates
setwd(paste0(res.dir,'/Direct_modified/'))

natl.U.direct.modified.yearly <- readRDS('natl_yearly_direct_U.rds')
natl.R.direct.modified.yearly <- readRDS('natl_yearly_direct_R.rds')
natl.overall.direct.modified.yearly <- readRDS('natl_yearly_direct_overall.rds')

# harmonize column names
natl.U.direct.modified.yearly.toplot <- natl.U.direct.modified.yearly$educ_years
natl.U.direct.modified.yearly.toplot$method <- 'Modified Weighted'
natl.R.direct.modified.yearly.toplot <- natl.R.direct.modified.yearly$educ_years
natl.R.direct.modified.yearly.toplot$method <- 'Modified Weighted'
natl.overall.direct.modified.yearly.toplot <- natl.overall.direct.modified.yearly$educ_years
natl.overall.direct.modified.yearly.toplot$method <- 'Modified Weighted'



### load GLM estimates 
setwd(paste0(res.dir,'/GLM/'))

natl.overall.adj.GLM.yearly <- readRDS('natl_yearly_GLM_overall.rds')
natl.U.adj.GLM.yearly <- readRDS('natl_yearly_GLM_U.rds')
natl.R.adj.GLM.yearly <- readRDS('natl_yearly_GLM_R.rds')

# harmonize column names
natl.U.adj.GLM.yearly.toplot <- natl.U.adj.GLM.yearly$educ_years
natl.U.adj.GLM.yearly.toplot$method <- 'Survey GLM'
natl.R.adj.GLM.yearly.toplot <- natl.R.adj.GLM.yearly$educ_years
natl.R.adj.GLM.yearly.toplot$method <- 'Survey GLM'
natl.overall.adj.GLM.yearly.toplot <- natl.overall.adj.GLM.yearly$educ_years
natl.overall.adj.GLM.yearly.toplot$method <- 'Survey GLM'

### load INLA estimates
setwd(paste0(res.dir,'/INLA/National'))

natl.overall.adj.INLA.yearly <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV','_overall_res.rds'))
natl.U.adj.INLA.yearly <-  readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV','_U_res.rds'))
natl.R.adj.INLA.yearly <-   readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV','_R_res.rds'))

# natl.overall.adj.INLA.yearly <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_overall_res.rds'))
# natl.U.adj.INLA.yearly <-  readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_U_res.rds'))
# natl.R.adj.INLA.yearly <-   readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_R_res.rds'))

# harmonize column names
natl.U.adj.INLA.yearly.toplot <- natl.U.adj.INLA.yearly$educ_years
natl.U.adj.INLA.yearly.toplot <- natl.U.adj.INLA.yearly.toplot[,c('birth.year','lower' , 'mean' ,'upper')]
colnames(natl.U.adj.INLA.yearly.toplot) <- c('group','lower' , 'mean' ,'upper')
natl.U.adj.INLA.yearly.toplot$sd <- NA
natl.U.adj.INLA.yearly.toplot$method <- 'Spatial Model'

natl.R.adj.INLA.yearly.toplot <- natl.R.adj.INLA.yearly$educ_years
natl.R.adj.INLA.yearly.toplot <- natl.R.adj.INLA.yearly.toplot[,c('birth.year','lower' , 'mean' ,'upper')]
colnames(natl.R.adj.INLA.yearly.toplot) <- c('group','lower' , 'mean' ,'upper')
natl.R.adj.INLA.yearly.toplot$sd <- NA
natl.R.adj.INLA.yearly.toplot$method <- 'Spatial Model'

natl.overall.adj.INLA.yearly.toplot <- natl.overall.adj.INLA.yearly$educ_years
natl.overall.adj.INLA.yearly.toplot <- natl.overall.adj.INLA.yearly.toplot[,c('birth.year','lower' , 'mean' ,'upper')]
colnames(natl.overall.adj.INLA.yearly.toplot) <- c('group','lower' , 'mean' ,'upper')
natl.overall.adj.INLA.yearly.toplot$sd <- NA
natl.overall.adj.INLA.yearly.toplot$method <- 'Spatial Model'


###########################################################################
######### visualize national education level by birth cohort, all methods 
###########################################################################


### urban estimates 
natl.U.toplot <- rbind(natl.U.direct.unadj.yearly.toplot,
                       natl.U.direct.modified.yearly.toplot,
                       natl.U.adj.GLM.yearly.toplot,
                       natl.U.adj.INLA.yearly.toplot)

natl.U.toplot$method <- factor(natl.U.toplot$method,
                               levels = c('Spatial Model','Survey GLM', "Naive Weighted",'Modified Weighted'))

colnames(natl.U.toplot) <-c("est","Lower_CI","Upper_CI","sd" , "year", "method")
natl.U.toplot$year <- as.numeric(natl.U.toplot$year)

custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 

g.U.natl <- ggplot(aes(x = year, group = method), data = natl.U.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.15, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.05), linewidth = 0.75) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  ylab('Ultimate Years of Schooling') +
  #ggtitle('Overall: Direct, Survey GLM, and INLA Estimates') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme_bw() +  
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 16),
    legend.key.width = unit(2, 'cm'),
    legend.title = element_text(size = 16),
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 20)
  )

setwd(paste0(res.dir,'/Visualization/National'))
if(!file.exists(paste0("urban_natl_yearly_compare",plot.file.suffix ,".pdf"))){
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.U.natl, width=11.2, height = 7, file = paste0("urban_natl_yearly_compare",plot.file.suffix ,".pdf"))
}



### rural estimates 
natl.R.toplot <- rbind(natl.R.direct.unadj.yearly.toplot,
                       natl.R.direct.modified.yearly.toplot,
                       natl.R.adj.GLM.yearly.toplot,
                       natl.R.adj.INLA.yearly.toplot)

natl.R.toplot$method <- factor(natl.R.toplot$method,
                               levels = c('Spatial Model','Survey GLM', "Naive Weighted",'Modified Weighted'))

colnames(natl.R.toplot) <-c("est","Lower_CI","Upper_CI","sd" , "year", "method")
natl.R.toplot$year <- as.numeric(natl.R.toplot$year)

custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 

g.R.natl <- ggplot(aes(x = year, group = method), data = natl.R.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.15, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.05), linewidth = 0.75) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  ylab('Ultimate Years of Schooling') +
  #ggtitle('Overall: Direct, Survey GLM, and INLA Estimates') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme_bw() +  
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 16),
    legend.key.width = unit(2, 'cm'),
    legend.title = element_text(size = 16),
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 20)
  )

setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("rural_natl_yearly_compare",plot.file.suffix ,".pdf"))){
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.R.natl, width=11.2, height = 7, file = paste0("rural_natl_yearly_compare",plot.file.suffix ,".pdf"))
}



### overall estimates 
natl.overall.toplot <- rbind(natl.overall.direct.unadj.yearly.toplot,
                             natl.overall.direct.modified.yearly.toplot,
                             natl.overall.adj.GLM.yearly.toplot,
                             natl.overall.adj.INLA.yearly.toplot)

natl.overall.toplot$method <- factor(natl.overall.toplot$method,
                                     levels = c('Spatial Model','Survey GLM', "Naive Weighted",'Modified Weighted'))

colnames(natl.overall.toplot) <-c("est","Lower_CI","Upper_CI","sd" , "year", "method")
natl.overall.toplot$year <- as.numeric(natl.overall.toplot$year)

custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 

g.overall.natl <- ggplot(aes(x = year, group = method), data = natl.overall.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.15, position = position_dodge(width = 0.15)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.15), linewidth = 0.75) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  ylab('Ultimate Years of Schooling') +
  #ggtitle('Overall: Direct, Survey GLM, and INLA Estimates') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme_bw() +  
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 16),
    legend.key.width = unit(2, 'cm'),
    legend.title = element_text(size = 16),
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 20)
  )


### save plot

setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("overall_natl_yearly_compare",plot.file.suffix ,".pdf"))){
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.overall.natl, width=11.2, height = 7, file = paste0("overall_natl_yearly_compare",plot.file.suffix ,".pdf"))
}










################################################################
######### calculate UR difference
################################################################

### load unadjusted direct estimates
natl.UR.diff.direct.unadj.toplot <- natl.overall.direct.unadj.yearly.toplot
natl.UR.diff.direct.unadj.toplot$mean <- natl.U.direct.unadj.yearly.toplot$mean- natl.R.direct.unadj.yearly.toplot$mean
natl.UR.diff.direct.unadj.toplot$sd <- sqrt((natl.U.direct.unadj.yearly.toplot$sd)^2+(natl.R.direct.unadj.yearly.toplot$sd)^2)
natl.UR.diff.direct.unadj.toplot$lower <- natl.UR.diff.direct.unadj.toplot$mean-1.96*natl.UR.diff.direct.unadj.toplot$sd
natl.UR.diff.direct.unadj.toplot$upper <- natl.UR.diff.direct.unadj.toplot$mean+1.96*natl.UR.diff.direct.unadj.toplot$sd

### load modified direct estimates
natl.UR.diff.direct.modified.toplot <- natl.overall.direct.modified.yearly.toplot
natl.UR.diff.direct.modified.toplot$mean <- natl.U.direct.modified.yearly.toplot$mean- natl.R.direct.modified.yearly.toplot$mean
natl.UR.diff.direct.modified.toplot$sd <- sqrt((natl.U.direct.modified.yearly.toplot$sd)^2+(natl.R.direct.modified.yearly.toplot$sd)^2)
natl.UR.diff.direct.modified.toplot$lower <- natl.UR.diff.direct.modified.toplot$mean-1.96*natl.UR.diff.direct.modified.toplot$sd
natl.UR.diff.direct.modified.toplot$upper <- natl.UR.diff.direct.modified.toplot$mean+1.96*natl.UR.diff.direct.modified.toplot$sd


### load GLM estimates 
natl.UR.diff.GLM.toplot <- natl.overall.adj.GLM.yearly.toplot
natl.UR.diff.GLM.toplot$mean <- natl.U.adj.GLM.yearly.toplot$mean- natl.R.adj.GLM.yearly.toplot$mean
natl.UR.diff.GLM.toplot$sd <- sqrt((natl.U.adj.GLM.yearly.toplot$sd)^2+(natl.R.adj.GLM.yearly.toplot$sd)^2)
natl.UR.diff.GLM.toplot$lower <- natl.UR.diff.GLM.toplot$mean-1.96*natl.UR.diff.GLM.toplot$sd
natl.UR.diff.GLM.toplot$upper <- natl.UR.diff.GLM.toplot$mean+1.96*natl.UR.diff.GLM.toplot$sd


### load INLA estimates
setwd(paste0(res.dir,'/INLA/National'))

INLA.UR.diff <- readRDS(paste0('natl_aggre_adm1_BYM2_RW',rw.order,'_typeIV','_UR_diff_res.rds'))
# INLA.UR.diff <- readRDS(paste0('natl_aggre_adm1_fixed_RW',rw.order,'_UR_diff_res.rds'))

# harmonize column names
natl.UR.diff.INLA.toplot <- INLA.UR.diff$educ_years
natl.UR.diff.INLA.toplot <- natl.UR.diff.INLA.toplot[,c('birth.year','lower' , 'mean' ,'upper')]
colnames(natl.UR.diff.INLA.toplot) <- c('group','lower' , 'mean' ,'upper')
natl.UR.diff.INLA.toplot$sd <- NA
natl.UR.diff.INLA.toplot$method <- 'Spatial Model'






###########################################################################
######### visualize UR difference by birth cohort, all methods 
###########################################################################

### combine estimates 
natl.UR.diff.toplot <- rbind(natl.UR.diff.direct.unadj.toplot,
                       natl.UR.diff.direct.modified.toplot,
                       natl.UR.diff.GLM.toplot,
                       natl.UR.diff.INLA.toplot)




natl.UR.diff.toplot$method <- factor(natl.UR.diff.toplot$method,
                                     levels = c('Spatial Model','Survey GLM', "Naive Weighted",'Modified Weighted'))

colnames(natl.UR.diff.toplot) <-c("est","Lower_CI","Upper_CI","sd" , "year", "method")
natl.UR.diff.toplot$year <- as.numeric(natl.UR.diff.toplot$year)

custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 

g.UR.diff.natl <- ggplot(aes(x = year, group = method), data = natl.UR.diff.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.15, position = position_dodge(width = 0.75)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.25), linewidth = 0.75) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  ylab('Ultimate Years of Schooling') +
  #ggtitle('Overall: Direct, Survey GLM, and INLA Estimates') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme_bw() +  
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 16),
    legend.key.width = unit(2, 'cm'),
    legend.title = element_text(size = 16),
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 20)
  )


### save plot
setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("UR_diff_natl_yearly_compare",plot.file.suffix ,".pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.UR.diff.natl, width=11.2, height = 7, file = paste0("UR_diff_natl_yearly_compare",plot.file.suffix ,".pdf"))
  
}

###########################################################################
######### plot U/R/overall difference together 
###########################################################################


all.data.toplot <- rbind(natl.UR.diff.toplot %>% mutate(type='Urban/Rural Difference'),
                  natl.R.toplot %>% mutate(type='Rural UYS'),
                  natl.U.toplot %>% mutate(type='Urban UYS'),
                  natl.overall.toplot %>% mutate(type='Overall UYS'))

all.data.toplot <- all.data.toplot %>% 
  mutate(type = factor(type,levels=c('Urban UYS','Rural UYS','Overall UYS','Urban/Rural Difference')))

g.all.four <- ggplot(aes(x = year, group = method), data = all.data.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.15, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.05), linewidth = 0.42) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  ylab('Ultimate Years of Schooling') +
  facet_wrap(~type, ncol = 2) +
  #ggtitle('Overall: Direct, Survey GLM, and INLA Estimates') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme_bw() +  
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 13),
    legend.key.width = unit(2, 'cm'),
    legend.title = element_text(size = 14),
    strip.text.x = element_text(size = 16),
    # strip.text.y = element_text(size = 18),
    text = element_text(size = 16),
    # axis.text.x = element_text(size = 18),
    # axis.text.y = element_text(size = 18),
    # plot.title = element_text(size = 20)
  )


### save plot
setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("UR_all_types_compare",plot.file.suffix ,".pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.all.four, width=10, height = 8, file = paste0("UR_all_types_compare",plot.file.suffix ,".pdf"))
  
}



###########################################################################
######### visualize distribution by age group 
###########################################################################


#natl.overall.INLA.p.dist <- natl.overall.adj.INLA.yearly$grade_dist_p
#natl.overall.INLA.p.dist <- natl.overall.direct.unadj.yearly$grade_dist_p
natl.overall.INLA.p.dist <- natl.overall.adj.GLM.yearly$grade_dist_p
natl.overall.INLA.p.dist$birth.year <- as.numeric(natl.overall.INLA.p.dist$group)

natl.overall.INLA.p.dist$age.start <- (floor((survey_year+survey_year_span-
                                                natl.overall.INLA.p.dist$birth.year)/5))*5
natl.overall.INLA.p.dist$age.start[natl.overall.INLA.p.dist$age.start>45] <- 45 
natl.overall.INLA.p.dist$age.start[natl.overall.INLA.p.dist$age.start<15] <- 15 

natl.overall.INLA.p.dist$agegrp <- paste0(natl.overall.INLA.p.dist$age.start,'-',natl.overall.INLA.p.dist$age.start+4,sep='')


natl.overall.INLA.educ.cat.toplot <- natl.overall.INLA.p.dist %>%
  mutate(education=case_when(grade==0~'No Education',
                             grade<7~'Some Primary',
                             grade==7~'Completed Primary',
                             grade<11~'Some Secondary',
                             grade==11~ 'Completed Secondary',
                             grade>11~ 'More than Secondary'
  ))%>%
  group_by(education,birth.year)%>%
  summarise(prob = sum(mean, na.rm = TRUE), .groups = "keep")%>%
  ungroup()%>%
  mutate(education = factor(education, levels = rev(c(
    "No Education",
    "Some Primary",
    "Completed Primary",
    "Some Secondary",
    "Completed Secondary",
    "More than Secondary")
  )))
