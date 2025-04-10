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
library(geofacet)
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



### admin names 

adm.names = admin1_info$data[,c('admin1.name','admin1.char','admin1.num')]
adm.names$region.int <- adm.names$admin1.num

### prepare geofacet grid 
toplot.poly.adm1 <- country_shp_smoothed[['Admin-1']] 
toplot.poly.adm1$Internal <- c(1:dim(toplot.poly.adm1)[1])

poly.adm1.grid <- grid_auto(toplot.poly.adm1[!toplot.poly.adm1$Internal%in%c(7,8,12,13,18),
                                             c('Internal','geometry')],names='Internal',
                            seed=20) 

poly.adm1.grid <- poly.adm1.grid %>% rename(code = name_Internal)
poly.adm1.grid <- rbind(poly.adm1.grid,
                        data.frame(code = c(7,12,8,18,13),
                                   row = c(1,2,4,5,6),
                                   col=c(7,7,7,7,7)))

poly.adm1.grid <- poly.adm1.grid%>%mutate(name=code)
poly.adm1.grid <- poly.adm1.grid%>%mutate(admin1.num=code)

poly.adm1.grid <- poly.adm1.grid %>% left_join(admin1_info$data[,c('admin1.name','admin1.num')],by='admin1.num')
poly.adm1.grid <- poly.adm1.grid%>%mutate(code=admin1.name) %>%
  select(row,col,code,name)

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
######### load admin-1 estimates for UYS
################################################################

### load unadjusted direct estimates
setwd(paste0(res.dir,'/Direct_unadj/'))

adm1.U.direct.unadj.agegrp <- readRDS('adm1_agegrp_direct_U.rds')
adm1.R.direct.unadj.agegrp<- readRDS('adm1_agegrp_direct_R.rds')
adm1.overall.direct.unadj.agegrp <- readRDS('adm1_agegrp_direct_overall.rds')

# harmonize column names
adm1.U.direct.unadj.agegrp.toplot <- adm1.U.direct.unadj.agegrp$educ_years
adm1.U.direct.unadj.agegrp.toplot$method <- 'Naive Weighted'
adm1.U.direct.unadj.agegrp.toplot <- adm1.U.direct.unadj.agegrp.toplot %>% rename('birth.year' = 'group')

adm1.R.direct.unadj.agegrp.toplot <- adm1.R.direct.unadj.agegrp$educ_years
adm1.R.direct.unadj.agegrp.toplot$method <- 'Naive Weighted'
adm1.R.direct.unadj.agegrp.toplot <- adm1.R.direct.unadj.agegrp.toplot %>% rename('birth.year' = 'group')

adm1.overall.direct.unadj.agegrp.toplot <- adm1.overall.direct.unadj.agegrp$educ_years
adm1.overall.direct.unadj.agegrp.toplot$method <- 'Naive Weighted'
adm1.overall.direct.unadj.agegrp.toplot <- adm1.overall.direct.unadj.agegrp.toplot %>% rename('birth.year' = 'group')


### load modifiedusted direct estimates
setwd(paste0(res.dir,'/Direct_modified/'))

adm1.U.direct.modified.agegrp <- readRDS('adm1_agegrp_direct_U.rds')
adm1.R.direct.modified.agegrp<- readRDS('adm1_agegrp_direct_R.rds')
adm1.overall.direct.modified.agegrp <- readRDS('adm1_agegrp_direct_overall.rds')

# harmonize column names
adm1.U.direct.modified.agegrp.toplot <- adm1.U.direct.modified.agegrp$educ_years
adm1.U.direct.modified.agegrp.toplot$method <- 'Modified Weighted'
adm1.U.direct.modified.agegrp.toplot <- adm1.U.direct.modified.agegrp.toplot %>% rename('birth.year' = 'group')

adm1.R.direct.modified.agegrp.toplot <- adm1.R.direct.modified.agegrp$educ_years
adm1.R.direct.modified.agegrp.toplot$method <- 'Modified Weighted'
adm1.R.direct.modified.agegrp.toplot <- adm1.R.direct.modified.agegrp.toplot %>% rename('birth.year' = 'group')

adm1.overall.direct.modified.agegrp.toplot <- adm1.overall.direct.modified.agegrp$educ_years
adm1.overall.direct.modified.agegrp.toplot$method <- 'Modified Weighted'
adm1.overall.direct.modified.agegrp.toplot <- adm1.overall.direct.modified.agegrp.toplot %>% rename('birth.year' = 'group')



### load GLM estimates 
setwd(paste0(res.dir,'/GLM/'))

adm1.overall.adj.GLM.yearly <- readRDS('adm1_yearly_GLM_overall.rds')
adm1.U.adj.GLM.yearly <- readRDS('adm1_yearly_GLM_U.rds')
adm1.R.adj.GLM.yearly <- readRDS('adm1_yearly_GLM_R.rds')

# harmonize column names
adm1.U.adj.GLM.yearly.toplot <- adm1.U.adj.GLM.yearly$educ_years
adm1.U.adj.GLM.yearly.toplot$method <- 'Survey GLM'
adm1.R.adj.GLM.yearly.toplot <- adm1.R.adj.GLM.yearly$educ_years
adm1.R.adj.GLM.yearly.toplot$method <- 'Survey GLM'
adm1.overall.adj.GLM.yearly.toplot <- adm1.overall.adj.GLM.yearly$educ_years
adm1.overall.adj.GLM.yearly.toplot$method <- 'Survey GLM'

### load INLA estimates
setwd(paste0(res.dir,'/INLA/Admin1'))

# adm1.overall.adj.INLA.yearly <- readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_overall_res.rds'))
# adm1.U.adj.INLA.yearly <-  readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_U_res.rds'))
# adm1.R.adj.INLA.yearly <-   readRDS(paste0('adm1_INLA_fixed_RW',rw.order,'_R_res.rds'))

adm1.overall.adj.INLA.yearly <- readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_overall_res.rds'))
adm1.U.adj.INLA.yearly <-  readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_U_res.rds'))
adm1.R.adj.INLA.yearly <-   readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_R_res.rds'))

# harmonize column names
adm1.U.adj.INLA.yearly.toplot <- adm1.U.adj.INLA.yearly$educ_years
adm1.U.adj.INLA.yearly.toplot <- adm1.U.adj.INLA.yearly.toplot[,c('birth.year','lower' , 'mean' ,'upper','admin1.char')]
#colnames(adm1.U.adj.INLA.yearly.toplot) <- c('group','lower' , 'mean' ,'upper','admin1.char')
adm1.U.adj.INLA.yearly.toplot$sd <- NA
adm1.U.adj.INLA.yearly.toplot$method <- 'Spatial Model'

adm1.R.adj.INLA.yearly.toplot <- adm1.R.adj.INLA.yearly$educ_years
adm1.R.adj.INLA.yearly.toplot <- adm1.R.adj.INLA.yearly.toplot[,c('birth.year','lower' , 'mean' ,'upper','admin1.char')]
#colnames(adm1.R.adj.INLA.yearly.toplot) <- c('group','lower' , 'mean' ,'upper','admin1.char')
adm1.R.adj.INLA.yearly.toplot$sd <- NA
adm1.R.adj.INLA.yearly.toplot$method <- 'Spatial Model'

adm1.overall.adj.INLA.yearly.toplot <- adm1.overall.adj.INLA.yearly$educ_years
adm1.overall.adj.INLA.yearly.toplot <- adm1.overall.adj.INLA.yearly.toplot[,c('birth.year','lower' , 'mean' ,'upper','admin1.char')]
#colnames(adm1.overall.adj.INLA.yearly.toplot) <- c('group','lower' , 'mean' ,'upper','admin1.char')
adm1.overall.adj.INLA.yearly.toplot$sd <- NA
adm1.overall.adj.INLA.yearly.toplot$method <- 'Spatial Model'



###########################################################################
######### visualize admin-1 UYS by birth cohort, all methods 
###########################################################################

### overall estimates 
adm1.overall.toplot <- rbind(adm1.overall.direct.unadj.agegrp.toplot,
#                             adm1.overall.direct.modified.agegrp.toplot,
                             adm1.overall.adj.GLM.yearly.toplot,
                             adm1.overall.adj.INLA.yearly.toplot)

colnames(adm1.overall.toplot) <-c("est","Lower_CI","Upper_CI","sd" , "year",'admin1.char', "method")
adm1.overall.toplot$year <- as.numeric(adm1.overall.toplot$year)

adm1.overall.toplot <- adm1.overall.toplot %>% left_join(admin1_info$data[,c('admin1.char','admin1.name')],by='admin1.char')

setwd(paste0(res.dir,'/Visualization/Admin1'))


custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 


g.adm1.overall <- ggplot(aes(x = year, group = method), data = adm1.overall.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.2, position = position_dodge(width = 0.1)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.1), linewidth = 0.42) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  theme_bw()+
  ylab('Ultimate Years of Schooling') +
  #ggtitle('Overall Admin-1: Direct, Survey GLM, and INLA Estimates') +
  facet_wrap(~admin1.name, ncol = 5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_y_continuous(breaks = c(3,6,9,12)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 15),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    #strip.text.x = element_text(size = 14),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 15),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

  
### save plot
setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("overall_adm1_UYS_yearly_compare.pdf"))){
    
    setwd(paste0(res.dir,'/Visualization/Admin1'))
    ggsave(g.adm1.overall, width=9, height = 11,#ceiling(dim(admin1_info$data)[1]/3)*3+4,
           file = paste0("overall_adm1_UYS_yearly_compare.pdf"))
}


### urban estimates 
adm1.U.toplot <- rbind(adm1.U.direct.unadj.agegrp.toplot,
#                       adm1.U.direct.modified.agegrp.toplot,
                       adm1.U.adj.GLM.yearly.toplot,
                       adm1.U.adj.INLA.yearly.toplot)


colnames(adm1.U.toplot) <-c("est","Lower_CI","Upper_CI","sd" , "year",'admin1.char', "method")
adm1.U.toplot$year <- as.numeric(adm1.U.toplot$year)

adm1.U.toplot <- adm1.U.toplot %>% left_join(admin1_info$data[,c('admin1.char','admin1.name')],by='admin1.char')

custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 

g.adm1.U <- ggplot(aes(x = year, group = method), data = adm1.U.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.2, position = position_dodge(width = 0.1)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.1), linewidth = 0.42) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  theme_bw()+
  ylab('Ultimate Years of Schooling') +
  #ggtitle('Overall Admin-1: Direct, Survey GLM, and INLA Estimates') +
  facet_wrap(~admin1.name, ncol = 5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_y_continuous(breaks = c(3,6,9,12)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 15),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    #strip.text.x = element_text(size = 14),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 15),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )
 
### save plot 
setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("urban_adm1_UYS_yearly_compare.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.U, width=9, height = 11, #ceiling(dim(admin1_info$data)[1]/3)*3+4,
         file = paste0("urban_adm1_UYS_yearly_compare.pdf"))
  
}

### rural estimates 
adm1.R.toplot <- rbind(adm1.R.direct.unadj.agegrp.toplot,
#                       adm1.R.direct.modified.agegrp.toplot,
                       adm1.R.adj.GLM.yearly.toplot,
                       adm1.R.adj.INLA.yearly.toplot)


colnames(adm1.R.toplot) <-c("est","Lower_CI","Upper_CI","sd" , "year",'admin1.char', "method")
adm1.R.toplot$year <- as.numeric(adm1.R.toplot$year)

adm1.R.toplot <- adm1.R.toplot %>% left_join(admin1_info$data[,c('admin1.char','admin1.name')],by='admin1.char')
adm1.R.toplot <- adm1.R.toplot[!adm1.R.toplot$admin1.name=='Dar es Salaam',]


custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 

g.adm1.R <- ggplot(aes(x = year, group = method), data = adm1.R.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.2, position = position_dodge(width = 0.1)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.1), linewidth = 0.42) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  theme_bw()+
  ylab('Ultimate Years of Schooling') +
  #ggtitle('Overall Admin-1: Direct, Survey GLM, and INLA Estimates') +
  facet_wrap(~admin1.name, ncol = 5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  scale_y_continuous(breaks = c(3,6,9,12)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 15),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    #strip.text.x = element_text(size = 14),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 15),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


### save plot
setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("rural_adm1_UYS_yearly_compare.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.R, width=9, height =11,# ceiling(dim(admin1_info$data)[1]/3)*3+4,
         file = paste0("rural_adm1_UYS_yearly_compare.pdf"))
  
}

###########################################################################
######### compare UYS by age group all methods, scatter plot
###########################################################################


### Load INLA estimates
setwd(paste0(res.dir, '/INLA/Admin1'))
adm1.overall.INLA.5yr.res <- readRDS(paste0('adm1_INLA_BYM2_RW', rw.order, '_typeIV', '_overall_5yr_res.rds'))
adm1.overall.INLA.5yr.UYS <- adm1.overall.INLA.5yr.res$educ_years


adm1.overall.direct.unadj.5yr.UYS <- adm1.overall.direct.unadj.agegrp.toplot
adm1.overall.direct.unadj.5yr.UYS$agegrp_start <- (survey_year-adm1.overall.direct.unadj.5yr.UYS$birth.year)-2
adm1.overall.direct.unadj.5yr.UYS <- adm1.overall.direct.unadj.5yr.UYS %>%
  mutate(agegrp = paste0(agegrp_start,'-',agegrp_start+4))
adm1.overall.direct.unadj.5yr.UYS$direct_est <- adm1.overall.direct.unadj.5yr.UYS$mean

### overall estimates 
adm1.UYS.agegrp.compare.INLA.direct <- adm1.overall.INLA.5yr.UYS %>%
  left_join(adm1.overall.direct.unadj.5yr.UYS[,c('direct_est','admin1.char','agegrp')],
            by=c('agegrp','admin1.char'))


g.compare.5yr.INLA.direct <- ggplot(adm1.UYS.agegrp.compare.INLA.direct, 
                    aes(x = direct_est, y = mean, label = admin1.name, group = agegrp)) +
  geom_point(size = 1.8, alpha = 0.8, color = "#377EB8") +  # Muted blue color
  facet_wrap(~agegrp, ncol = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + # Gray reference line
  scale_x_continuous(limits = c(2.4, 11.5)) +  # Fix x-axis from 3 to 11
  scale_y_continuous(limits = c(2.4, 11.5)) +  # Fix y-axis from 3 to 11
  labs(x = "UYS: Naive Weighted", y = "UYS: Spatial Model") +
  theme_bw()+
  theme(
    aspect.ratio = 1,  # Ensure square facets
    panel.background = element_blank(),  # Remove background color
    panel.grid.major = element_line(color = "gray90"),  # Subtle major grid lines
    panel.grid.minor = element_line(color = "gray95"),  # Even lighter minor grid lines
    axis.text = element_text(size = 14),  # Larger axis labels
    axis.title = element_text(size = 18),  # Larger axis titles
    strip.text = element_text(size = 16),  # Facet panel text
    plot.background = element_blank(),  # Remove overall background
    plot.title = element_text(size = 20, hjust = 0.5)  # Centered & bold title
  )

### save plot
setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("overall_5yr_scatter_compare_INLA_direct.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.compare.5yr.INLA.direct, width=10, height = 12,
         file = paste0("overall_5yr_scatter_compare_INLA_direct.pdf"))
  
}

###########################################################################
######### visualize admin-1 UYS, comparing methods, geo_facet
###########################################################################

setwd(paste0(res.dir,'/Visualization/Admin1'))

custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 

g.adm1.overall.geofacet <- ggplot(aes(x = year, group = method), data = adm1.overall.toplot) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = method, group = method),
              alpha = 0.2, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = est, color = method, group = method),
            position = position_dodge(width = 0.05), linewidth = 0.42) +
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Birth Year') +
  ylab('Ultimate Years of Schooling') +
  theme_bw()+
  #ggtitle('Overall Admin-1: Direct, Survey GLM, and INLA Estimates') +
  facet_geo(~ admin1.name, grid = poly.adm1.grid) +  # Apply geofacet layout
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1975, 2005, by = 10)) +
  scale_y_continuous(breaks = seq(3, 12, by = 3)) +
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 16),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    #strip.text.x = element_text(size = 14),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )



setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists(paste0("overall_adm1_UYS_yearly_geofacet.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.overall.geofacet, width=11, height = 10,
         file = paste0("overall_adm1_UYS_yearly_geofacet.pdf"))
}


################################################################
### Education level distribution, INLA, geo_facet
################################################################

setwd(paste0(res.dir))


if(!dir.exists(paths = paste0('Visualization/Admin1'))){ 
  dir.create(file.path('Visualization/Admin1'))
}

for(tmp_agegrp in paste0(c(1:7)*5+10,'-',c(1:7)*5+14)){
  if(!dir.exists(paths = paste0('Visualization/Admin1/',tmp_agegrp))){ 
    dir.create(file.path('Visualization/Admin1/',tmp_agegrp))
  }
}

### Load INLA estimates
setwd(paste0(res.dir, '/INLA/Admin1'))

adm1.overall.adj.INLA.5yr.draws <- readRDS(paste0('adm1_INLA_BYM2_RW', rw.order, '_typeIV', '_overall_5yr_est_draws.rds'))
adm1.overall.INLA.5yr.educ.levels.draws <- aggre.Educ.levels(post.draws=adm1.overall.adj.INLA.5yr.draws,
                                                                  educ_level_cutoffs = c(0,1,7,8,12,14))
adm1.overall.INLA.5yr.educ.levels.res <-summarize.Educ.INLA.postsamps(post.draws = adm1.overall.INLA.5yr.educ.levels.draws,
                                                                      est.grid = adm1.overall.INLA.5yr.educ.levels.draws$est.grid,
                                                                      adm.names = adm.names,
                                                                      subnational =T,
                                                                      nSamp=1000)
  

for(tmp_agegrp in paste0(c(1:7)*5+10,'-',c(1:7)*5+14)){
  
  # tmp_agegrp <- '15-19'
  # Harmonize column names
  adm1.overall.INLA.educ.levels.toplot <- adm1.overall.INLA.5yr.educ.levels.res$grade_dist_p
  adm1.overall.INLA.educ.levels.toplot <- adm1.overall.INLA.educ.levels.toplot %>% filter(agegrp == tmp_agegrp)
  adm1.overall.INLA.educ.levels.toplot <- adm1.overall.INLA.educ.levels.toplot[, c('grade', 'mean', 'admin1.name', 'lower', 'upper')]
  
  # Group data into broader education categories and compute aggregated CIs
  adm1.overall.INLA.grade.cat.toplot <- adm1.overall.INLA.educ.levels.toplot %>%
    mutate(education = case_when(
      grade == 0 ~ 'No Education',
      grade < 7 ~ 'Some Primary',
      grade == 7 ~ 'Completed Primary',
      grade == 8 ~ 'Lower Secondary',
      grade == 12 ~ 'Upper Secondary',
      TRUE ~ 'More than Secondary'
    )) %>%
    mutate(education = factor(education, levels = rev(c(
      "No Education",
      "Some Primary",
      "Completed Primary",
      "Lower Secondary",
      "Upper Secondary",
      "More than Secondary")
    )))
  
  # Plot with confidence intervals
  g.adm1.agegrp.grade.dist <- ggplot(adm1.overall.INLA.grade.cat.toplot, aes(x = education, y = mean, fill = education)) +
    geom_col() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Add CIs
    facet_geo(~ admin1.name, grid = poly.adm1.grid) +  # Apply geofacet layout
    coord_flip() +
    labs(x = "Education Level", y = "Probability") + #, title = paste0("Education Level by Admin-1 Regions (Age Group ", tmp_agegrp, ')')
    #theme_minimal() +
    scale_fill_manual(
      values = c(
        "No Education" = "#D73027",  # Red
        "Some Primary" = "#FC8D59",  # Orange
        "Completed Primary" = "#FFD700",  # Yellow
        "Lower Secondary" = "#4DAF4A",  # **Bright Green**
        "Upper Secondary" = "#1F78B4",  # **Strong Vivid Blue (Highlighted)**
        "More than Secondary" = "#984EA3"  # **Rich Purple**
      )
    ) +
    theme_bw()+
    theme(
      legend.position = 'bottom',
      legend.key.height = unit(0.5, 'cm'),
      legend.text = element_text(size = 16),
      legend.key.width = unit(2, 'cm'),
      #legend.title = element_text(size = 15),
      #strip.text.x = element_text(size = 14),
      #strip.text.y = element_text(size = 14),
      text = element_text(size = 16),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      #axis.text.y = element_text(size = 13),
      #plot.title = element_text(size = 16),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
      panel.grid.minor = element_blank()  # Remove minor grid lines
    )+
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(legend.position = "none")  # Hide legend if not needed
  
  setwd(paste0(res.dir,'/Visualization/Admin1/',tmp_agegrp))
  if(!file.exists(paste0("overall_adm1_level_dist_geofacet_",tmp_agegrp ,".pdf"))){
    
    setwd(paste0(res.dir,'/Visualization/Admin1/',tmp_agegrp))
    ggsave(g.adm1.agegrp.grade.dist, width=13, height = 10,
           file = paste0("overall_adm1_level_dist_geofacet_",tmp_agegrp,".pdf"))
  }
  
  
}











### Load INLA estimates
setwd(paste0(res.dir, '/INLA/Admin1'))

adm1.overall.adj.INLA.5yr.draws <- readRDS(paste0('adm1_INLA_BYM2_RW', rw.order, '_typeIV', '_overall_5yr_est_draws.rds'))
adm1.overall.INLA.5yr.educ.levels.draws <- aggre.Educ.levels(post.draws=adm1.overall.adj.INLA.5yr.draws,
                                                             educ_level_cutoffs = c(0,1,7,8,12,14))
adm1.overall.INLA.5yr.educ.levels.res <- summarize.Educ.INLA.postsamps(post.draws = adm1.overall.INLA.5yr.educ.levels.draws,
                                                                       est.grid = adm1.overall.INLA.5yr.educ.levels.draws$est.grid,
                                                                       adm.names = adm.names,
                                                                       subnational =T,
                                                                       nSamp=1000)

# Harmonize column names
adm1.overall.INLA.educ.levels.toplot <- adm1.overall.INLA.5yr.educ.levels.res$grade_dist_p
adm1.overall.INLA.educ.levels.toplot <- adm1.overall.INLA.educ.levels.toplot[, c('grade', 'mean', 'admin1.name', 'agegrp', 'lower', 'upper')]

# Group data into broader education categories and compute aggregated CIs
adm1.overall.INLA.grade.cat.toplot <- adm1.overall.INLA.educ.levels.toplot %>%
  mutate(education = case_when(
    grade == 0 ~ 'No Education',
    grade < 7 ~ 'Some Primary',
    grade == 7 ~ 'Completed Primary',
    grade == 8 ~ 'Lower Secondary',
    grade == 12 ~ 'Upper Secondary',
    TRUE ~ 'More than Secondary'
  )) %>%
  mutate(
    education = factor(education, levels = rev(c(
      "No Education",
      "Some Primary",
      "Completed Primary",
      "Lower Secondary",
      "Upper Secondary",
      "More than Secondary"
    ))),
    agegrp = factor(agegrp, levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) # Ensure proper ordering
  )

# Stacked bar plot by age group within each region
g.adm1.all.agegrp.grade.dist <- ggplot(adm1.overall.INLA.grade.cat.toplot, 
                                       aes(x = agegrp, y = mean, fill = education)) +
  geom_col(position = "stack", width = 0.85) +
  facet_geo(~ admin1.name, grid = poly.adm1.grid) +  # Apply geofacet layout to show regions
  labs(x = "Age Group", y = NULL, fill = "Education Level") + 
  theme_minimal(base_size = 18) +
  coord_flip() +
  scale_y_continuous(breaks = NULL) +  # Remove y-axis breaks
  scale_fill_manual(
    values = c(
      "No Education" = "#D73027",  # Red
      "Some Primary" = "#FC8D59",  # Orange
      "Completed Primary" = "#FFD700",  # Yellow
      "Lower Secondary" = "#4DAF4A",  # Bright Green
      "Upper Secondary" = "#1F78B4",  # Strong Blue
      "More than Secondary" = "#984EA3"  # Rich Purple
    ),
    breaks = rev(levels(adm1.overall.INLA.grade.cat.toplot$education))  # Reverse legend order
  ) +
  theme(
    legend.position = "bottom",  # Move legend to bottom
    legend.direction = "horizontal",  # Ensure legend items are in a row
    legend.title = element_text(size = 18),  # Customize legend title size
    axis.text.y = element_text(size = 12)  # Customize y-axis label size
  ) +
  guides(fill = guide_legend(nrow = 2))  # Force a single-row legend

setwd(paste0(res.dir,'/Visualization/Admin1'))

#if(!file.exists( paste0('overall_adm1_educ_level_dist_geofacet_all_age_group' ,".pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.all.agegrp.grade.dist, width=12.2, height = 10,
         file = paste0('overall_adm1_educ_level_dist_geofacet_all_age_group' ,".pdf"))
#}






################################################################
### Survival curve, INLA, geo_facet
################################################################

### load INLA estimates
setwd(paste0(res.dir,'/INLA/Admin1'))
adm1.overall.INLA.5yr <- readRDS(paste0('adm1_INLA_BYM2_RW',rw.order,'_typeIV','_overall_5yr_res.rds'))

adm1.overall.INLA.5yr.surv <- adm1.overall.INLA.5yr$surv_p
adm1.overall.INLA.5yr.surv <- adm1.overall.INLA.5yr.surv %>% filter(grade<19)

### plot
#custom_palette_agegrp <- c(brewer.pal(7, "Set1"))

g.adm1.overall.surv.geofacet <- ggplot(aes(x = grade, group = agegrp), data = adm1.overall.INLA.5yr.surv) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = agegrp, group = agegrp),
              alpha = 0.2, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = mean, color = agegrp, group = agegrp),
            position = position_dodge(width = 0.05), linewidth = 0.42) +
  #geom_point(aes(y = mean, color = agegrp, group = agegrp),
  #           position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Grade') +
  ylab('Probability of Completing Grade') +
  #ggtitle('Overall Admin-1: Direct, Survey GLM, and INLA Estimates') +
  facet_geo(~ admin1.name, grid = poly.adm1.grid) +  # Apply geofacet layout
  scale_x_continuous(breaks = c(1,7,11,13,18)) +
  labs(fill = "Age Group", color = "Age Group") +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  #scale_fill_manual(values = custom_palette_agegrp, name = "Age Group") +
  #scale_color_manual(values = custom_palette_agegrp, name = "Age Group") +
  theme_bw()+
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 16),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    #strip.text.x = element_text(size = 14),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  guides(fill = guide_legend(nrow = 2))  # Force a single-row legend


setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists( paste0('overall_adm1_grade_surv_geofacet_all_age_group' ,".pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.overall.surv.geofacet, width=11, height = 10,
         file = paste0('overall_adm1_grade_surv_geofacet_all_age_group' ,".pdf"))
}







################################################################
### Highest level attended, INLA, geo_facet
################################################################

### load INLA estimates
setwd(paste0(res.dir, '/INLA/Admin1'))

adm1.overall.adj.INLA.yearly.draws <- readRDS(paste0('adm1_INLA_BYM2_RW', rw.order, '_typeIV', '_overall_est_draws.rds'))
adm1.overall.INLA.yearly.high.levels.draws <- aggre.Educ.levels(post.draws=adm1.overall.adj.INLA.yearly.draws,
                                                             educ_level_cutoffs = c(0,1,8,12,14))
adm1.overall.INLA.yearly.high.levels.res <- summarize.Educ.INLA.postsamps(post.draws = adm1.overall.INLA.yearly.high.levels.draws,
                                                                       est.grid = adm1.overall.INLA.yearly.high.levels.draws$est.grid,
                                                                       adm.names = adm.names,
                                                                       subnational =T,
                                                                       nSamp=1000)

adm1.overall.INLA.yearly.high.level.surv <- adm1.overall.INLA.yearly.high.levels.res$surv_p
adm1.overall.INLA.yearly.high.level.surv <- adm1.overall.INLA.yearly.high.level.surv %>%
  mutate(education = case_when(
    grade == 1 ~ 'Primary or Higher',
    grade == 8 ~ 'Lower Secondary or Higher',
    grade == 12 ~ 'Upper Secondary or Higher',
    TRUE ~ 'More than Secondary'
  ))    %>% mutate(education = factor(education, levels = (c(
    "Primary or Higher",
    "Lower Secondary or Higher",
    "Upper Secondary or Higher",
    "More than Secondary")
  )))


### plot
g.adm1.overall.level.surv.geofacet <- ggplot(aes(x = birth.year, group = education), data = adm1.overall.INLA.yearly.high.level.surv) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = education, group = education),
              alpha = 0.2, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = mean, color = education, group = education),
            position = position_dodge(width = 0.05), linewidth = 0.42) +
  xlab('Birth Year') +
  ylab('Probability of Attaining Education Level') +
  facet_geo(~ admin1.name, grid = poly.adm1.grid) +  # Apply geofacet layout
  labs(fill = "Education Level", color = "Education Level") +  # Correct legend title
  scale_x_continuous(breaks = seq(1975, 2005, by = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()+
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 16),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    #strip.text.x = element_text(size = 14),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))  # Ensure single-row legend

setwd(paste0(res.dir,'/Visualization/Admin1'))

if(!file.exists( paste0('overall_adm1_educ_level_surv_geofacet_yearly' ,".pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.overall.level.surv.geofacet, width=11, height = 10,
         file = paste0('overall_adm1_educ_level_surv_geofacet_yearly' ,".pdf"))
}



