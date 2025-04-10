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
library(patchwork)

library(egg)
library(gridExtra)
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

### original data 

setwd(paste0(data.dir,'/prepared_dat'))

educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))


### !!! modify years of education to education groups 
educ_dat_ind <- educ_dat_ind %>%
  mutate(educ.yrs= case_when(educ.yrs==0 ~ 0,   # no education
                             educ.yrs<7 ~ 1,    # some primary
                             educ.yrs<8 ~ 2,    # completed primary 
                             educ.yrs<12~ 3,    # lower secondary (some or completed)
                             educ.yrs<14~ 4,    # upper secondary (some or completed)
                             educ.yrs>13 ~ 5,   # tertiary
                             TRUE ~NA)) 


################################################################
######### prepare analysis data set
################################################################

modified_ind = T

if(modified_ind){
  account.censor.ind=T
  folder.suffix='modified'
}else{
  account.censor.ind = F
  folder.suffix = 'unadj'
}

### overall
educ.splitted.overall <- getEduc_splitted(data = educ_dat_ind,
                                          compact = T, 
                                          account.censor=account.censor.ind,
                                          compact.by =  c('v001','v022', "weights","agegrp"))

educ.splitted.overall$strata <- educ.splitted.overall$v022

### urban
educ.splitted.U <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='urban',],
                                    compact = T, 
                                    account.censor=account.censor.ind,
                                    compact.by =  c('v001','v022', "weights","agegrp"))

educ.splitted.U$strata <- educ.splitted.U$v022


### rural
educ.splitted.R <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='rural',],
                                    compact = T, 
                                    account.censor=account.censor.ind,
                                    compact.by =  c('v001','v022', "weights","agegrp"))

educ.splitted.R$strata <- educ.splitted.R$v022


################################################################
######### national
################################################################

natl.overall.direct.year.group <- getEduc.Direct.group(splitted.educ= educ.splitted.overall,
                                                        group.var='agegrp')

natl.U.direct.year.group <- getEduc.Direct.group(splitted.educ= educ.splitted.U,
                                                       group.var='agegrp')

natl.R.direct.year.group <- getEduc.Direct.group(splitted.educ= educ.splitted.R,
                                                       group.var='agegrp')

################################################################
######### plot natl proportional odds
################################################################

grade_labels <- c("0" = "No Education", "1" = "Some Primary", "2" = "Completed Primary", 
                  "3" = "Lower Secondary", "4" = "Higher Secondary", "5" = "More than Secondary")
### overall 

natl.overall.direct.cont.odds <- natl.overall.direct.year.group$cond_surv_p

natl.overall.direct.cont.odds <- natl.overall.direct.cont.odds %>% 
  mutate(logit.mean = logit(mean))%>% 
  mutate(logit.lower = logit(lower))%>% 
  mutate(logit.upper = logit(upper)) %>%
  filter(abs(logit.mean)<8)%>%
  mutate(agegrp = group)%>%
  mutate(logit.mean=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.mean))%>%
  mutate(logit.lower=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.lower))%>%
  mutate(logit.upper=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.upper))
  
# g.natl.overall.prop <- ggplot(aes(x = factor(grade_int), group = agegrp), data = natl.overall.direct.cont.odds) +
#   geom_line(aes(y = logit.mean, color = agegrp, group = agegrp),
#             position = position_dodge(width = 0.01), linewidth = 0.42) +
#   geom_ribbon(aes(ymin = logit.lower, ymax = logit.upper, fill = agegrp, group = agegrp),
#               alpha = 0.2, position = position_dodge(width = 0.01)) +
#     scale_x_discrete(labels = grade_labels) +
#   xlab('Education Level') +
#   ylab('Log-Odds of Conditional Dropout Probability')+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()+
#   theme(
#     legend.position = 'bottom',
#     legend.key.height = unit(0.5, 'cm'),
#     legend.text = element_text(size = 15),
#     legend.key.width = unit(2, 'cm'),
#     #legend.title = element_text(size = 15),
#     #strip.text.x = element_text(size = 14),
#     #strip.text.y = element_text(size = 14),
#     text = element_text(size = 15),
#     #axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     #axis.text.y = element_text(size = 13),
#     #plot.title = element_text(size = 16),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
#     panel.grid.minor = element_blank()  # Remove minor grid lines
#   )





### Urban

natl.U.direct.cont.odds <- natl.U.direct.year.group$cond_surv_p

natl.U.direct.cont.odds <- natl.U.direct.cont.odds %>% 
  mutate(logit.mean = logit(mean))%>% 
  mutate(logit.lower = logit(lower))%>% 
  mutate(logit.upper = logit(upper)) %>%
  filter(abs(logit.mean)<8)%>%
  mutate(agegrp = group)%>%
  mutate(logit.mean=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.mean))%>%
  mutate(logit.lower=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.lower))%>%
  mutate(logit.upper=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.upper))

# g.natl.U.prop <- ggplot(aes(x = factor(grade_int), group = agegrp), data = natl.U.direct.cont.odds) +
#   geom_line(aes(y = logit.mean, color = agegrp, group = agegrp),
#             position = position_dodge(width = 0.01), linewidth = 0.42) +
#   geom_ribbon(aes(ymin = logit.lower, ymax = logit.upper, fill = agegrp, group = agegrp),
#               alpha = 0.2, position = position_dodge(width = 0.01)) +
#   scale_x_discrete(labels = grade_labels) +
#   xlab('Education Level') +
#   ylab('Log-Odds of Conditional Dropout Probability')+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()+
#   theme(
#     legend.position = 'bottom',
#     legend.key.height = unit(0.5, 'cm'),
#     legend.text = element_text(size = 15),
#     legend.key.width = unit(2, 'cm'),
#     #legend.title = element_text(size = 15),
#     #strip.text.x = element_text(size = 14),
#     #strip.text.y = element_text(size = 14),
#     text = element_text(size = 15),
#     #axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     #axis.text.y = element_text(size = 13),
#     #plot.title = element_text(size = 16),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
#     panel.grid.minor = element_blank()  # Remove minor grid lines
#   )




### Rural

natl.R.direct.cont.odds <- natl.R.direct.year.group$cond_surv_p

natl.R.direct.cont.odds <- natl.R.direct.cont.odds %>% 
  mutate(logit.mean = logit(mean))%>% 
  mutate(logit.lower = logit(lower))%>% 
  mutate(logit.upper = logit(upper)) %>%
  filter(abs(logit.mean)<8)%>%
  mutate(agegrp = group)%>%
  mutate(logit.mean=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.mean))%>%
  mutate(logit.lower=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.lower))%>%
  mutate(logit.upper=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.upper))


# g.natl.R.prop <- ggplot(aes(x = factor(grade_int), group = agegrp), data = natl.R.direct.cont.odds) +
#   geom_line(aes(y = logit.mean, color = agegrp, group = agegrp),
#             position = position_dodge(width = 0.01), linewidth = 0.42) +
#   geom_ribbon(aes(ymin = logit.lower, ymax = logit.upper, fill = agegrp, group = agegrp),
#               alpha = 0.2, position = position_dodge(width = 0.01)) +
#   scale_x_discrete(labels = grade_labels) +
#   xlab('Education Level') +
#   ylab('Log-Odds of Conditional Dropout Probability')+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()+
#   theme(
#     legend.position = 'bottom',
#     legend.key.height = unit(0.5, 'cm'),
#     legend.text = element_text(size = 15),
#     legend.key.width = unit(2, 'cm'),
#     #legend.title = element_text(size = 15),
#     #strip.text.x = element_text(size = 14),
#     #strip.text.y = element_text(size = 14),
#     text = element_text(size = 15),
#     #axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     #axis.text.y = element_text(size = 13),
#     #plot.title = element_text(size = 16),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
#     panel.grid.minor = element_blank()  # Remove minor grid lines
#   )



### plot three together

natl.R.direct.cont.odds <- natl.R.direct.cont.odds %>% mutate(type='Rural')
natl.U.direct.cont.odds <- natl.U.direct.cont.odds %>% mutate(type='Urban')
natl.overall.direct.cont.odds <- natl.overall.direct.cont.odds %>% mutate(type='Overall')

natl.direct.all.cont.odds <- rbind(natl.U.direct.cont.odds,natl.R.direct.cont.odds,natl.overall.direct.cont.odds)
natl.direct.all.cont.odds <- natl.direct.all.cont.odds %>%
  mutate(type=factor(type,levels=c('Urban','Rural','Overall')))

grade_labels <- c(
  "0" = "No Educ.",
  "1" = "Some Prim.",
  "2" = "Comp. Prim.",
  "3" = "Lower Sec.",
  "4" = "Higher Sec.",
  "5" = "Post-Sec."
)

y_limits <- c(min(natl.direct.all.cont.odds$logit.lower,na.rm=T),max(natl.direct.all.cont.odds$logit.upper,na.rm=T))

g.natl.all.three.prop <- ggplot(aes(x = factor(grade_int), group = agegrp), data = natl.direct.all.cont.odds) +
  geom_line(aes(y = logit.mean, color = agegrp, group = agegrp),
            position = position_dodge(width = 0.01), linewidth = 0.42) +
  geom_ribbon(aes(ymin = logit.lower, ymax = logit.upper, fill = agegrp, group = agegrp),
              alpha = 0.2, position = position_dodge(width = 0.01)) +
  scale_x_discrete(labels = grade_labels) +
  scale_y_continuous(limits = y_limits) +
  facet_wrap(~ type, scales = "fixed",ncol=2) +  # Facet by age group with fixed scales
  xlab('Education Level') +
  ylab('Log-Odds of Conditional Dropout Probability')+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Age Group", color = "Age Group") +  # Correct legend title
  theme_bw()+
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 15),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    strip.text.x = element_text(size = 16),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 15),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

part1 <- g.natl.all.three.prop %+% subset(natl.direct.all.cont.odds, type != 'Overall') + labs(x = NULL)+ theme(legend.position = "none") 
part2 <- g.natl.all.three.prop %+% subset(natl.direct.all.cont.odds, type == 'Overall')

g.natl.all.three.prop.formatted <- grid.arrange(grobs = lapply(
  list(part1, part2),
  set_panel_size,
  width = unit(14, "cm"),
  height = unit(8, "cm")
))

setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("natl_educ_level_proportional_check.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.natl.all.three.prop.formatted, width=13, height = 11.2,#ceiling(dim(admin1_info$data)[1]/3)*3+4,
         file = paste0("natl_educ_level_proportional_check.pdf"))
}

################################################################
######### subnational
################################################################

educ.splitted.admin1.overall <- merge_adm_info(data = educ.splitted.overall,
                                               adm1.info = admin1_info$data,
                                               adm2.info = admin2_info$data,
                                               cluster.info = cluster.info)


### loop over regions 
region.list <- unique(educ.splitted.admin1.overall$admin1.char)

adm1.educ.years <- data.frame()
adm1.survival.p <- data.frame()
adm1.cond.survival.p <- data.frame()
adm1.grade.dist.p <- data.frame()

for(tmp.region in region.list ){
  
  tmp.educ <- educ.splitted.admin1.overall[educ.splitted.admin1.overall$admin1.char ==tmp.region,]
  
  ### remove years with too few observations 
  tmp.educ <- tmp.educ %>% 
    group_by(agegrp) %>% 
    filter(n_distinct(v001) >= 2)
  
  tmp.educ <- as.data.frame(tmp.educ)
  
  tmp.res <- getEduc.Direct.group(splitted.educ= tmp.educ,
                                  group.var = 'agegrp')
  
  tmp.res.educ <- tmp.res$educ_years
  tmp.res.surv.p <- tmp.res$surv_p
  tmp.res.cond.p <- tmp.res$cond_surv_p
  tmp.res.grade.dist.p <- tmp.res$grade_dist_p
  
  tmp.res.educ$admin1.char <- tmp.region
  tmp.res.surv.p$admin1.char <- tmp.region
  tmp.res.cond.p$admin1.char <- tmp.region
  tmp.res.grade.dist.p$admin1.char <- tmp.region
  
  adm1.educ.years <- rbind(tmp.res.educ,adm1.educ.years)
  adm1.survival.p <- rbind(tmp.res.surv.p,adm1.survival.p)
  adm1.cond.survival.p <- rbind(tmp.res.cond.p,adm1.cond.survival.p)
  adm1.grade.dist.p <- rbind(tmp.res.grade.dist.p,adm1.grade.dist.p)
  
}

adm1.agegrp.direct.overall <- list(surv_p = adm1.survival.p,
                                   cond_surv_p = adm1.cond.survival.p,
                                   grade_dist_p = adm1.grade.dist.p,
                                   educ_years = adm1.educ.years)



################################################################
### Check Proportional, Admin-1
################################################################


adm1.direct.cont.odds <- adm1.agegrp.direct.overall$cond_surv_p

adm1.direct.cont.odds <- adm1.direct.cont.odds %>% 
  mutate(logit.mean = logit(mean))%>% 
  mutate(logit.lower = logit(lower))%>% 
  mutate(logit.upper = logit(upper)) %>%
  filter(abs(logit.mean)<8)%>%
  mutate(agegrp=group)%>%
  mutate(logit.mean=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.mean))%>%
  mutate(logit.lower=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.lower))%>%
  mutate(logit.upper=case_when(agegrp=='15-19'&grade_int>3~NA,TRUE~logit.upper))


#adm1.direct.cont.odds <- adm1.direct.cont.odds[abs(adm1.direct.cont.odds$logit_prob)<8,]

adm1.direct.cont.odds <- adm1.direct.cont.odds %>%
  left_join(admin1_info$data%>% select(c('admin1.char','admin1.name')),by='admin1.char')


grade_labels <- c(
  "0" = "No Educ.",
  "1" = "Some Prim.",
  "2" = "Comp. Prim.",
  "3" = "Lower Sec.",
  "4" = "Higher Sec.",
  "5" = "Post-Sec."
)

g.adm1.overall.prop <- ggplot(aes(x = factor(grade_int), group = agegrp), data = adm1.direct.cont.odds) +
  geom_line(aes(y = logit.mean, color = agegrp, group = agegrp),
            position = position_dodge(width = 0.05), linewidth = 0.42) +
  # geom_ribbon(aes(ymin = logit.lower, ymax = logit.upper, fill = agegrp, group = agegrp),
  #             alpha = 0.2, position = position_dodge(width = 0.1)) +
  scale_x_discrete(labels = grade_labels) +
  xlab('Education Level') +
  ylab('Log-Odds of Conditional Dropout Probability')+
  facet_wrap(~admin1.name, ncol = 5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Age Group", color = "Age Group") +  # Correct legend title
  #scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  #scale_y_continuous(breaks = c(3,6,9,12)) +
  #scale_fill_manual(values = custom_palette, name = "Method") +
  #scale_color_manual(values = custom_palette, name = "Method") +
  theme_bw()+
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

if(!file.exists(paste0("overall_adm1_grade_level_proportional_check.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/Admin1'))
  ggsave(g.adm1.overall.prop, width=10, height = 12,#ceiling(dim(admin1_info$data)[1]/3)*3+4,
         file = paste0("overall_adm1_grade_level_proportional_check.pdf"))
}










################################################################
######### check proportional odds by single grades 
################################################################

### original data 

setwd(paste0(data.dir,'/prepared_dat'))

educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))

################################################################
######### prepare analysis data set
################################################################

modified_ind = T

if(modified_ind){
  account.censor.ind=T
  folder.suffix='modified'
}else{
  account.censor.ind = F
  folder.suffix = 'unadj'
}

### overall
educ.splitted.overall <- getEduc_splitted(data = educ_dat_ind,
                                          compact = T, 
                                          account.censor=account.censor.ind,
                                          compact.by =  c('v001','v022', "weights","agegrp"))

educ.splitted.overall$strata <- educ.splitted.overall$v022

### urban
educ.splitted.U <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='urban',],
                                    compact = T, 
                                    account.censor=account.censor.ind,
                                    compact.by =  c('v001','v022', "weights","agegrp"))

educ.splitted.U$strata <- educ.splitted.U$v022


### rural
educ.splitted.R <- getEduc_splitted(data = educ_dat_ind[educ_dat_ind$urban=='rural',],
                                    compact = T, 
                                    account.censor=account.censor.ind,
                                    compact.by =  c('v001','v022', "weights","agegrp"))

educ.splitted.R$strata <- educ.splitted.R$v022



################################################################
######### national
################################################################

natl.overall.direct.year.group <- getEduc.Direct.group(splitted.educ= educ.splitted.overall,
                                                       group.var='agegrp')

# natl.U.direct.year.group <- getEduc.Direct.group(splitted.educ= educ.splitted.U,
#                                                  group.var='agegrp')
# 
# natl.R.direct.year.group <- getEduc.Direct.group(splitted.educ= educ.splitted.R,
#                                                  group.var='agegrp')

################################################################
######### plot natl proportional odds
################################################################


### overall 

natl.overall.direct.cont.odds <- natl.overall.direct.year.group$cond_surv_p

natl.overall.direct.cont.odds <- natl.overall.direct.cont.odds %>% 
  mutate(logit.mean = logit(mean))%>% 
  mutate(logit.lower = logit(lower))%>% 
  mutate(logit.upper = logit(upper)) %>%
  filter(abs(logit.mean)<8)%>%
  mutate(agegrp = group)%>%
  filter(grade_int<16)%>%
  mutate(logit.mean=case_when(agegrp=='15-19'&grade_int>11~NA,TRUE~logit.mean))%>%
  mutate(logit.lower=case_when(agegrp=='15-19'&grade_int>11~NA,TRUE~logit.lower))%>%
  mutate(logit.upper=case_when(agegrp=='15-19'&grade_int>11~NA,TRUE~logit.upper))

g.natl.overall.grades.prop <- ggplot(aes(x = grade_int, group = agegrp), data = natl.overall.direct.cont.odds) +
  geom_line(aes(y = logit.mean, color = agegrp, group = agegrp),
            position = position_dodge(width = 0.01), linewidth = 0.42) +
  xlab('Grade') +
  ylab('Log-Odds of Conditional Dropout Probability')+
  labs(fill = "Age Group", color = "Age Group") +  # Correct legend title
  scale_x_continuous(breaks = c(1:15)) +  # Remove y-axis breaks
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()+
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 15),
    legend.key.width = unit(2, 'cm'),
    #legend.title = element_text(size = 15),
    #strip.text.x = element_text(size = 14),
    #strip.text.y = element_text(size = 14),
    text = element_text(size = 15),
    #axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    #axis.text.y = element_text(size = 13),
    #plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),  # Lighter major grid
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


### save plot
setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("overall_natl_grade_proportional_check.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.natl.overall.grades.prop, width=11, height = 8,#ceiling(dim(admin1_info$data)[1]/3)*3+4,
         file = paste0("overall_natl_grade_proportional_check.pdf"))
}




