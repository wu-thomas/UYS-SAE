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
######### load data
################################################################

setwd(paste0(data.dir,'/shapeFiles_gadm'))

country_shp_analysis <- readRDS('country_shp_analysis.rds')
country_shp_smoothed <- readRDS('country_shp_smoothed.rds')
admin1_info <- readRDS('admin1_info.rds')
admin2_info <- readRDS('admin2_info.rds')


setwd(paste0(data.dir,'/DHS_data'))
cluster.info <- readRDS(file=paste0(country,'_',survey_year,'_cluster_info.rds'))

### individual level
setwd(paste0(data.dir,'/prepared_dat'))

educ_dat_ind <- readRDS( paste0('educ_dat_ind_',survey_year,'.rds'))

################################################################
#########    prepare results
################################################################

sim_res_direct_unadj <- data.frame()
sim_res_direct_modified <- data.frame()

sim_res_GLM <- data.frame()
sim_res_INLA_rw1 <- data.frame()
#sim_res_INLA_rw2 <- data.frame()

for(rep_idx in 1:10){
  # rep_idx = 1
  for(sim_age_group in c(25,30,35,40)){
    # sim_age_group = 25
    res_folder_suffix = paste0('sim_censored_',sim_age_group,'_',sim_age_group+4)

    ### overall

    # Direct-unadjusted
    setwd(paste0(res.dir,'/Direct_unadj/',res_folder_suffix,'/rep_',rep_idx))
    tmp_natl_overall_direct_unadj <- readRDS('natl_yearly_direct_overall.rds')$educ_years

    # Direct-modified
    setwd(paste0(res.dir,'/Direct_modified/',res_folder_suffix,'/rep_',rep_idx))
    tmp_natl_overall_direct_modified <- readRDS('natl_yearly_direct_overall.rds')$educ_years

    # GLM
    setwd(paste0(res.dir,'/GLM/',res_folder_suffix,'/rep_',rep_idx))
    tmp_natl_overall_GLM <- readRDS('natl_yearly_GLM_overall.rds')$educ_years

    # INLA
    setwd(paste0(res.dir,'/INLA/',res_folder_suffix,'/rep_',rep_idx,'/National'))
    tmp_natl_overall_INLA_rw1 <- readRDS(paste0('natl_aggre_adm1_BYM2_RW1_typeIV_overall_res.rds'))$educ_years

    #tmp_natl_overall_INLA_rw1 <- readRDS(paste0('natl_aggre_adm1_fixed_RW1_overall_res.rds'))$educ_years
    #tmp_natl_overall_INLA_rw2 <- readRDS(paste0('natl_INLA_','rw2_iid','_time_trend_','overall_res.rds'))$educ.years.summary

    # Assign repetition ID
    tmp_natl_overall_direct_unadj$rep_idx = rep_idx
    tmp_natl_overall_direct_modified$rep_idx = rep_idx
    tmp_natl_overall_GLM$rep_idx = rep_idx
    tmp_natl_overall_INLA_rw1$rep_idx = rep_idx
    #tmp_natl_overall_INLA_rw2$rep_idx = rep_idx

    # Assign age group
    tmp_natl_overall_direct_unadj$age_group = paste(sim_age_group,'-',sim_age_group+4)
    tmp_natl_overall_direct_modified$age_group = paste(sim_age_group,'-',sim_age_group+4)
    tmp_natl_overall_GLM$age_group =  paste(sim_age_group,'-',sim_age_group+4)
    tmp_natl_overall_INLA_rw1$age_group =  paste(sim_age_group,'-',sim_age_group+4)
    #tmp_natl_overall_INLA_rw2$age_group =  paste(sim_age_group,'-',sim_age_group+4)


    # combine all repetitions
    sim_res_direct_unadj <- rbind(sim_res_direct_unadj,tmp_natl_overall_direct_unadj%>% filter(group>=max(educ_dat_ind$birth.year-(sim_age_group-10))&
                                                                               group<max(educ_dat_ind$birth.year-(sim_age_group-15))))
    sim_res_direct_modified <- rbind(sim_res_direct_modified,tmp_natl_overall_direct_modified%>% filter(group>=max(educ_dat_ind$birth.year-(sim_age_group-10))&
                                                                               group<max(educ_dat_ind$birth.year-(sim_age_group-15))))
    sim_res_GLM <- rbind(sim_res_GLM,tmp_natl_overall_GLM%>% filter(group>=max(educ_dat_ind$birth.year-(sim_age_group-10))&
                                                                      group<max(educ_dat_ind$birth.year-(sim_age_group-15))))
    sim_res_INLA_rw1 <- rbind(sim_res_INLA_rw1,tmp_natl_overall_INLA_rw1%>% filter(birth.year>=max(educ_dat_ind$birth.year-(sim_age_group-10))&
                                                                                     birth.year<max(educ_dat_ind$birth.year-(sim_age_group-15))))
    # sim_res_INLA_rw2 <- rbind(sim_res_INLA_rw2,tmp_natl_overall_INLA_rw2%>% filter(birth.year>=max(educ_dat_ind$birth.year-(sim_age_group-10))&
    #                                                                                  birth.year<max(educ_dat_ind$birth.year-(sim_age_group-15))))

  }
}


sim_res_direct_unadj <- sim_res_direct_unadj %>% mutate(direct_unadj_est = mean, birth.year=as.integer(group))%>%
  select(c('birth.year','direct_unadj_est','rep_idx','age_group'))

sim_res_direct_modified <- sim_res_direct_modified %>% mutate(direct_modified_est = mean, birth.year=as.integer(group))%>%
  select(c('birth.year','direct_modified_est','rep_idx'))

sim_res_GLM <- sim_res_GLM %>%  mutate(GLM_est = mean, birth.year=as.integer(group))%>%
  select(c('birth.year','GLM_est','rep_idx'))

sim_res_INLA_rw1 <- sim_res_INLA_rw1 %>%  mutate(INLA_rw1_est = mean)%>%
  select(c('birth.year','INLA_rw1_est','rep_idx'))

# sim_res_INLA_rw2 <- sim_res_INLA_rw2 %>%  mutate(INLA_rw2_est = p_Med)%>%
#   select(c('birth.year','INLA_rw2_est','rep_idx'))




### load true values
setwd(paste0(res.dir,'/Direct_unadj/'))

eudc_res_overall <- readRDS('natl_yearly_direct_overall.rds')$educ_years
eudc_res_overall <- eudc_res_overall %>% filter(group>=max(educ_dat_ind$birth.year-(40-10))&
                                                       group<max(educ_dat_ind$birth.year-(25-15))) %>%
  rename(benchmark = mean)

### merge with the truth (direct estimates without censoring)

sim_res_all <- sim_res_direct_unadj %>%
  left_join(sim_res_direct_modified,by = c("birth.year" = "birth.year", "rep_idx" = "rep_idx")) %>%
  left_join(sim_res_GLM,by = c("birth.year" = "birth.year", "rep_idx" = "rep_idx")) %>%
  left_join(sim_res_INLA_rw1,by = c("birth.year" = "birth.year", "rep_idx" = "rep_idx")) %>%
  #left_join(sim_res_INLA_rw2,by = c("birth.year" = "birth.year", "rep_idx" = "rep_idx")) %>%
  left_join(eudc_res_overall %>% select(c('group','benchmark')),by = c("birth.year" = "group"))

most_vul_age_reminder <- (max(educ_dat_ind$birth.year-(25-15))-1)%%5

sim_res_all <- sim_res_all %>% mutate( censor_year= (5-birth.year+most_vul_age_reminder)%%5)
sim_res_all <- sim_res_all[!is.na(sim_res_all$benchmark),]
### overall bias
overall_bias <- sim_res_all %>%
  mutate(across(ends_with("est"), ~ . - benchmark, .names = "bias_{col}")) %>%
  summarise(across(starts_with("bias"), mean, .names = "overall_avg_{.col}"))

colnames(overall_bias) <- c('direct_unadj','direct_modified','GLM','INLA')

### bias by years to survey
dist_to_survey_year_bias <- sim_res_all %>%
  mutate(across(ends_with("est"), ~ . - benchmark, .names = "bias_{col}")) %>%
  group_by(censor_year) %>%
  summarise(across(starts_with("bias"), mean, .names = "avg_{.col}")) %>%
  ungroup()

colnames(dist_to_survey_year_bias) <- c('censor_year','direct_unadj','direct_modified','GLM','INLA')


### bias by age group
by_age_group_bias <- sim_res_all %>%
  mutate(across(ends_with("est"), ~ . - benchmark, .names = "bias_{col}")) %>%
  group_by(age_group) %>%
  summarise(across(starts_with("bias"), mean, .names = "avg_{.col}")) %>%
  ungroup()

colnames(by_age_group_bias) <- c('age_group','direct_unadj','direct_modified','GLM','INLA')


################################################################
#########    plot
################################################################

sim_res_all_toplot <- rbind(sim_res_direct_unadj %>%  mutate(est=direct_unadj_est) %>%  
                              select(c('birth.year','est','rep_idx')) %>% mutate(method='Naive Weighted'),
                            sim_res_direct_modified %>% mutate(est=direct_modified_est) %>% 
                              select(c('birth.year','est','rep_idx')) %>% mutate(method='Modified Weighted'),
                            sim_res_GLM %>% mutate(est=GLM_est) %>% 
                              select(c('birth.year','est','rep_idx')) %>% mutate(method='Survey GLM'),
                            sim_res_INLA_rw1 %>% mutate(est=INLA_rw1_est) %>% 
                              select(c('birth.year','est','rep_idx')) %>% mutate(method='Spatial Model') )
           

sim_res_all_toplot <- sim_res_all_toplot %>%
  left_join(eudc_res_overall %>% select(c('group','benchmark')),by = c("birth.year" = "group"))                    

sim_res_all_toplot <- sim_res_all_toplot %>% mutate(bias_est = est- benchmark)  %>%
  group_by(birth.year,method) %>%
  summarize(mean_bias = mean(bias_est))%>%
  ungroup()
  
sim_res_all_toplot$age <- 2021-sim_res_all_toplot$birth.year

sim_res_all_toplot$method <- factor(sim_res_all_toplot$method,
                                     levels = c('Spatial Model','Survey GLM', "Naive Weighted",'Modified Weighted'))


custom_palette <- c(brewer.pal(3, "Set1"), "#ba910b") 



g.bias.natl <- ggplot(aes(x = age, group = method), data = sim_res_all_toplot) +
  geom_line(aes(y = mean_bias, color = method, group = method),
            position = position_dodge(width = 0.05), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  # geom_point(aes(y = est, color = method, group = method),
  #            position = position_dodge(width = 0.25), size = 0.85) +
  xlab('Age at Survey (2022)') +
  ylab('Bias in Estimated UYS') +
  #ggtitle('Overall: Direct, Survey GLM, and INLA Estimates') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(25, 40, by = 5)) +
  scale_x_reverse()+
  scale_fill_manual(values = custom_palette, name = "Method") +
  scale_color_manual(values = custom_palette, name = "Method") +
  theme_bw() +  
  theme(
    legend.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.text = element_text(size = 17),
    legend.key.width = unit(2, 'cm'),
    # legend.title = element_text(size = 16),
    # strip.text.x = element_text(size = 18),
    # strip.text.y = element_text(size = 18),
    text = element_text(size = 17),
    # axis.text.x = element_text(size = 18),
    # axis.text.y = element_text(size = 18),
    # plot.title = element_text(size = 20)
  )


setwd(paste0(res.dir,'/Visualization/National'))
if(!file.exists(paste0("sim_bias.pdf"))){
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(g.bias.natl, width=12, height = 8, file = paste0("sim_bias.pdf"))
}
