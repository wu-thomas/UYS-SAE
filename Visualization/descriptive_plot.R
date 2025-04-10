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
######### load data
################################################################
library(ggplot2)
library(dplyr)

# Sample dataset
group.A.label <- "A: Survey at 2022 – Original Setting"
group.B.label <- "B: Survey at 2012 – Artificial Censoring Applied"


educ.surv.samp.panel.A <- data.frame(
  #group = 'Survey taken in 2022',
  ID = c('Y-1','Y-2','Y-3'),  # Using letters instead of numbers
  birth.year = rep(2005,times=3),
  start.educ = NA,
  #end.educ = NA,
  #T_survey = 2022,
  educ.years = c(0,6,14),
  entrance.age = c(8,6,7) #sample(5:7, 10, replace = TRUE),
  #status = c("Ultimate Completion","Ultimate Completion", "Censored") #sample(c("censored", "completed"), 10, replace = TRUE)
)


educ.surv.samp.panel.B <- data.frame(
  #group = 'Survey taken in 2012',
  ID = c('O-1','O-2','O-3'),  # Using letters instead of numbers
  birth.year = rep(1995,times=3),
  start.educ = NA,
  #end.educ = NA,
  #T_survey = 2012,
  educ.years = c(0,5,16),
  entrance.age = c(8,6,7) #sample(5:7, 10, replace = TRUE),
  #status = c("Ultimate Completion","Ultimate Completion", "Censored") #sample(c("censored", "completed"), 10, replace = TRUE)
)


vline_data <- data.frame(
  group = c(group.B.label, group.A.label),
  vline_year = c(2012,2022)  # Different vlines per facet
)

natl.educ.surv.samp <- rbind (rbind(educ.surv.samp.panel.A,educ.surv.samp.panel.B)%>%
                                mutate(group = group.A.label,T_survey=2022),
                              educ.surv.samp.panel.B%>%
                                mutate(group = group.B.label,T_survey=2012))

# Compute start of education and exit time
natl.educ.surv.samp <- natl.educ.surv.samp %>%
  mutate(
    start.educ = birth.year + entrance.age,
    end.educ.uncensored = start.educ + educ.years,
    end.educ.censored = ifelse(end.educ.uncensored > T_survey, T_survey, end.educ.uncensored) # Truncate if censored
  )%>%
  mutate(status =ifelse(end.educ.uncensored > T_survey, "Censored", "Ultimate Completion"))


natl.educ.surv.samp$group <- factor(natl.educ.surv.samp$group,
                                    levels = c(group.B.label,group.A.label))

# Create plot
library(ggplot2)
library(dplyr)
library(ggforce)


educ.history.graph <- ggplot(natl.educ.surv.samp, aes(y = ID)) + 
  # Birth to school entrance (dashed, green)
  geom_linerange(aes(xmin = birth.year, xmax = start.educ, color = "Birth till School Entry", linetype = "Birth till School Entry"), 
                 size = 0.75) + 
  
  # School entrance to survey time (solid, blue)
  geom_linerange(aes(xmin = start.educ, xmax = pmin(end.educ.censored, T_survey), color = "School till Survey", linetype = "School till Survey"), 
                 size = 0.75) + 
  
  # From survey time onwards (dotted, light blue, censored)
  geom_linerange(aes(xmin = T_survey, xmax = end.educ.uncensored, color = "Survey till Censored", linetype = "Survey till Censored"), 
                 size = 0.75, data = natl.educ.surv.samp %>% filter(status == "Censored")) + 
  
  # Marker at ultimate completion (solid square)
  geom_point(aes(x = end.educ.uncensored, shape = "Ultimate Completion"), size = 4, color = "gray10") +  
  
  # Triangle marker at school entrance (triangle)
  geom_point(aes(x = start.educ, shape = "School Entrance"), size = 3,color = "gray60", stroke = 1.5,
             data = natl.educ.surv.samp)+ #%>% filter(educ.years > 0)) +  
  
  # Marker at censored time (hollow square)
  geom_point(aes(x = end.educ.censored, shape = status), size = 4,
             data = natl.educ.surv.samp %>% filter(status == "Censored")) +
  
  # Marker at birth time (circle)
  geom_point(aes(x = birth.year, shape = 'Birth'), size = 4) +
  
  
  # Custom shape legend (school entrance, completion, still in school)
  scale_shape_manual(name = "Milestones",
                     values = c('Birth' =1,"School Entrance" = 2, "Ultimate Completion" = 15, "Censored" = 0),
                     breaks = c("Birth","School Entrance", "Censored", "Ultimate Completion")) +
  
  # Custom color for three phases
  scale_color_manual(
    name = "Education Timeline",
    values = c("Birth till School Entry" = "green", 
               "School till Survey" = "blue", 
               "Survey till Censored" = "lightblue"),
    labels = c(
      expression("Birth " %->% " School Entry   "),  # Using '%->%' for arrow
      expression("Observed Education   "),
      expression("Unobserved Education  ")
    )
  ) +
  
  # Custom linetype for properly rotated horizontal legend
  scale_linetype_manual(name = "Education Timeline",
                        values = c("Birth till School Entry" = "dashed", 
                                   "School till Survey" = "solid", 
                                   "Survey till Censored" = "longdash"),
                        labels = c(
                          expression("Birth " %->% " School Entry   "),  # Using '%->%' for arrow
                          expression("Observed Education   "),
                          expression("Unobserved Education  ")
                        ))+
  
  # two birth cohorts 
  ggforce::facet_col(facets = vars(group), 
                     scales = "free_y", 
                     space = "free")+
  #facet_wrap(~ group,ncol = 1,scales = 'free')+
  #ggforce::facet_row(vars(group), scales = 'free_x', space = 'free',strip.position = 'top',nrow=1)+
  #facet_wrap_paginate(~ group,ncol = 1,scales = 'free')+
  #facet_grid(rows = vars(group), space = "free", scales = "free") +  # Allows dynamic panel height
  
  theme(panel.spacing = unit(1, "cm"))+
  # Dashed line indicating the survey year
  #geom_vline(xintercept = 2022, linetype = "dashed", color = "red", size = 1.5, alpha = 0.3) +
  geom_vline(data = vline_data, aes(xintercept = vline_year), 
             linetype = "dashed", color = "red", size = 1, alpha = 0.3) +
  geom_label(data = vline_data, aes(x = vline_year, y = 'O-1', 
                                    label = paste0("Survey Time \n",vline_year)),
             fill = "white", color = "grey30", size = 4.5, angle = 0, vjust = 0.5, 
            label.size = 0.3)+

  scale_x_continuous(breaks = seq(1970, 2035, by = 5)) +
  
  # Labels and theme
  xlab("Calendar Year") +  
  ylab("Subject ID") + 
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text.x = element_text(size = 17),
        strip.text.y = element_text(size = 17),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        plot.title = element_text(size = 20),
        # Ensure shape and linetype legends are in separate rows
        legend.box = "vertical")






### save plot
setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("Educ_history_example.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(educ.history.graph, width=12.4, height = 7, file = paste0("Educ_history_example.pdf"))
  
}



############ only 2022 time 
processed.A <- educ.surv.samp.panel.A %>%
  mutate(T_survey=2022) %>%
  mutate(
    start.educ = birth.year + entrance.age,
    end.educ.uncensored = start.educ + educ.years,
    end.educ.censored = ifelse(end.educ.uncensored > T_survey, T_survey, end.educ.uncensored) # Truncate if censored
  )%>%
  mutate(status =ifelse(end.educ.uncensored > T_survey, "Censored", "Ultimate Completion"))

educ.history.simple.graph <- ggplot(processed.A,
                                    aes(y = ID)) + 
  # Birth to school entrance (dashed, green)
  geom_linerange(aes(xmin = birth.year, xmax = start.educ, color = "Birth till School Entry", linetype = "Birth till School Entry"), 
                 size = 0.75) + 
  
  # School entrance to survey time (solid, blue)
  geom_linerange(aes(xmin = start.educ, xmax = pmin(end.educ.censored, T_survey), color = "School till Survey", linetype = "School till Survey"), 
                 size = 0.75) + 
  
  # From survey time onwards (dotted, light blue, censored)
  geom_linerange(aes(xmin = T_survey, xmax = end.educ.uncensored, color = "Survey till Censored", linetype = "Survey till Censored"), 
                 size = 0.75, data = processed.A %>% filter(status == "Censored")) + 
  
  # Marker at ultimate completion (solid square)
  geom_point(aes(x = end.educ.uncensored, shape = "Ultimate Completion"), size = 4, color = "gray10") +  
  
  # Triangle marker at school entrance (triangle)
  geom_point(aes(x = start.educ, shape = "School Entrance"), size = 3,color = "gray60", stroke = 1.5,
             data = processed.A)+ #%>% filter(educ.years > 0)) +  
  
  # Marker at censored time (hollow square)
  geom_point(aes(x = end.educ.censored, shape = status), size = 4,
             data = processed.A %>% filter(status == "Censored")) +
  
  # Marker at birth time (circle)
  geom_point(aes(x = birth.year, shape = 'Birth'), size = 4) +
  
  
  # Custom shape legend (school entrance, completion, still in school)
  scale_shape_manual(name = "Milestones",
                     values = c('Birth' =1,"School Entrance" = 2, "Ultimate Completion" = 15, "Censored" = 0),
                     breaks = c("Birth","School Entrance", "Censored", "Ultimate Completion")) +
  
  # Custom color for three phases
  scale_color_manual(
    name = "Education Timeline",
    values = c("Birth till School Entry" = "green", 
               "School till Survey" = "blue", 
               "Survey till Censored" = "lightblue"),
    labels = c(
      expression("Birth " %->% " School Entry   "),  # Using '%->%' for arrow
      expression("Observed Education   "),
      expression("Unobserved Education  ")
    )
  ) +
  
  # Custom linetype for properly rotated horizontal legend
  scale_linetype_manual(name = "Education Timeline",
                        values = c("Birth till School Entry" = "dashed", 
                                   "School till Survey" = "solid", 
                                   "Survey till Censored" = "longdash"),
                        labels = c(
                          expression("Birth " %->% " School Entry   "),  # Using '%->%' for arrow
                          expression("Observed Education   "),
                          expression("Unobserved Education  ")
                        ))+
  theme(panel.spacing = unit(1, "cm"))+
  # Dashed line indicating the survey year
  #geom_vline(xintercept = 2022, linetype = "dashed", color = "red", size = 1.5, alpha = 0.3) +
  geom_vline(data = vline_data%>%filter(vline_year==2022), aes(xintercept = vline_year), 
             linetype = "dashed", color = "red", size = 1, alpha = 0.3) +
  geom_label(data = vline_data%>%filter(vline_year==2022), aes(x = vline_year, y = 'Y-1', 
                                    label = paste0("Survey Time \n",vline_year)),
             fill = "white", color = "grey30", size = 4.5, angle = 0, vjust = 0.5, 
             label.size = 0.3)+
  
  scale_x_continuous(breaks = seq(1970, 2035, by = 5)) +
  
  # Labels and theme
  xlab("Calendar Year") +  
  ylab("Subject ID") + 
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text.x = element_text(size = 17),
        strip.text.y = element_text(size = 17),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        plot.title = element_text(size = 20),
        # Ensure shape and linetype legends are in separate rows
        legend.box = "vertical")





### save plot
setwd(paste0(res.dir,'/Visualization/National'))

if(!file.exists(paste0("Educ_history_example_only_2022.pdf"))){
  
  setwd(paste0(res.dir,'/Visualization/National'))
  ggsave(educ.history.simple.graph, width=12, height = 5, file = paste0("Educ_history_example_only_2022.pdf"))
  
}





