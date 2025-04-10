library(tidyr)

###############################################################
###  load GADM files
###############################################################
get_country_GADM <- function(country,resolution=1) {
  
  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==country,'ISO3_CountryCode']
  
  gadm_list <- list()
  levels <- 0
  repeat {
    tmp.gadm <- geodata::gadm(country = country_iso3, resolution=resolution,
                              level = levels,
                              path = tempdir())
    if (is.null(tmp.gadm)) {
      break
    } else {
      tmp.gadm <- sf::st_as_sf(tmp.gadm)
      tmp.gadm <- sf::st_set_crs(tmp.gadm, 4326)
      
      n_region <- dim(tmp.gadm)[1]
      #message(paste0('n region: ',n_region))
      if(n_region >1000){break}
      
      
      if(levels==0){      gadm_list[['National']]  <- tmp.gadm
      }else{
        gadm_list[[paste0('Admin-',levels)]]  <- tmp.gadm}
      levels <- levels + 1
    }
  }
  
  
  return(gadm_list)
}


###############################################################
###  prepare individual education years data
###############################################################

#' Prepare individual records for education years and whether still in school indicator 
#' 
#' @param PR.recode.raw DHS PR recode raw data.
#' @param IR.recode.raw DHS IR recode raw data.


prepare_educ_yrs <- function(PR.recode.raw, IR.recode.raw) {
  
  ### 
  dat_women <- merge(IR.recode.raw,
                     PR.recode.raw[,c('hv001','hv002','hvidx','hv108','hv106','hv107','hv121','hv122','hv123','hv124')],
                     by.x=c('v001','v002','v003'),by.y=c('hv001','hv002','hvidx'),
                     all.x=T)
  
  pre <- ""
  strat <- attr(dat_women[, paste0(pre, "v025")], which='labels')
  names(strat) <- tolower(names(strat))
  
  dat_women[, paste0(pre, "v025")] <- ifelse(dat_women[, paste0(pre, "v025")] == strat["urban"][[1]],'urban','rural')
  dat_women[, "urban"] <- factor(dat_women[, paste0(pre, "v025")], levels = c('urban','rural'))
  dat_women[, paste0(pre, "v024")] <- factor(labelled::unlabelled(dat_women[, paste0(pre, "v024")]))
  dat_women[, paste0(pre, "v023")] <- factor(labelled::unlabelled(dat_women[, paste0(pre, "v023")]))
  dat_women[, paste0(pre, "v022")] <- factor(labelled::unlabelled(dat_women[, paste0(pre, "v022")]))
  
  
  vars.to.keep <- c('caseid','v001','v005','v022','v023','v024','v025','v133','v010','v013',
                  'v106','v107','hv106','hv107',
                    'hv121','urban','v149','hv108','hv124','hv123','hv122') # hv108 for education years not reliable
  
  dat_women <- dat_women %>%
    select(all_of(vars.to.keep)) %>%
    mutate(cluster = v001) %>%
    mutate(educ.yrs = v133) %>%
    mutate(birth.year = v010) %>% 
    mutate(agegrp= paste0(as.numeric(v013)*5+10,'-',as.numeric(v013)*5+14))
  
  ### determine in-school status based on different coding from DHS surveys
  if(sum(dat_women$hv121==1,na.rm=T)>0){
    dat_women <- dat_women%>%
      mutate(in.school = as.numeric(hv121==1))
  }else{
    dat_women <- dat_women%>%
      mutate(in.school = as.numeric(hv121==1|hv121==2))
  }
  
  
  ### for in school subject, the variable is often misrecorded as current grade, correct it
  if(sum(!is.na(dat_women$hv124))>0){
    dat_women <- dat_women %>% 
      mutate(educ.yrs = case_when(in.school==1 & hv124-hv108<1 ~v133-1,
                                  TRUE~v133))%>%
      mutate(educ.yrs = case_when(educ.yrs <0 ~ 0,
                                  TRUE~educ.yrs))
  }else{
    
    ### older surveys recorded education years as current grade, not completed
    dat_women <- dat_women %>% 
      mutate(educ.yrs = case_when(educ.yrs>0&in.school==1 ~ educ.yrs-1,
                                  TRUE~educ.yrs))
  }
  
  
  
  
  dat_women$weights=dat_women$v005/1000000
  
  
  return(dat_women)
  
  
  # in recent birth cohorts, values from v133 is higher than hv108 for a small percentage of people
  # I suspect that a small portion of the subjects misrecorded the completed years by counting the last year in progress
  
  # educ_dat_ind <- educ_dat_ind[educ_dat_ind$hv108<90,]
  # educ_dat_ind$diff <- educ_dat_ind$v133-educ_dat_ind$hv108
  # 
  # tmp_diff <- educ_dat_ind %>% group_by(birth.year)%>%
  #   summarize(mean_diff = mean(diff))
  # 
  # table(educ_dat_ind$in.school,educ_dat_ind$diff)
  
  
}


###############################################################
###  merge geographical info to analysis data
###############################################################

merge_adm_info <- function(data,adm1.info,adm2.info,cluster.info){
  
  # merge cluster info
  data <- left_join(data,cluster.info$data,by=c("cluster"))
  
  # merge admin char (index)
  data <- left_join(data,adm1.info[,c('admin1.name','admin1.char','admin1.num')],by=c("admin1.name"))
  data <- left_join(data,adm2.info[,c('admin2.name.full','admin2.char','admin2.num')],by=c("admin2.name.full"))
  
  # remove clusters missing GPS
  data<- data[!(is.na(data$LONGNUM)), ]
  
  return(data)
}


###############################################################
###  split education history for discrete time hazard
###############################################################

#' split education history into single year segments
#'
#' @param data DHS IR recode raw data.
#' @param compact Whether to aggregate data (based on cluster etc.)
#' @param compact.by Variable for the aggregation
#' @param account.censor Whether to account for censor, i.e. whether treat survey year as stop receiving education


getEduc_splitted <- function(data = NULL,
                             compact = T, 
                             account.censor=T,
                             compact.by =  c('v001', "weights")
) {
  
  splitted.educ <- data %>%
    rowwise() %>%  # Enable row-wise operations
    mutate(
      # Dropout indicator
      start_grade = list(seq(0, educ.yrs)),
      end_grade = list(seq(1, educ.yrs+1))
    )%>%
    unnest(cols = c(start_grade, end_grade)) %>%
    ungroup() %>% 
    mutate(
      drop = ifelse(end_grade > educ.yrs, 1, 0),
    )   
  
  if(account.censor){
    splitted.educ <- splitted.educ %>% filter( !(end_grade > educ.yrs&in.school==1))
  }
  
  splitted.educ <- splitted.educ[splitted.educ$end_grade<= max(splitted.educ$educ.yrs),]
  
  ### specify each period
  splitted.educ$bin <- paste0(splitted.educ$start_grade,'-',splitted.educ$end_grade)
  splitted.educ$grade <- factor(as.character(splitted.educ$bin),levels=unique(splitted.educ$bin))
  
  ### if not compact
  if(!compact){
    return(splitted.educ)
  }
  
  ### compact by birth cohort/region/etc.
  #splitted.educ$birth.year <- splitted.educ$v010
  
  splitted.educ.aggre <- splitted.educ[, c(compact.by, "grade", "drop")]
  
  splitted.educ.aggre$total <- 1
  
  formula <- as.formula(paste0(".~ grade+ ", paste(compact.by, collapse = " + ")))
  
  splitted.educ.comp <- stats::aggregate(formula, data = splitted.educ.aggre, FUN = sum, drop = TRUE)
  
  splitted.educ.comp$drop_frac <- splitted.educ.comp$drop/splitted.educ.comp$total
  splitted.educ.comp$Y <- splitted.educ.comp$drop
  
  if('birth.year' %in% compact.by){
    splitted.educ.comp$birth.year.int <- splitted.educ.comp$birth.year-min(splitted.educ.comp$birth.year)+1
  }
  
  splitted.educ.comp$cluster <- splitted.educ.comp$v001
  
  return(splitted.educ.comp)
  
}



###############################################################
###  split education history for simulation
###############################################################


### !! different than the split for observed data
### because in historical data, we only observe the completed years of education and 
### dropout can only happen at end of a grade, not like reality that it can be in between grades

#' split education history into single year segments
#'
#' @param data DHS IR recode raw data.
#' @param compact Whether to aggregate data (based on cluster etc.)
#' @param compact.by Variable for the aggregation
#' @param account.censor Whether to account for censor, i.e. whether treat survey year as stop receiving education


getEduc_splitted_sim <- function(data = NULL,
                             compact = T, 
                             account.censor=T,
                             compact.by =  c('v001', "weights")
) {
  
  ### prepare variables
  educ.surv <- as.data.table(data)
  variables <- colnames(data)
  variables <- variables[!variables %in% c('geometry')]
  
  
  ### prepare variables for survival format 
  if(account.censor){educ.surv$drop <- as.numeric(educ.surv$in.school==0)}else{
    educ.surv$drop <- 1}
  
  educ.surv$time <- educ.surv$educ.yrs # survival time
  
  
  educ.surv$obsStart <- 0 # begin period
  educ.surv$obsStop <- as.double(educ.surv$educ.yrs) # drop out/ finish school year
  
  educ.surv$obsStop[educ.surv$obsStart == educ.surv$obsStop] <- educ.surv$obsStop[educ.surv$obsStart == educ.surv$obsStop] + 0.01
  
  ### split education history into person-year
  formula <- as.formula(paste(c("Surv(time = obsStart, time2=obsStop, event = drop)~ drop", variables),  
                              collapse = "+"))
  
  splitted.educ <- survival::survSplit(formula,
                                       data = educ.surv, cut = c(0.02, 1:max(educ.surv$educ.yrs)),
                                       start = "start_grade", end = "end_grade", event = "drop")
  
  
  splitted.educ[splitted.educ$start_grade <0.03,]$start_grade <- 0   
  splitted.educ[splitted.educ$end_grade <0.03,]$end_grade <- 0   
  
  splitted.educ$start_grade <- as.integer(splitted.educ$start_grade)+1
  splitted.educ$end_grade <- as.integer(splitted.educ$end_grade)+1
  
  splitted.educ[splitted.educ$end_grade==1,]$start_grade <- 0   
  #splitted.educ[splitted.educ$end_grade==1,]$end_grade <- 0   
  
  
  splitted.educ <- splitted.educ[splitted.educ$end_grade<= max(splitted.educ$educ.yrs),]
  
  ### specify each period
  splitted.educ$bin <- paste0(splitted.educ$start_grade,'-',splitted.educ$end_grade)
  # splitted.educ[splitted.educ$bin=='0-0',]$bin <- '0'
  splitted.educ$grade <- factor(as.character(splitted.educ$bin),levels=unique(splitted.educ$bin))
  
  ### if not compact
  if(!compact){
    return(splitted.educ)
  }
  
  ### compact by birth cohort/region/etc.
  #splitted.educ$birth.year <- splitted.educ$v010
  
  splitted.educ.aggre <- splitted.educ[, c(compact.by, "grade", "drop")]
  
  splitted.educ.aggre$total <- 1
  
  formula <- as.formula(paste0(".~ grade+ ", paste(compact.by, collapse = " + ")))
  
  splitted.educ.comp <- stats::aggregate(formula, data = splitted.educ.aggre, FUN = sum, drop = TRUE)
  
  splitted.educ.comp$drop_frac <- splitted.educ.comp$drop/splitted.educ.comp$total
  splitted.educ.comp$Y <- splitted.educ.comp$drop
  
  if('birth.year' %in% compact.by){
    splitted.educ.comp$birth.year.int <- splitted.educ.comp$birth.year-min(splitted.educ.comp$birth.year)+1
    
  }
  
  splitted.educ.comp$cluster <- splitted.educ.comp$v001
  
  return(splitted.educ.comp)
  
}
