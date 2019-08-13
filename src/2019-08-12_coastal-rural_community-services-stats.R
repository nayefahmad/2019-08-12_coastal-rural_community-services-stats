

#'--- 
#' title: "ALC rates for Coastal Rural Sites (SH, SGH, PRGH)"
#' author: "Nayef Ahmad"
#' date: "2019-08-12"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 


# database cnx: --------
library(odbc)
library(dbplyr)
library(tidyverse)
library(denodoExtractor)

setup_denodo()


# 2) Validating with weekly iCare report ------------


# sgh census patient-days and ALC patient days 
df1.prgh_census <- 
  vw_census %>% 
  filter(facility_short_name == "SMH", 
         census_date_id <= "20190801", 
         census_date_id >= "20190726") %>% 
  select(facility_short_name, 
         census_date_id, 
         patient_id, 
         is_alc_p4p_definition_at_census, 
         alc_category_desc) %>% 
  collect %>% 
  arrange(patient_id, 
          census_date_id)

# result: 
df1.prgh_census
summary(df1.prgh_census)

# calc ALC rate: 
sum(df1.prgh_census$is_alc)/nrow(df1.prgh_census)

