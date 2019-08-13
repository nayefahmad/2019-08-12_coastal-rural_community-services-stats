

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



# 3) validate using sql server: -------

#' See the iCare reports saved in the docs folder of this repo

# connections: 
cnx2 <- dbConnect(odbc::odbc(),
                  dsn = "cnx_SPDBSCSTA001")
census_sql_server <- dplyr::tbl(cnx2, 
                     dbplyr::in_schema("ADTCMart.ADTC", 
                                       "vwCensusFact"))

# 3.1) PRGH ALC rate ------
df1.prgch_sql_serv <- 
  census_sql_server %>% 
  filter(FacilityLongName == "Powell River General Hospital", 
         CensusDate <= "2019-08-01", 
         CensusDate >= "2019-07-26") %>% 
  select(FacilityLongName, 
         CensusDate, 
         PatientID, 
         PatientServiceDADDesc) %>% 
  mutate(is_alc = ifelse(PatientServiceDADCode == "99", 1, 0)) %>% # show_query()
  collect %>% 
  arrange(PatientID, 
          CensusDate) 

sum(df1.prgch_sql_serv$is_alc)/nrow(df1.prgch_sql_serv)  # 0.2943262; matches with iCare report 


# 3.2) SH ALC rate ------
df2.sh_sql_serv <- 
  census_sql_server %>% 
  filter(FacilityLongName == "Sechelt Hospital", 
         CensusDate <= "2019-07-18", 
         CensusDate >= "2019-07-12", 
         NursingUnitCode %in% c("1AA-SM", 
                                "2A-SM", 
                                "3A-SM", 
                                "CCU-SM", 
                                "DC-SM", 
                                "EA-SM", 
                                "MAT-SM", 
                                "MH-SM", 
                                "PAR-SM")) %>% 
  select(FacilityLongName, 
         CensusDate, 
         PatientID, 
         PatientServiceDADDesc) %>% 
  mutate(is_alc = ifelse(PatientServiceDADCode == "99", 1, 0), 
         is_valid = ifelse(PatientServiceDADDesc != "Invalid", 1, 0)) %>% # show_query()
  collect %>% 
  arrange(PatientID, 
          CensusDate) 

sum(df2.sh_sql_serv$is_alc)/sum(df2.sh_sql_serv$is_valid)
