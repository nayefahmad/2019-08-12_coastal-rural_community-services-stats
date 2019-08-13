

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

#+ lib, include = FALSE 
# database cnx: --------
library(odbc)
library(dbplyr)
library(tidyverse)
library(stringr)
library(denodoExtractor)

setup_denodo()




#+ analysis

# 2) validate using sql server: -------
#' ## 2) validate using sql server: 

#' See the iCare reports saved in the docs folder of this repo

# connections: 
cnx2 <- dbConnect(odbc::odbc(),
                  dsn = "cnx_SPDBSCSTA001")
census_sql_server <- dplyr::tbl(cnx2, 
                     dbplyr::in_schema("ADTCMart.ADTC", 
                                       "vwCensusFact"))

# 2.1) PRGH ALC rate ------
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


# 2.2) SH ALC rate ------
df2.sh_sql_serv <- 
  census_sql_server %>% 
  filter(FacilityLongName == "Sechelt Hospital", 
         CensusDate <= "2019-07-25", 
         CensusDate >= "2019-07-19") %>%  
         
  # filter(NursingUnitCode %in% c("1AA-SM",
  #                               "2A-SM",
  #                               "3A-SM",
  #                               "CCU-SM",
  #                               "DC-SM",
  #                               "EA-SM",
  #                               "MAT-SM",
  #                               "MH-SM",
  #                               "PAR-SM")) %>%
  
  select(FacilityLongName, 
         CensusDate, 
         PatientID, 
         PatientServiceDADDesc) %>% 
  
  mutate(is_alc = ifelse(PatientServiceDADCode == "99", 1, 0), 
         is_valid = ifelse(PatientServiceDADDesc != "Invalid", 1, 0)) %>% # show_query()
  collect %>% 
  arrange(PatientID, 
          CensusDate) 

sum(df2.sh_sql_serv$is_alc)/nrow(df2.sh_sql_serv)  # 0.2056075







# 3) validate using Denodo: ------------
#' ## 3) validate using Denodo: 

# 3.1) SH census patient-days and ALC patient days ------
df3.sh_census <- 
  vw_census %>% 
  filter(facility_short_name == "SMH", 
         census_date_id <= "20190725", 
         census_date_id >= "20190719") %>% 
  select(facility_short_name, 
         census_date_id, 
         patient_id, 
         med_service_desc, 
         alc_category_desc) %>%
  
  mutate(is_alc = ifelse(str_detect(med_service_desc, "ALC"), 1, 0)) %>% 
  
  collect %>% 
  arrange(patient_id, 
          census_date_id)

# result: 
# df3.sh_census
# summary(df3.sh_census)

# calc ALC rate: 
sum(df3.sh_census$is_alc)/nrow(df3.sh_census)  # 0.2056075

#' Result matches what we got from SQL Server 