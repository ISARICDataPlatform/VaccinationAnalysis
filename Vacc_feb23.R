  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(stringi)
  library(forcats)
  library(tables)
  
  #Setting wd and loading data
  setwd("F:\\VaccinationAnalysis")
  
  load(file='Data/vaccination_feb2023_v2.rda') #Vaccination data
  
  vacc <- subset(vaccination_feb, select=c("usubjid","studyid","inrefid","intrt","inmodify",
                                           "inpresp","inoccur","instdtc","indtc","intpt",
                                           "intptref"))
  
  load(file='Data/input.tbl_feb2023.v1.rda')
  
  adm_data <- subset(input.tbl, select=c("usubjid","date_admit"))

  #Merging Vaccination & Admission datasets (removes unmatched observations)
  vaccadmi <- merge(vacc, adm_data, by= c("usubjid", "usubjid"))
  
  #Subjects lost in the merging
  lost <- subset(vacc, !(usubjid %in% vaccadmi$usubjid))
  
  #Identifying & counting observations with admission pre-March 2021
  vaccadmi$prevac <- dplyr::if_else(vaccadmi$date_admit > as.Date("2021-03-01"), 
                               "N", "Y")
  
  #Removing observations with admission pre-March 2021
  vaccadmi <- subset(vaccadmi, !(prevac == "Y"))
  
  #Removing follow-up rows
  vaccadmi <- subset(vaccadmi, !(intpt == "FOLLOW-UP"))
  
  # Creating a variable that gives the number of rows per subject ID (number_visits)
  # and another variable that cumulatively counts the number of visits (zero_ind)
  number_visits <- rle(sort(vaccadmi$usubjid))
  vaccadmi$number_visits <- number_visits[[1]][match(vaccadmi$usubjid, number_visits[[2]])]
  
  vaccadmi$zero_ind <- ave(vaccadmi$usubjid==vaccadmi$usubjid, vaccadmi$usubjid, FUN=cumsum)
  
  # Table of number of rows per subject 
  # Note that here and in  other places we need to use (vaccination['zero_ind'] == 1) to ensure subjects 
  # are only counted once
  table(subset(vaccadmi, zero_ind == 1)$number_visits)
  
  # We need to create a variable that summarises whether the individual receive or not vaccination, accounting for all his/her rows
  # the first step is creating a numerical version of the inoccur variable, as not possible to use the string format
  vaccadmi$vaccination_num[vaccadmi$inoccur =='Y'] <- 1
  vaccadmi$vaccination_num[vaccadmi$inoccur =='N'] <- 0
  vaccadmi$vaccination_num[vaccadmi$inoccur =='U'] <- -9
  
  # Creating a variable that says: if patient has one row with inoccur = Y, it will be considered vaccinated; 
  # if he/she has no inoccur = Y, but it will be considered Negative, even if other rows had Unknown
  
  #Vaccination_nb_values : Checks vaccination number is equal across all rows for one subject
  #Vaccination_ever : Checks maximum value of Vaccination_num for all rows of one subject (Y/N/U vaccine status)
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(vaccination_nb_values = length(unique(na.omit(vaccination_num))))
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(vaccination_ever = max(na.omit(vaccination_num)))
  
  # Two variables were created above. In addition to the one explained above, this one tells how many had more 
  # than one value of inoccur (not accounting for missing values)
  table(subset(vaccadmi, zero_ind=="1")$vaccination_nb_values)
  
  # Distribution of vaccination variable per subject ID (not by row)
  table(subset(vaccadmi, zero_ind == 1)$vaccination_ever)
  
  # Vaccine type for those with unknown, no, and yes  for vaccine
  table(subset(vaccadmi, vaccination_ever == -9)$inmodify) #Unknown
  table(subset(vaccadmi, vaccination_ever == 0)$inmodify)  #No
  table(subset(vaccadmi, vaccination_ever == 1)$inmodify)  #Yes
  
  ## Checking vaccine dose info for unknown, no and yes for vaccine
  table(subset(vaccadmi, vaccination_ever == -9)$inrefid) #Unknown
  table(subset(vaccadmi, vaccination_ever == 0)$inrefid) #No
  table(subset(vaccadmi, vaccination_ever == 1)$inrefid) #Yes
  
  ##------------------------------------------------------------------------------------------------------
  ##Recoding problematic data
  
  #Finding subjects with missing days (YY-MM) only via length of string
  vaccadmi$datelength <- stri_length(vaccadmi$instdtc) %>% as.data.frame()
  
  table(vaccadmi$datelength)
  #Some have a value of 7 which implies an incomplete date
  daymiss <- subset(vaccadmi, datelength == "7", select= c("usubjid","inrefid","inmodify","instdtc"))
  
  #Recoding missing days into 15th of the month
  vaccadmi$instdtc[vaccadmi$instdtc =="2021-01"] <- "2021-01-15"
  vaccadmi$instdtc[vaccadmi$instdtc =="2021-03"] <- "2021-03-15"
  
  #Creating a variable to identify rows with modified date
  vaccadmi$dateamend <- if_else(vaccadmi$usubjid %in% daymiss$usubjid, 1, 0) 
  
  ##-------------------------------------------------------------------------------------------------------
  # The rest of the code is looking at vaccine types and vaccine date
  # The first step here is to create a numerical vaccine type variable
  
  #Recoding names for subjects with missing vaccine names
  vaccadmi$inmodify[vaccadmi$intrt == "PFIZER COVID VACCINE"] <- "COVID-19 VACCINE PFIZER-BIONTECH"
  vaccadmi$inmodify[vaccadmi$intrt == "NO COVID VACCINATIONS"] <- "COVID-19 VACCINATION"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID19 AZ VACCINE"] <- "COVID-19 VACCINE ASTRAZENECA/UNIVERSITY OF OXFORD"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID VACCINE PFIZER  BOOSTER"] <- "COVID-19 VACCINE PFIZER-BIONTECH"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID VACCINE PFIZER"] <- "COVID-19 VACCINE PFIZER-BIONTECH"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID VACCINE  ASTRA ZENECA"] <- "COVID-19 VACCINE ASTRAZENECA/UNIVERSITY OF OXFORD"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID-19 VACCINE SINOVAC (CORONAVAC)"] <- "COVID-19 VACCINE SINOVAC"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID-19 VACCINE PFIZER/BIO-N-TEC"] <- "COVID-19 VACCINE PFIZER-BIONTECH"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID-19 VACCINE PFIZER"] <- "COVID-19 VACCINE PFIZER-BIONTECH"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID-19 VACCINE CITY NAME REDACTED/ASTRAZENECA"] <- "COVID-19 VACCINE ASTRAZENECA/UNIVERSITY OF OXFORD"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID-19 VACCINE CANSINOBIO"] <- "COVID-19 VACCINE CANSINBIO"
  vaccadmi$inmodify[vaccadmi$intrt == "COVID-19 MR VACCINE COMIRNATY"] <- "COVID-19 VACCINE TYPE UNKNOWN"
  vaccadmi$inmodify[vaccadmi$intrt == "COME MALATO COVID LA PRIMA VACCINAZIONE FACEVA LE VECI DELLA SECONDA"] <- "COVID-19 VACCINE TYPE UNKNOWN"
  vaccadmi$inmodify[vaccadmi$intrt == "ASTRAZENECA COVID 19 VACCINE"] <- "COVID-19 VACCINE ASTRAZENECA/UNIVERSITY OF OXFORD"
  
  vaccadmi$vaccine_type_code <- recode(vaccadmi$inmodify,
                               "COVID-19 VACCINATION"="0", 
                                 "COVID-19 VACCINE ASTRAZENECA/UNIVERSITY OF OXFORD"="1",
                                 "COVID-19 VACCINE COVAXIN"="2",
                                 "COVID-19 VACCINE JANSSENS (JOHNSON AND JOHNSON)"="3",
                                 "COVID-19 VACCINE MODERNA"="4",
                                 "COVID-19 VACCINE NOVAVAX"="5",
                                 "COVID-19 VACCINE PFIZER-BIONTECH"="6",
                                 "COVID-19 VACCINE SINOPHARM"="7",
                                 "COVID-19 VACCINE SINOVAC"="8",
                                 "COVID-19 VACCINE SPUTNIK V"="9",
                                 "COVID-19 VACCINE TYPE UNKNOWN"="10"
                                 )

  #-------------------------------------------------------------------------
  ## Generating variables with information per dose (DOSE 1) for each subject 
  #Temp_Dose_1 applies the vaccine code number only for rows with "Dose 1", all other values are -10
  vaccadmi$temp_dose_1 <- ifelse(vaccadmi$inrefid =='DOSE 1', vaccadmi$vaccine_type_code, -10)
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_type_dose_1 = length(unique(na.omit(temp_dose_1))))
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(type_dose_1 = max(temp_dose_1))
  
  #-------------------------------------------------------------------------
  ## Generating variables with information per dose (DOSE 2) for each subject 
  vaccadmi$temp_dose_2 <- ifelse(vaccadmi$inrefid =='DOSE 2', vaccadmi$vaccine_type_code, -10)
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_type_dose_2 = length(unique(na.omit(temp_dose_2))))
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(type_dose_2 = max(temp_dose_2))
  
  #-------------------------------------------------------------------------
  ## Generating variables with information per dose (DOSE 3) for each subject 
  vaccadmi$temp_dose_3 <- ifelse(vaccadmi$inrefid =='DOSE 3', vaccadmi$vaccine_type_code, -10)
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_type_dose_3 = length(unique(na.omit(temp_dose_3))))
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(type_dose_3 = max(temp_dose_3))
  
  #-------------------------------------------------------------------------
  ## Generating variables with information per dose (DOSE 4) for each subject 
  vaccadmi$temp_dose_4 <- ifelse(vaccadmi$inrefid =='DOSE 4', vaccadmi$vaccine_type_code, -10)
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_type_dose_4 = length(unique(na.omit(temp_dose_4))))
  
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(type_dose_4 = max(temp_dose_4))

  #Converting type_dose = -10 to NA
  vaccadmi$type_dose_1[vaccadmi$type_dose_1 ==-10] <- NA
  vaccadmi$type_dose_2[vaccadmi$type_dose_2 ==-10] <- NA
  vaccadmi$type_dose_3[vaccadmi$type_dose_3 ==-10] <- NA
  vaccadmi$type_dose_4[vaccadmi$type_dose_4 ==-10] <- NA
  
  #Vaccination date-------------------------
  
  # The first step is to ensure the variable in the right format (date format)
  vaccadmi$vaccination_date <- as.Date(vaccadmi$instdtc)
  
  # How many observations have non-missing vaccination date
  table(is.na(vaccadmi$vaccination_date))
  
  ## Distribution of non-missing date by Vaccination_ever
  table(subset(vaccadmi, instdtc != "")$vaccination_ever)
  #This states that some people without "Yes" for vaccination, have a vaccination date.
  #These records are stored in the "dateproblem" dataframe.
  dateproblem <- as.data.frame(subset(vaccadmi, instdtc != "" & vaccination_ever <=0))
  
  # Checking if there are vaccination observations with missing Dose info
  doseproblem <- as.data.frame(subset(vaccadmi, vaccadmi$instdtc != "" & vaccadmi$inrefid == ""))
  #Subjects in 'doseproblem' have vaccination date but missing dosage information
  
  ## Checking if there are duplicated information for those with doses
  
  #Individuals with Dose 1/2/3/4 and vaccination
  d1 <- subset(vaccadmi, inrefid == "DOSE 1" & instdtc != "")
  uniqueN(d1$usubjid)
  
  d2 <- subset(vaccadmi, inrefid == "DOSE 2" & instdtc != "")
  uniqueN(d2$usubjid)
  
  d3 <- subset(vaccadmi, inrefid == "DOSE 3" & instdtc != "")
  uniqueN(d3$usubjid)
  
  d4 <- subset(vaccadmi, inrefid == "DOSE 4" & instdtc != "")
  uniqueN(d4$usubjid)
  
  #-------------------------------------------------------------------------
  ## Generating vaccine date variable with information per subject (DOSE 1 only)
  #First step is creating a temporary variable that has vaccination date only for those observations with the right "DOSE" row
  vaccadmi$temp_date_1 <- ifelse(vaccadmi$inrefid =='DOSE 1' & is.na(vaccadmi$vaccination_date) == FALSE, vaccadmi$vaccination_date, 0)
  
  #Defining vacc_date & nb_vaccine_date
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(vacc_date_1 = max(temp_date_1))
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_vaccine_date_1 = length(unique(temp_date_1)))
  
  #Converting from integer to time
  vaccadmi$temp_date_1 <- as.POSIXct(vaccadmi$temp_date_1*24*60*60, origin = "1970-01-01", tz="UTC")
  vaccadmi$vacc_date_1 <- as.POSIXct(vaccadmi$vacc_date_1*24*60*60, origin = "1970-01-01", tz="UTC")
  
  #Converting fake temporary dates to NA
  vaccadmi$vacc_date_1[vaccadmi$vacc_date_1 < 1990] <- NA
  
  #-------------------------------------------------------------------------
  ## Generating vaccine date variable with information per subject (DOSE 2 only)
  #First step is creating a temporary variable that has vaccination date only for those observations with the right "DOSE" row
  vaccadmi$temp_date_2 <- ifelse(vaccadmi$inrefid =='DOSE 2' & is.na(vaccadmi$vaccination_date) == FALSE, vaccadmi$vaccination_date, 0)
  
  #Defining vacc_date & nb_vaccine_date
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(vacc_date_2 = max(temp_date_2))
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_vaccine_date_2 = length(unique(temp_date_2)))
  
  #Converting from integer to time
  vaccadmi$temp_date_2 <- as.POSIXct(vaccadmi$temp_date_2*24*60*60, origin = "1970-01-01", tz="UTC")
  vaccadmi$vacc_date_2 <- as.POSIXct(vaccadmi$vacc_date_2*24*60*60, origin = "1970-01-01", tz="UTC")
  
  #Converting fake temporary dates to NA
  vaccadmi$vacc_date_2[vaccadmi$vacc_date_2 < 1990] <- NA

  #-------------------------------------------------------------------------
  ## Generating vaccine date variable with information per subject (DOSE 3 only)
  #First step is creating a temporary variable that has vaccination date only for those observations with the right "DOSE" row
  vaccadmi$temp_date_3 <- ifelse(vaccadmi$inrefid =='DOSE 3' & is.na(vaccadmi$vaccination_date) == FALSE, vaccadmi$vaccination_date, 0)
  
  #Defining vacc_date & nb_vaccine_date
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(vacc_date_3 = max(temp_date_3))
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_vaccine_date_3 = length(unique(temp_date_3)))
  
  #Converting from integer to time
  vaccadmi$temp_date_3 <- as.POSIXct(vaccadmi$temp_date_3*24*60*60, origin = "1970-01-01", tz="UTC")
  vaccadmi$vacc_date_3 <- as.POSIXct(vaccadmi$vacc_date_3*24*60*60, origin = "1970-01-01", tz="UTC")
  
  #Converting fake temporary dates to NA
  vaccadmi$vacc_date_3[vaccadmi$vacc_date_3 < 1990] <- NA
  
  #-------------------------------------------------------------------------
  ## Generating vaccine date variable with information per subject (DOSE 4 only)
  #First step is creating a temporary variable that has vaccination date only for those observations with the right "DOSE" row
  vaccadmi$temp_date_4 <- ifelse(vaccadmi$inrefid =='DOSE 4' & is.na(vaccadmi$vaccination_date) == FALSE, vaccadmi$vaccination_date, 0)
  
  #Defining vacc_date & nb_vaccine_date
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(vacc_date_4 = max(temp_date_4))
  vaccadmi <- vaccadmi %>% group_by(usubjid) %>% dplyr::mutate(nb_vaccine_date_4 = length(unique(temp_date_4)))
  
  #Converting from integer to time
  vaccadmi$temp_date_4 <- as.POSIXct(vaccadmi$temp_date_4*24*60*60, origin = "1970-01-01", tz="UTC")
  vaccadmi$vacc_date_4 <- as.POSIXct(vaccadmi$vacc_date_4*24*60*60, origin = "1970-01-01", tz="UTC")
  
  #Converting fake temporary dates to NA
  vaccadmi$vacc_date_4[vaccadmi$vacc_date_4 < 1990] <- NA

  #-------------------------------------------------------------------------
  #Number of subjects with vaccine date 1
  sum(is.na(subset(vaccadmi, zero_ind==1)$vacc_date_1) == FALSE)
  
  #Number of subjects with vaccine date 2
  sum(is.na(subset(vaccadmi, zero_ind==1)$vacc_date_2) == FALSE)

  #Number of subjects with vaccine date 3
  sum(is.na(subset(vaccadmi, zero_ind==1)$vacc_date_3) == FALSE)
  
  #Number of subjects with vaccine date 4
  sum(is.na(subset(vaccadmi, zero_ind==1)$vacc_date_4) == FALSE)
 
  #----------------------------------------------------------------
  ## Are the successive doses occurring after the previous ones as expected? (Dose 2 and 1)
  dateorder <- subset(vaccadmi, zero_ind==1 & vacc_date_1 >= vacc_date_2)
  dateorder <- subset(dateorder, select = c(usubjid, studyid, vacc_date_1, vacc_date_2, type_dose_1, type_dose_2))
  
  vaccadmi$conflict_date1 <- if_else(vaccadmi$usubjid %in% dateorder$usubjid, 1, 0)
  
  ## Are the successive doses occurring after the previous ones as expected? (Dose 3 and 2)
  dateorder2 <- subset(vaccadmi, zero_ind==1 & vacc_date_2 >= vacc_date_3) 
  dateorder2 <- subset(dateorder2, select = c(usubjid, studyid, vacc_date_2, vacc_date_3, type_dose_1, type_dose_2, type_dose_3))
  
  vaccadmi$conflict_date2 <- if_else(vaccadmi$usubjid %in% dateorder2$usubjid, 1, 0)
 
#--------------------------------------------------------------------
#Flow chart decisions

#Removing subjects with unknown vaccination, but have a vaccine type listed
conflicta <- subset(vaccadmi, vaccination_ever == -9 & inmodify=="COVID-19 VACCINE TYPE UNKNOWN") %>% as.data.frame()
vaccadmi <- vaccadmi[!vaccadmi$usubjid %in% conflicta$usubjid,]
  
#subjects without vaccination, but have a vaccine type listed
conflictb <- subset(vaccadmi, vaccination_ever == 0 & (inmodify=="COVID-19 VACCINE SINOPHARM" | inmodify=="COVID-19 VACCINE PFIZER-BIONTECH" | inmodify =="COVID-19 VACCINE JANSSENS (JOHNSON AND JOHNSON)" | inmodify=="COVID-19 VACCINE TYPE UNKNOWN")) %>% as.data.frame()
conflictb <- subset(vaccadmi, usubjid %in% conflictb$usubjid, select = c(usubjid,inrefid,inmodify,inoccur,intpt))

#subjects without vaccination, but have a vaccine type listed removed
vaccadmi <- vaccadmi[!vaccadmi$usubjid %in% conflictb$usubjid,]

#removing subjects with blank vaccination status, no dose info, but vaccine info
vaccadmi <- subset(vaccadmi, !(vaccination_ever == "-Inf"))

#Changing patient dates with dose 1/2/3 before December 2020 to NA
vaccadmi$vacc_date_1[vaccadmi$usubjid %in% subset(d1, instdtc < "2020-12-01")$usubjid] <- NA

vaccadmi$vacc_date_2[vaccadmi$usubjid %in% subset(d2, instdtc < "2020-12-01")$usubjid] <- NA

#Changing patient dates with dose 2 date earlier than dose 1 to NA
vaccadmi$vacc_date_1[vaccadmi$usubjid %in% dateorder$usubjid] <- NA
vaccadmi$vacc_date_2[vaccadmi$usubjid %in% dateorder$usubjid] <- NA

#Changing patient dates with dose 3 date earlier than dose 2 to NA
vaccadmi$vacc_date_2[vaccadmi$usubjid %in% dateorder2$usubjid] <- NA
vaccadmi$vacc_date_3[vaccadmi$usubjid %in% dateorder2$usubjid] <- NA

####Vaccination post-hospitalisation check---------------------------------------------------------------
vacchosp <- subset(vaccadmi, instdtc > date_admit & inrefid =="DOSE 1" & intpt=="", select= c("usubjid","inrefid","instdtc","date_admit","intpt"))
# 287  patients with first dose occurring after hospitalisation

vacchosp$timediff <- round(difftime(vacchosp$instdtc, vacchosp$date_admit, units = "days"), digits = 0)

vaccadmi$conflict_d1posthosp <- if_else(vaccadmi$usubjid %in% vacchosp$usubjid, 1, 0)

#Dose 2
vacchosp2 <- subset(vaccadmi, instdtc > date_admit & inrefid =="DOSE 2" & intpt=="", select= c("usubjid","inrefid","instdtc","date_admit","intpt"))

vacchosp2$timediff <- round(difftime(vacchosp2$instdtc, vacchosp2$date_admit, units = "days"), digits = 0)

vaccadmi$conflict_d2posthosp <- if_else(vaccadmi$usubjid %in% vacchosp2$usubjid, 1, 0)

#Dose 3
vacchosp3 <- subset(vaccadmi, instdtc > date_admit & inrefid =="DOSE 3" & intpt=="", select= c("usubjid","inrefid","instdtc","date_admit","intpt"))

vacchosp3$timediff <- round(difftime(vacchosp3$instdtc, vacchosp3$date_admit, units = "days"), digits = 0)

vaccadmi$conflict_d3posthosp <- if_else(vaccadmi$usubjid %in% vacchosp3$usubjid, 1, 0)

#Removing subjects with admission in/after March 2023 (End of data collection)
vaccadmi <- filter(vaccadmi, date_admit < '2023-03-01')
#-----------------------------------------------------------------------------------------------------------

#Table of vaccinated subjects with no dose info
View(subset(vaccadmi, vaccination_ever==1 & number_visits==1))

vaccadmi$ref <- 1

#Converting numerical values back for vaccination status
vaccadmi$vaccination_ever[vaccadmi$vaccination_ever == "1"] <- "Yes"
vaccadmi$vaccination_ever[vaccadmi$vaccination_ever == "0"] <- "No"
vaccadmi$vaccination_ever[vaccadmi$vaccination_ever == "-9"] <- "Unknown"

###Removing unnecessary attributes & converting to long form for final set
vaccadmifinal <- subset(vaccadmi, select=c(
                                          "usubjid","studyid","inrefid","intrt",
                                          "inmodify","inpresp","inoccur","instdtc","indtc",
                                          "date_admit","number_visits",
                                          "zero_ind", "vaccination_ever", "type_dose_1", "type_dose_2", "type_dose_3", "type_dose_4", 
                                          "vacc_date_1", "vacc_date_2", "vacc_date_3", "vacc_date_4", "dateamend","conflict_date1", "conflict_date2", 
                                          "conflict_d1posthosp", "conflict_d2posthosp", "conflict_d3posthosp", "ref"))

save(vaccadmifinal, file = "vaccadmifinal.rda")

#Merging and converting to one row per patient-----------------------------------------------------------------

vacc_ever<-vaccadmifinal%>%
  filter(inpresp=="Y")%>%
  filter(intrt=="COVID-19 VACCINATION")%>%
  dplyr::select(studyid, usubjid,"covid_vacc"=inoccur, intrt)%>%
  dplyr::distinct(usubjid, .keep_all =T)%>%
  pivot_wider(id_cols=usubjid, names_from = intrt,  values_from = covid_vacc)
#renaming vacc brands
vacc_distinct<- vaccadmifinal%>%
  dplyr::distinct(usubjid, .keep_all =T)%>%
  mutate_at(c("type_dose_1", "type_dose_2", "type_dose_3", "type_dose_4"),
            funs(recode(., "1"="AstraZeneca", "2"="Covaxin",
                        "3"="Johnson & Johnson", "4"="Moderna",
                        "5"="Novavax", "6"="Pfizer",
                        "7"="Sinopharm", "8"="Sinovac",
                        "9"="Sputnik V", "10"="Unknown vaccine", 
                        "COVID-19 VACCINE CANSINBIO"="Casinbio",.default = NULL))) 
vacc_final <- input.tbl%>%
  left_join(vacc_distinct, by = c("usubjid"))

#Removing some unnecessary and duplicated attributes
vacc_final <-  vacc_final[,!(names(vacc_final) %in% c("date_admit.y","studyid.y","inrefid","zero_ind","intrt","inmodify", "dateamend","conflict_date1","conflict_date2", "inpresp", "inoccur","instdtc","indtc","number_visits"))]

#Grouping outcome variables
vacc_final[which(vacc_final$outcome == "ongoing care"),]$outcome <- "LTFU"
vacc_final[which(vacc_final$outcome == "transferred"),]$outcome <- "LTFU"
vacc_final[which(vacc_final$outcome == "unknown outcome"),]$outcome <- "LTFU"
vacc_final[which(vacc_final$outcome == "forwarding to home"),]$outcome <- "LTFU"
vacc_final[which(vacc_final$outcome == "lost to follow-up"),]$outcome <- "LTFU"
vacc_final[which(vacc_final$outcome == "death"),]$outcome <- "Death"
vacc_final[which(vacc_final$outcome == "discharge"),]$outcome <- "Discharge"
vacc_final[which(vacc_final$outcome == "palliative care"),]$outcome <- "LTFU"

vacc_only <- subset(vacc_final, ref=="1")

save(vacc_only, file = "vacc_only.rda")
save(vacc_final,  file="vacc_final.rda")
write.csv(vacc_final,file='vacc_final2.csv')