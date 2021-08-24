##### load libraries ####
library(checkpoint) #for package version control
checkpoint("2021-07-28") #when the code is run packages are the same version as when code was created
library(here) #set relative working directory
library(tidyverse) #for data cleaning
library(naniar) #for changing values to na
library(lubridate) #for converting dates
library(lme4)#for statistical analysis - logistic regression
library(sjstats) #to calculate intraclass correlation coefficient
library(sjPlot) #for model visualisation
here()

#### paths to raw data ####
person <- here("rawdata", "DSET18", "IAPTDataSetPerson.csv") #person dataframe
referral <- here("rawdata", "DSET18", "IAPTDataSetReferral.csv") #referral dataframe
appointment <- here("rawdata", "DSET18", "IAPTDataSetAppointment.csv") #appointment dataframe
postcode <- here("rawdata", "DSET18", "postcodes.csv") #postcode dataframe

#### general data preparation - prepared collaboratively ####

#### person dataframe --------------------------------------

##load data
persondf <- read.csv(person) 

##select and rename variables
persondf <- select(persondf, -OrgCodeProvider, -ReligiousGroup, -BAFIndicator)

##correct vector class
#check vector class
summary(persondf)
#for categorical variables represented by numbers under 10, convert to numeric then character
#this is to remove any preceeding 0s and ensure code can be reused
persondf <- persondf %>%
  #convert gender to numeric
  mutate(Gender = as.numeric(Gender)) %>%
  #convert gender to character
  mutate(Gender = as.character(Gender))

##replace miscoded or NUll values with NA
persondf <- persondf %>%
  #for categorical data, change values that do not correspond to the codebook to NA
  #if codebook stated value = "unknown" and doesn't specify patient was asked also change to NA
  mutate(Gender = ifelse(Gender == "1" | Gender == "2" | Gender == "9", Gender, NA)) %>%
  mutate(Ethnicity = ifelse(Ethnicity == "A" | Ethnicity == "B" | Ethnicity == "C" |
                              Ethnicity == "D" | Ethnicity == "E" | Ethnicity == "F" |
                              Ethnicity == "G" | Ethnicity == "H" | Ethnicity == "J" |
                              Ethnicity == "K" | Ethnicity == "L" | Ethnicity == "M" |
                              Ethnicity == "N" | Ethnicity == "P" | Ethnicity == "R" |
                              Ethnicity == "S" | Ethnicity == "Z", Ethnicity, NA)) %>%
  mutate(SexualOrientation = ifelse(SexualOrientation == "1" | SexualOrientation == "2" |
                                      SexualOrientation == "3" | SexualOrientation == "4" |
                                      SexualOrientation == "Z", SexualOrientation, NA)) %>%
  mutate(LTCondition = ifelse(LTCondition == "Y" | LTCondition == "N" |
                                LTCondition == "U" | LTCondition == "Z", LTCondition, NA)) %>%
  #where codebook doesn't specify valid values, replace "NULL" with NA
  replace_with_na(replace = list(LPTID = "NULL",
                                 Postcode = "NULL",
                                 GMPC = "NULL",
                                 AgeAtReferral = "NULL"))

##Collapse ethnicity values
persondf <- persondf %>%
  mutate(Ethnicity = case_when(Ethnicity == "A" ~ "White",
                               Ethnicity == "B" ~ "White",
                               Ethnicity == "C" ~ "White",
                               Ethnicity == "D" ~ "Mixed",
                               Ethnicity == "E" ~ "Mixed",
                               Ethnicity == "F" ~ "Mixed",
                               Ethnicity == "G" ~ "Mixed",
                               Ethnicity == "H" ~ "Asian",
                               Ethnicity == "J" ~ "Asian",
                               Ethnicity == "K" ~ "Asian",
                               Ethnicity == "L" ~ "Asian",
                               Ethnicity == "M" ~ "Black",
                               Ethnicity == "N" ~ "Black",
                               Ethnicity == "P" ~ "Black",
                               Ethnicity == "R" ~ "Other Ethnicity",
                               Ethnicity == "S" ~ "Other Ethnicity",
                               Ethnicity == "Z" ~ "Not stated"))

##collapse dataframe so each patient ID appears only once
collapsedpersondf <- persondf %>%
  group_by(LPTID) %>%
  #select row which has the earliest age at referral
  slice(which.min(AgeAtReferral)) %>%
  ungroup()

#### referral dataframe -----------------------------------
##load data
referraldf <- read.csv(referral)

##select and rename variables
referraldf <- select(referraldf, -OrgCodeProvider, -OptInDate, -ServReqAccInd, -OrgCodeComm, -OrgCodeProvidStepTo, -CareCluster)
##check and correcting incorrect vector class
summary(referraldf)
#convert RefRecDate, OnsetDate and EndDate to date
referraldf <- referraldf %>%
  mutate(RefRecDate = as.Date(RefRecDate, "%d/%m/%Y"),
         OnsetDate = ym(OnsetDate), #raw data does not include day so has defaulted to the 1st of the month)
         EndDate = as.Date(EndDate, "%d/%m/%Y"))

##replace miscoded or NUll values with NA
#for categorical data, change values that do not correspond to the codebook to NA
#if codebook stated value = "unknown" and doesn't specify patient was asked also change to NA
referraldf <- referraldf %>%
  mutate(SourceReferral = ifelse(SourceReferral == "A1" | SourceReferral == "A2" |
                                   SourceReferral == "A3" | SourceReferral == "B1" |
                                   SourceReferral == "B2" | SourceReferral == "C1" |
                                   SourceReferral == "C2" | SourceReferral == "D1" |
                                   SourceReferral == "E1" | SourceReferral == "E2" |
                                   SourceReferral == "E3" | SourceReferral == "E4" |
                                   SourceReferral == "E5" | SourceReferral == "F1" |
                                   SourceReferral == "F1" | SourceReferral == "F2" |
                                   SourceReferral == "F3" | SourceReferral == "G1" |
                                   SourceReferral == "G2" | SourceReferral == "G3" |
                                   SourceReferral == "G4" | SourceReferral == "H1" |
                                   SourceReferral == "H2" | SourceReferral == "I1" |
                                   SourceReferral == "I2" | SourceReferral == "J1" |
                                   SourceReferral == "J2" | SourceReferral == "J3" |
                                   SourceReferral == "J4" | SourceReferral == "K1" |
                                   SourceReferral == "K1" | SourceReferral == "K2" |
                                   SourceReferral == "K3" | SourceReferral == "K4" |
                                   SourceReferral == "K5" | SourceReferral == "L1" |
                                   SourceReferral == "L2" | SourceReferral == "M1" |
                                   SourceReferral == "M2" | SourceReferral == "M3" |
                                   SourceReferral == "M4" | SourceReferral == "M5" |
                                   SourceReferral == "M6" | SourceReferral == "N1" |
                                   SourceReferral == "N2", SourceReferral, NA)) %>%
  mutate(RecurInd = ifelse(RecurInd == "Y" | RecurInd == "N" |
                             RecurInd == "U" | RecurInd == "Z", RecurInd, NA)) %>%
  mutate(EndCode = ifelse(EndCode == "10" | EndCode == "11" | EndCode == "12" |
                            EndCode == "13" | EndCode == "14" | EndCode == "15" |
                            EndCode == "97" | EndCode == "40" | EndCode == "41" |
                            EndCode == "42" | EndCode == "43" | EndCode == "44" |
                            EndCode == "45" | EndCode == "98", EndCode, NA)) %>%
  #where codebook doesn't specify valid values, replace "NULL" with NA
  replace_with_na(replace = list(LPTID = "NULL",
                                 ServiceID = "NULL",
                                 RefRecDate = "NULL",
                                 ProvDiag = "NULL",
                                 OnsetDate = "NULL",
                                 EndDate = "NULL"))

##changing "NULL" and miscoded/unknown values to NA
referraldf <- referraldf %>%
  replace_with_na(replace = list(LPTID = "NULL",
                                 ServiceID = "NULL",
                                 RefRecDate = "NULL",
                                 SourceReferral = c("RHA", "NULL"),
                                 ProvDiag = "NULL",
                                 OnsetDate = "NULL",
                                 RecurInd = "NULL",
                                 EndCode = "NULL",
                                 EndDate = "NULL"))

##create new column for number of referrals 
referraldf <- referraldf %>%
  #counts number of rows each LPTID appears
  add_count(LPTID, name = "NumberofReferrals")

##create approximate age at referral for patients with multiple referrals
#select patients with multiple referrals
multireferraldf <- referraldf %>%
  #include if more than one referral
  filter(NumberofReferrals > 1) %>%
  #extract year of referral
  mutate(ReferralYear = year(RefRecDate))
#subset multiple referral df to earliest referral for each patient
collapsedreferraldf <- multireferraldf %>%
  #group by patient
  group_by(LPTID) %>%
  #retain earliest referral date
  slice(which.min(RefRecDate)) %>%
  ungroup() %>%
  #select required variables
  select(LPTID, RefRecDate, ReferralYear)
#merge collapsed referral and collapsed person df
collapsedreferralpersondf <- inner_join(collapsedreferraldf, collapsedpersondf, by = "LPTID")
#calculate approximate birth year
collapsedreferralpersondf <- collapsedreferralpersondf %>%
  mutate(ApproxBirthYear = ReferralYear - AgeAtReferral)

##separate multi-referral dataframe into earliest referral and later referrals
firstreferraldf <- semi_join(multireferraldf, collapsedreferralpersondf) #first referral
laterreferralsdf <- anti_join(multireferraldf, collapsedreferralpersondf) #later referrals
#merge earliest referral with collapsed multi referral person df
mergedfirstreferral <- inner_join(firstreferraldf, collapsedreferralpersondf) #joined by LPTID and referral date and year
#add approximate age at referral
mergedfirstreferral <- mergedfirstreferral %>%
  mutate(ApproxAgeAtReferral = AgeAtReferral) %>%
  #remove referral and birth year
  select(-ReferralYear, -ApproxBirthYear)
#remove age at referral and referral date/year from collapsed referral person df
collapsedreferralpersondf <- collapsedreferralpersondf %>%
  select(-AgeAtReferral, -RefRecDate, -ReferralYear)
#merge later referrals with collapsed multi referral person df
mergedlaterreferrals <- inner_join(laterreferralsdf, collapsedreferralpersondf, by = "LPTID") #merged by LPTID
#calculate approximate age at referral
mergedlaterreferrals <- mergedlaterreferrals %>%
  mutate(ApproxAgeAtReferral = ReferralYear - ApproxBirthYear) %>%
  #remove approximate birth year and referral year
  select(-ApproxBirthYear, -ReferralYear)

##copy age at referral to approximate age at referral for patients with one referral
#remove patients with multiple referrals
singlereferraldf <- referraldf %>%
  filter(NumberofReferrals == 1)
#merge single referral with collapsedpersondf retaining only patients with single referrals
mergedsinglereferraldf <- inner_join(singlereferraldf, collapsedpersondf, by = "LPTID")
#add approximate age at referral
mergedsinglereferraldf <- mergedsinglereferraldf %>%
  mutate(ApproxAgeAtReferral = AgeAtReferral)

##merge single and multiple referral/person data back into a single dataframe
finaldf <- bind_rows(mergedsinglereferraldf, mergedfirstreferral, mergedlaterreferrals)

#### postcode dataframe --------------------------------------------
##load data
postcodedf <- read.csv(postcode)

##select and rename variables
#lists of old and new variable names
oldnamespostcode <- c("Postcode", "Index.of.Multiple.Deprivation.Decile", "Income.Decile", "Employment.Decile", 
                      "Health.and.Disability.Decile", "Crime.Decile", "Barriers.to.Housing.and.Services.Decile",
                      "Living.Environment.Decile", "IDACI.Decile", "IDAOPI.Decile")
newnamespostcode <- c("Postcode", "IMD", "Income", "Employment", "HealthandDisability", "Crime", 
                      "BarrierstoHousingandServices", "LivingEnvironment", "IDACI", "IDAOPI")
#select variables
postcodedf<- select(postcodedf, Postcode, Index.of.Multiple.Deprivation.Decile, Income.Decile, Employment.Decile, Employment.Decile,
                    Health.and.Disability.Decile, Crime.Decile,Barriers.to.Housing.and.Services.Decile, Living.Environment.Decile, IDACI.Decile,
                    IDAOPI.Decile) %>%
  #rename variables
  rename_with(~ newnamespostcode [which(oldnamespostcode == .x)], oldnamespostcode)

##check and correct incorrect vector class
summary(postcodedf) #all vector classification correct

## Collapse deciles into quintiles for all decile variables
postcodedf <- postcodedf %>%
  mutate(IMD = case_when(IMD == "1" ~ "1",
                         IMD == "2" ~ "1",
                         IMD == "3" ~ "2",
                         IMD == "4" ~ "2",
                         IMD == "5" ~ "3",
                         IMD == "6" ~ "3",
                         IMD == "7" ~ "4",
                         IMD == "8" ~ "4",
                         IMD == "9" ~ "5",
                         IMD == "10" ~ "5")) %>%
  mutate(Income = case_when(Income == "1" ~ "1",
                            Income == "2" ~ "1",
                            Income == "3" ~ "2",
                            Income == "4" ~ "2",
                            Income == "5" ~ "3",
                            Income == "6" ~ "3",
                            Income == "7" ~ "4",
                            Income == "8" ~ "4",
                            Income == "9" ~ "5",
                            Income == "10" ~ "5")) %>%
  mutate(Employment = case_when(Employment == "1" ~ "1",
                                Employment == "2" ~ "1",
                                Employment == "3" ~ "2",
                                Employment == "4" ~ "2",
                                Employment == "5" ~ "3",
                                Employment == "6" ~ "3",
                                Employment == "7" ~ "4",
                                Employment == "8" ~ "4",
                                Employment == "9" ~ "5",
                                Employment == "10" ~ "5")) %>%
  mutate(HealthandDisability = case_when(HealthandDisability == "1" ~ "1",
                                         HealthandDisability == "2" ~ "1",
                                         HealthandDisability == "3" ~ "2",
                                         HealthandDisability == "4" ~ "2",
                                         HealthandDisability == "5" ~ "3",
                                         HealthandDisability == "6" ~ "3",
                                         HealthandDisability == "7" ~ "4",
                                         HealthandDisability == "8" ~ "4",
                                         HealthandDisability == "9" ~ "5",
                                         HealthandDisability == "10" ~ "5")) %>%
  mutate(Crime = case_when(Crime == "1" ~ "1",
                           Crime == "2" ~ "1",
                           Crime == "3" ~ "2",
                           Crime == "4" ~ "2",
                           Crime == "5" ~ "3",
                           Crime == "6" ~ "3",
                           Crime == "7" ~ "4",
                           Crime == "8" ~ "4",
                           Crime == "9" ~ "5",
                           Crime == "10" ~ "5")) %>%
  mutate(BarrierstoHousingandServices = case_when(BarrierstoHousingandServices == "1" ~ "1",
                                                  BarrierstoHousingandServices == "2" ~ "1",
                                                  BarrierstoHousingandServices == "3" ~ "2",
                                                  BarrierstoHousingandServices == "4" ~ "2",
                                                  BarrierstoHousingandServices == "5" ~ "3",
                                                  BarrierstoHousingandServices == "6" ~ "3",
                                                  BarrierstoHousingandServices == "7" ~ "4",
                                                  BarrierstoHousingandServices == "8" ~ "4",
                                                  BarrierstoHousingandServices == "9" ~ "5",
                                                  BarrierstoHousingandServices == "10" ~ "5")) %>%
  mutate(LivingEnvironment = case_when(LivingEnvironment == "1" ~ "1",
                                       LivingEnvironment == "2" ~ "1",
                                       LivingEnvironment == "3" ~ "2",
                                       LivingEnvironment == "4" ~ "2",
                                       LivingEnvironment == "5" ~ "3",
                                       LivingEnvironment == "6" ~ "3",
                                       LivingEnvironment == "7" ~ "4",
                                       LivingEnvironment == "8" ~ "4",
                                       LivingEnvironment == "9" ~ "5",
                                       LivingEnvironment == "10" ~ "5")) %>%
  mutate(IDACI = case_when(IDACI == "1" ~ "1",
                           IDACI == "2" ~ "1",
                           IDACI == "3" ~ "2",
                           IDACI == "4" ~ "2",
                           IDACI == "5" ~ "3",
                           IDACI == "6" ~ "3",
                           IDACI == "7" ~ "4",
                           IDACI == "8" ~ "4",
                           IDACI == "9" ~ "5",
                           IDACI == "10" ~ "5")) %>%
  mutate(IDAOPI = case_when(IDAOPI == "1" ~ "1",
                            IDAOPI == "2" ~ "1",
                            IDAOPI == "3" ~ "2",
                            IDAOPI == "4" ~ "2",
                            IDAOPI == "5" ~ "3",
                            IDAOPI == "6" ~ "3",
                            IDAOPI == "7" ~ "4",
                            IDAOPI == "8" ~ "4",
                            IDAOPI == "9" ~ "5",
                            IDAOPI == "10" ~ "5"))


##replace miscoded or NUll values with NA
postcodedf <- postcodedf %>%
  #for categorical data, change values that do not correspond to the codebook to NA
  mutate(IMD = ifelse(IMD == 1 | IMD == 2 | IMD == 3 | IMD == 4 | IMD == 5, IMD, NA)) %>%
  mutate(Income = ifelse(Income == 1 | Income == 2 | Income == 3 | Income == 4 |
                           Income == 5, Income, NA)) %>%
  mutate(Employment = ifelse(Employment == 1 | Employment == 2 | Employment == 3 |
                               Employment == 4 | Employment == 5, Employment, NA)) %>%
  mutate(HealthandDisability = ifelse(HealthandDisability == 1 | HealthandDisability == 2 |
                                        HealthandDisability == 3 | HealthandDisability == 4 |
                                        HealthandDisability == 5,
                                      HealthandDisability, NA)) %>%
  mutate(Crime = ifelse(Crime == 1 | Crime == 2 | Crime == 3 | Crime == 4 |
                          Crime == 5, Crime, NA)) %>%
  mutate(BarrierstoHousingandServices = ifelse(BarrierstoHousingandServices == 1 |
                                                 BarrierstoHousingandServices == 2 |
                                                 BarrierstoHousingandServices == 3 |
                                                 BarrierstoHousingandServices == 4 |
                                                 BarrierstoHousingandServices == 5,
                                               BarrierstoHousingandServices, NA)) %>%
  mutate(LivingEnvironment = ifelse(LivingEnvironment == 1 | LivingEnvironment == 2 |
                                      LivingEnvironment == 3 | LivingEnvironment == 4 |
                                      LivingEnvironment == 5,
                                    LivingEnvironment, NA)) %>%
  mutate(IDACI = ifelse(IDACI == 1 | IDACI == 2 | IDACI == 3 | IDACI == 4 |
                          IDACI == 5, IDACI, NA)) %>%
  mutate(IDAOPI = ifelse(IDAOPI == 1 | IDAOPI == 2 | IDAOPI == 3 | IDAOPI == 4 |
                           IDAOPI == 5, IDAOPI, NA)) %>%
  #where codebook doesn't specify valid values, replace "NULL" with NA
  replace_with_na(replace = list(Postcode = "NULL"))


##add postcode data to merged dataframe
finaldf<-left_join(finaldf, postcodedf, by = "Postcode")
#remove postcode variable
finaldf <- finaldf %>%
  select(-Postcode)

#### appointment dataframe ------------------------------------
##load data
appointmentdf <- read.csv(appointment)

##select and rename variables
#lists of old and new variables names
oldnamesappointment <- c("PHQ9_Score", "Appointment", "Current.Assigned.Worker")
newnamesappointment <- c("PHQ9Score", "AppointmentDate", "AssignedWorker")

#select variables
appointmentdf <- select(appointmentdf, ServiceID, Appointment, StepIntensity, PrimaryRole, Attendance, Apptype, Consmedium,
                        FFComm, num_range("Thertype", 1:2), EmpStatus, PsychMed, PHQ9_Score:WSASRScore, AgoraScore, 
                        SocPhobiaScore:Current.Assigned.Worker) %>%
  #rename variables
  rename_with(~ newnamesappointment [which(oldnamesappointment == .x)], oldnamesappointment)


##check and correct incorrect vector class
summary(appointmentdf)
#convert clinical outcome scores to numerical
i <- c(13:22)
appointmentdf[ , i] <- apply(appointmentdf[ , i], 2,
                             function(x) as.numeric(x))
appointmentdf <- appointmentdf %>%
  #convert Appointment to date
  mutate(AppointmentDate = as.Date(AppointmentDate, "%d/%m/%Y")) %>%
  #for categorical variables represented by numbers under 10, convert to numeric then character
  #convert affected variables to numeric
  mutate(StepIntensity = as.numeric(StepIntensity),
         Attendance = as.numeric(Attendance),
         Apptype = as.numeric(Apptype),
         Consmedium = as.numeric(Consmedium),
         FFComm = as.numeric(FFComm)) %>%
  #convert affected variables to character
  mutate(StepIntensity = as.character(StepIntensity),
         Attendance = as.character(Attendance),
         Apptype = as.character(Apptype),
         Consmedium = as.character(Consmedium),
         FFComm = as.character(FFComm)) %>%
  #convert step intensity to factor
  mutate(StepIntensity = as.factor(StepIntensity))


##replace miscoded or NUll values with NA
appointmentdf <- appointmentdf %>%
  #for categorical data, change values that do not correspond to the codebook to NA
  mutate(StepIntensity = ifelse(StepIntensity == "1" | StepIntensity == "2" |
                                  StepIntensity == "3" | StepIntensity == "4",
                                StepIntensity, NA)) %>%
  mutate(PrimaryRole = ifelse(PrimaryRole == "10" | PrimaryRole == "11" |
                                PrimaryRole == "12" | PrimaryRole == "13" |
                                PrimaryRole == "14" | PrimaryRole == "15" |
                                PrimaryRole == "16" | PrimaryRole == "17" |
                                PrimaryRole == "40" | PrimaryRole == "41" |
                                PrimaryRole == "42" | PrimaryRole == "43" |
                                PrimaryRole == "44" | PrimaryRole == "45" |
                                PrimaryRole == "46" | PrimaryRole == "47" |
                                PrimaryRole == "48" | PrimaryRole == "49" |
                                PrimaryRole == "50", PrimaryRole, NA)) %>%
  mutate(Attendance = ifelse(Attendance == "2" | Attendance == "3" | Attendance == "4" |
                               Attendance == "5" | Attendance == "6" | Attendance == "7",
                             Attendance, NA)) %>%
  mutate(Apptype = ifelse(Apptype == "1" | Apptype == "2" | Apptype == "3" |
                            Apptype == "4" | Apptype == "5" | Apptype == "6" |
                            Apptype == "7", Apptype, NA)) %>%
  mutate(Consmedium = ifelse(Consmedium == "1" | Consmedium == "2" | Consmedium == "3" |
                               Consmedium == "4" | Consmedium == "5" | Consmedium == "6",
                             Consmedium, NA)) %>%
  mutate(FFComm = ifelse(FFComm == "1" | FFComm == "2" | FFComm == "3", FFComm, NA)) %>%
  mutate(Thertype1 = ifelse(Thertype1 == "20" | Thertype1 == "21" | Thertype1 == "22" |
                              Thertype1 == "23" | Thertype2 == "24" | Thertype1 == "25" |
                              Thertype1 == "26" | Thertype1 == "27" | Thertype1 == "28" |
                              Thertype1 == "29" | Thertype1 == "40" | Thertype1 == "41" |
                              Thertype1 == "42" | Thertype1 == "43" | Thertype1 == "44" |
                              Thertype1 == "45" | Thertype1 == "46" | Thertype1 == "47" |
                              Thertype1 == "48" | Thertype1 == "49" | Thertype1 == "50" |
                              Thertype1 == "51", Thertype1, NA)) %>%
  mutate(Thertype2 = ifelse(Thertype2 == "20" | Thertype2 == "21" | Thertype2 == "22" |
                              Thertype2 == "23" | Thertype2 == "24" | Thertype2 == "25" |
                              Thertype2 == "26" | Thertype2 == "27" | Thertype2 == "28" |
                              Thertype2 == "29" | Thertype2 == "40" | Thertype2 == "41" |
                              Thertype2 == "42" | Thertype2 == "43" | Thertype2 == "44" |
                              Thertype2 == "45" | Thertype2 == "46" | Thertype2 == "47" |
                              Thertype2 == "48" | Thertype2 == "49" | Thertype2 == "50" |
                              Thertype2 == "51", Thertype2, NA)) %>%
  mutate(EmpStatus = ifelse(EmpStatus == "01" | EmpStatus == "02" | EmpStatus == "03" |
                              EmpStatus == "04" | EmpStatus == "05" | EmpStatus == "06" |
                              EmpStatus == "07" | EmpStatus == "08" | EmpStatus == "ZZ",
                            EmpStatus, NA)) %>%
  mutate(PsychMed = ifelse(PsychMed == "01" | PsychMed == "02" | PsychMed == "03" |
                             PsychMed == "UU" | PsychMed == "ZZ", PsychMed, NA)) %>%
  mutate(PHQ9Score = ifelse(PHQ9Score >= 0 & PHQ9Score <= 27, PHQ9Score, NA)) %>%
  mutate(GAD7Score = ifelse(GAD7Score >= 0 & GAD7Score <= 21, GAD7Score, NA)) %>%
  mutate(WSASWScore = ifelse(WSASWScore >= 0 & WSASWScore <= 9, WSASWScore, NA)) %>%
  mutate(WSASHmScore = ifelse(WSASHmScore >= 0 & WSASHmScore <= 8, WSASHmScore, NA)) %>%
  mutate(WSASSlaScore = ifelse(WSASSlaScore >= 0 & WSASSlaScore <= 8, WSASSlaScore, NA)) %>%
  mutate(WSASPlaScore = ifelse(WSASPlaScore >= 0 & WSASPlaScore <= 8, WSASPlaScore, NA)) %>%
  mutate(WSASRScore = ifelse(WSASRScore >= 0 & WSASRScore <= 8, WSASRScore, NA)) %>%
  mutate(AgoraScore = ifelse(AgoraScore >= 0 & AgoraScore <= 8, AgoraScore, NA)) %>%
  mutate(SocPhobiaScore = ifelse(SocPhobiaScore >= 0 & SocPhobiaScore <= 8, SocPhobiaScore, NA)) %>%
  mutate(SpecPhobiaScore = ifelse(SpecPhobiaScore >= 0 & SpecPhobiaScore <= 8, SpecPhobiaScore, NA)) %>%
  #where codebook doesn't specify valid values, replace "NULL" with NA
  replace_with_na(replace = list(ServiceID = "NULL",
                                 AppointmentDate = "NULL",
                                 AssignedWorker = "NULL"))


##create a total WSAS score variable
#where the WSASWScore was not applicable, replace with mean of the other WSAS score values
appointmentdf<- appointmentdf %>%
  mutate(WSASWScore = ifelse(WSASWScore == 9, rowMeans(appointmentdf[,19:22]), WSASWScore))
#merge WSAS scores and remove individual columns
appointmentdf$WSASScore <- rowSums(appointmentdf[,15:19])
appointmentdf <- select(appointmentdf, -WSASWScore, -WSASHmScore, -WSASSlaScore, -WSASPlaScore, -WSASRScore)

##remove appointments that were not attended, occurred before referral or after the end of referral
#create df for referral dates
referraldates <- select(referraldf, EndDate, ServiceID, RefRecDate)
#join to appointmentdf
appointmentdf <- left_join(appointmentdf, referraldates, by = "ServiceID") %>%
  #remove rows where appointment was after the end of referral
  filter(AppointmentDate < EndDate | AppointmentDate == EndDate) %>%
  #remove rows where appointment was before the referral
  filter(AppointmentDate > RefRecDate | AppointmentDate == RefRecDate) %>%
  #only include data from appointments that were attended
  subset(Attendance == c("5", "6")) %>%
  #remove extraneous variables
  select(-EndDate, -RefRecDate, -Attendance)

##create an earliest appointment dataframe
earliestappointment <- appointmentdf  %>%
  #remove extraneous variables
  select(ServiceID, AppointmentDate, EmpStatus:SpecPhobiaScore, WSASScore)
#include appointment data from the earliest appointment only
earliestappointment <- earliestappointment %>%
  group_by(ServiceID) %>%
  #retains the row from the earliest appointment date for each ServiceID group
  slice(which.min(AppointmentDate)) %>%
  ungroup() %>%
  #rename variables to signal data is from the initial assessment appointment
  rename_with(~ paste("EA", .x, sep = "_"), AppointmentDate:WSASScore)

##add initial assessment information to final dataframe
finaldf <- left_join(finaldf, earliestappointment, by = "ServiceID")

##create treatment appointment dataframe
#subset appointmentdf to include only treatment appointments
treatment <- subset(appointmentdf, Apptype == c("2", "3", "5")) %>%
  #remove extraneous variables
  select(-Apptype, -EmpStatus, -PsychMed)
##Create number of treatment sessions variables
treatment <- treatment %>%
  #group rows by ServiceID
  group_by(ServiceID) %>%
  #add number of treatment appointments variable
  mutate(NumberofTreatmentAppts = n()) %>% #counts number of rows per group
  #add number of face to face appointments variable
  mutate(NumberofF2FAppts = sum(Consmedium == "1")) %>%
  #add number of one to one F2F appointments variable
  mutate(Numberof1to1F2FAppts = sum(FFComm == "1")) %>%
  
  ##Create variable that tells us whether referrals have received one type or mixed treatments
  mutate(SingleMixedTreatment = case_when(
    #if therapy type 2 column contains information, treatment type is mixed
    is.na(Thertype2) == "FALSE" ~ "Mixed",
    #if there is more than one unique Thertype1 value per ServiceID it is mixed
    n_distinct(Thertype1) > 1 ~ "Mixed",
    #if there is one unique Thertype1 value per Service ID it is single
    n_distinct(Thertype1) == 1 ~ "Single")) %>%
  #if SingleMixedTreatment variable does not match across ServiceID, amend value to "Mixed"
  mutate(SingleMixedTreatment = ifelse(n_distinct(SingleMixedTreatment) > 1, "Mixed", SingleMixedTreatment)) %>%
  
  ##Add variable that tells us if treatment was carried out by Trainees, Qualified Professionals, Both or Unknown
  mutate(PrimaryRoleTraining = ifelse(PrimaryRole > 39, "Qualified", "Trainee")) %>%
  #if ServiceID group has two unique values (Qualified and Trainee), amend to "Both"
  #ignores NA values when checking for distinct PrimaryRoleType values
  mutate(PrimaryRoleTraining = ifelse(n_distinct(PrimaryRoleTraining, na.rm = TRUE) > 1, "Both", PrimaryRoleTraining)) %>%
  #replace NA values with "Unknown"
  mutate(PrimaryRoleTraining = replace_na(PrimaryRoleTraining, "Unknown")) %>%
  #if any PrimaryRoleTraining value in a group is "Both" replace all other values in group with "Both"
  mutate(PrimaryRoleTraining = ifelse(any(PrimaryRoleTraining == "Both"), "Both", PrimaryRoleTraining)) %>%
  #if any PrimaryRoleTraining value in a group is "Unknown", replace all other values in group with "Unknown"
  mutate(PrimaryRoleTraining = ifelse(any(PrimaryRoleTraining == "Unknown"), "Unknown", PrimaryRoleTraining)) %>%
  #remove variables that are no longer required
  select(-FFComm, -Consmedium, -PrimaryRole) 

##create an initial treatment appointment dataframe
initialtreatment <- treatment %>%
  #retains the row from the earliest appointment date for each ServiceID group
  slice(which.min(AppointmentDate)) %>%
  ungroup() %>%
  #remove step intensity column
  select(-StepIntensity) %>%
  #rename variables to signal data is from the initial treatment appointment
  rename_with(~ paste("IT", .x, sep = "_"), AppointmentDate:WSASScore)

#merge initial treatment dataframe with final dataframe
finaldf <- left_join(finaldf, initialtreatment, by = "ServiceID")  

#create a final treatment appointment dataframe
finaltreatment <- treatment %>%
  #remove variables that are not required
  select(ServiceID:StepIntensity, PHQ9Score:SpecPhobiaScore, WSASScore) %>%
  #retains the row from the latest appointment date for each ServiceID group
  slice(which.max(AppointmentDate)) %>%
  ungroup() %>%
  #rename variables to signal data is from the final treatment appointment
  rename_with(~ paste("FT", .x, sep = "_"), AppointmentDate:WSASScore)

##merge final treatment dataframe with final dataframe
finaldf <- left_join(finaldf, finaltreatment, by = "ServiceID")

##for NumberofAppointment variables replace NAs with 0s
finaldf<- finaldf %>%
  replace_na(list(NumberofTreatmentAppts = 0, NumberofF2FAppts = 0, Numberof1to1F2FAppts = 0))

##remove all dataframes from environment except finaldf
rm(appointmentdf, collapsedpersondf, collapsedreferraldf, collapsedreferralpersondf, 
   finaltreatment, firstreferraldf, earliestappointment, initialtreatment,
   laterreferralsdf, mergedfirstreferral, mergedlaterreferrals, mergedsinglereferraldf,
   multireferraldf, persondf, postcodedf, referraldates, referraldf, singlereferraldf, treatment)


#### Preparation for analysis - prepared individually --------------------

## Select variables needed for analysis
finaldf <- finaldf %>%
  select(LPTID: RefRecDate, NumberofReferrals:Ethnicity, LTCondition, AgeAtReferral, IMD:EA_WSASScore, 
         IT_Thertype1:PrimaryRoleTraining, FT_StepIntensity:FT_WSASScore)

finaldf <- finaldf %>%
  #create change in WSAS Score variable
  mutate(WSASChange = FT_WSASScore - EA_WSASScore) %>%
  #create change in PHQ9 score variable
  mutate(PHQ9Change = FT_PHQ9Score - EA_PHQ9Score) %>%
  #create change in GAD7 score variable
  mutate(GAD7Change = FT_GAD7Score - EA_GAD7Score)

#Convert required variables from character to factor
finaldf <- finaldf %>%
  mutate(IMD = as.factor(IMD),
         Income = as.factor(Income),
         Employment = as.factor(Employment),
         HealthandDisability = as.factor(HealthandDisability),
         Crime = as.factor(Crime),
         BarrierstoHousingandServices = as.factor(BarrierstoHousingandServices),
         LivingEnvironment = as.factor(LivingEnvironment),
         IDACI = as.factor(IDACI),
         IDAOPI = as.factor(IDAOPI), 
         EA_EmpStatus = as.factor(EA_EmpStatus),
         EA_PsychMed = as.factor(EA_PsychMed),
         IT_Thertype1 = as.factor(IT_Thertype1),
         IT_Thertype2 = as.factor(IT_Thertype2),
         FT_StepIntensity = as.factor(FT_StepIntensity),
         NumberofF2FAppts = as.factor(NumberofF2FAppts))

#Create categorical variable 'Multiple Referrals' with options '1' meaning 'yes' or '0' meaning 'no'
finaldf <- finaldf %>%
  mutate(MultipleReferrals = if_else(NumberofReferrals == 1, "0", "1")) %>% #Any patients that have only one referral will return the value 'no', those with multiple referrals will return the value 'yes'
  #Convert variable from character to factor so it is treated as categorical
  mutate(MultipleReferrals = as.factor(MultipleReferrals)) %>%
  #Convert Gender variable to factor so it is treated as categorical
  mutate(Gender = as.factor(Gender)) %>%
  #Convert SingleMixedTreatment variable from character to categorical
  # '0' = single, '1' = mixed
  mutate(SingleMixedTreatment = case_when(SingleMixedTreatment == "Single" ~ "0", 
                                          SingleMixedTreatment == "Mixed" ~ "1")) %>%
  #Convert SingleMixedTreatment variable from character to factor
  mutate(SingleMixedTreatment = as.factor(SingleMixedTreatment)) %>%
  #Convert PrimaryRoleTraining from character to categorical
  # '0' = Trainee, '1' = Qualified, '2' = both
  mutate(PrimaryRoleTraining = case_when(PrimaryRoleTraining == "Trainee" ~ "0",
                                         PrimaryRoleTraining == "Qualified" ~ "1",
                                         PrimaryRoleTraining == "Both" ~ "2")) %>%
  #Convert PrimaryRoleTraining from character to factor
  mutate(PrimaryRoleTraining = as.factor(PrimaryRoleTraining))

#Collapse therapy type into smaller number of categories and rename numbers
finaldf <- finaldf %>%
  mutate(IT_Thertype1 = case_when(IT_Thertype1 == "20" ~ "SelfHelp",
                                  IT_Thertype1 == "21" ~ "SelfHelp",
                                  IT_Thertype1 == "22" ~ "SelfHelp",
                                  IT_Thertype1 == "23" ~ "SelfHelp",
                                  IT_Thertype1 == "24" ~ "LIBehaviouralActivation",
                                  IT_Thertype1 == "25" ~ "StructuredPhysicalActivity",
                                  IT_Thertype1 == "26" ~ "Ante/PostNatalCounselling",
                                  IT_Thertype1 == "27" ~ "PsychoeducationalPeerSupport",
                                  IT_Thertype1 == "28" ~ "OtherLI",
                                  IT_Thertype1 == "29" ~ "EmploymentSupportLI",
                                  IT_Thertype1 == "40" ~ "AppliedRelaxation",
                                  IT_Thertype1 == "41" ~ "HIBehaviouralActivation",
                                  IT_Thertype1 == "42" ~ "CouplesTherapyforDepression",
                                  IT_Thertype1 == "43" ~ "CollabCare",
                                  IT_Thertype1 == "44" ~ "CounsellingforDepression",
                                  IT_Thertype1 == "45" ~ "BriefPsychodynamicPsychotherapy",
                                  IT_Thertype1 == "46" ~ "EMDR",
                                  IT_Thertype1 == "47" ~ "Mindfulness",
                                  IT_Thertype1 == "48" ~ "OtherHI",
                                  IT_Thertype1 == "49" ~ "EmploymentSupportHI",
                                  IT_Thertype1 == "50" ~ "CBT",
                                  IT_Thertype1 == "51" ~ "IPT"))


### Sanity checks before statistical analysis

#Check how many distinct values for assigned worker to ensure that there is a sufficient sample size for multilevel modelling
finaldf %>% 
  summarise(count = n_distinct(IT_AssignedWorker, na.rm = TRUE)) #130 unique values excluding NA values

#Counts of each variable
finaldf %>% group_by(MultipleReferrals) %>% summarize(count=n())
finaldf %>% group_by(IMD) %>% summarize(count=n())
finaldf %>% group_by(IT_Thertype1) %>% summarize(count=n())
finaldf %>% group_by(NumberofF2FAppts) %>% summarize(count=n())
finaldf %>% group_by(FT_StepIntensity) %>% summarize(count=n())
finaldf %>% group_by(Gender) %>% summarize(count=n())


#Remove rows of that only have a count of 1
finaldf <- finaldf[-c(1910, #EMDR
                      15062, #Employment Support LI 
                      14786, #Mindfulness
                      8619, #9 F2F appts
                      14841), ] #12 F2F appts



#### Statistical Analysis ----------------------------------------------------


#Chi Square tests of each variable to investigate relationships between MultipleReferrals and variable
#Demographic variables
chisq.test(finaldf$MultipleReferrals, finaldf$AgeAtReferral, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$Gender, correct = FALSE) #sig
#Environment variables
chisq.test(finaldf$MultipleReferrals, finaldf$IMD, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$Income, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$Employment, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$HealthandDisability, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$Crime, correct = FALSE) #non-sig
chisq.test(finaldf$MultipleReferrals, finaldf$BarrierstoHousingandServices, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$LivingEnvironment, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$IDACI, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$IDAOPI, correct = FALSE) #sig
#Patient Variables Earliest Appointment
chisq.test(finaldf$MultipleReferrals, finaldf$EA_EmpStatus, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$EA_PsychMed, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$EA_PHQ9Score, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$EA_GAD7Score, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$EA_AgoraScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$EA_SocPhobiaScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$EA_SpecPhobiaScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$EA_WSASScore, correct = FALSE) #sig
#Patient Variables Initial Treatment
chisq.test(finaldf$MultipleReferrals, finaldf$IT_PHQ9Score, correct = FALSE) #non-sig
chisq.test(finaldf$MultipleReferrals, finaldf$IT_GAD7Score, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$IT_AgoraScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$IT_SocPhobiaScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$IT_SpecPhobiaScore, correct = FALSE) #non-sig
chisq.test(finaldf$MultipleReferrals, finaldf$IT_WSASScore, correct = FALSE) #sig
#Patient Variables Final Treatment
chisq.test(finaldf$MultipleReferrals, finaldf$FT_PHQ9Score, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$FT_GAD7Score, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$FT_AgoraScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$FT_SocPhobiaScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$FT_SpecPhobiaScore, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$FT_WSASScore, correct = FALSE) #non-sig
#Change score variables
chisq.test(finaldf$MultipleReferrals, finaldf$WSASChange, correct = FALSE) #non-sig
chisq.test(finaldf$MultipleReferrals, finaldf$PHQ9Change, correct = FALSE) #non-sig
chisq.test(finaldf$MultipleReferrals, finaldf$GAD7Change, correct = FALSE) #non-sig
#Appointment Variables
chisq.test(finaldf$MultipleReferrals, finaldf$IT_Thertype1, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$IT_AssignedWorker, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$NumberofTreatmentAppts, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$NumberofF2FAppts, correct = FALSE) #non-sig
chisq.test(finaldf$MultipleReferrals, finaldf$FT_StepIntensity, correct = FALSE) #sig
chisq.test(finaldf$MultipleReferrals, finaldf$SingleMixedTreatment, correct = FALSE) #non-sig
chisq.test(finaldf$MultipleReferrals, finaldf$PrimaryRoleTraining, correct = FALSE) #sig


### Multi level logistic regression models ###

#gender model
gen.model <- glmer(MultipleReferrals ~ Gender + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(gen.model) #non-sig
#age model
age.model <- glmer(MultipleReferrals ~ AgeAtReferral + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(age.model) #non-sig
#IMD model
imd.model <- glmer(MultipleReferrals ~ IMD + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(imd.model) #Sig
#Income model
income.model <- glmer(MultipleReferrals ~ Income + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(income.model) #Sig
#Employment model
employment.model <-  glmer(MultipleReferrals ~ Employment + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(employment.model) #Sig
#Health and Disability model
healthdisability.model <- glmer(MultipleReferrals ~ HealthandDisability + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(healthdisability.model) #non-sig
#Crime model
crime.model <- glmer(MultipleReferrals ~ Crime + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(crime.model) #non-sig
#Barriers to Housing and Services model
housingservices.model <- glmer(MultipleReferrals ~ BarrierstoHousingandServices + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(housingservices.model) #non-sig
#Living Environment model
livingenvironment.model <- glmer(MultipleReferrals ~ LivingEnvironment + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(livingenvironment.model) #non-sig
#IDACI model
idaci.model <- glmer(MultipleReferrals ~ IDACI + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(idaci.model) #sig
#IDAOPI model
idaopi.model <- glmer(MultipleReferrals ~ IDAOPI + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(idaopi.model) #sig
#EA_EmpStatus model
ea.empstatus.model <- glmer(MultipleReferrals ~ EA_EmpStatus + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ea.empstatus.model) #sig
#EA_PsychMed model
ea.psychmed.model <- glmer(MultipleReferrals ~ EA_PsychMed + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ea.psychmed.model) #sig
#Therapy type model
thertype.model <- glmer(MultipleReferrals ~ IT_Thertype1 + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(thertype.model) #sig
#Number of treatment appts model
treatmentappts.model <-  glmer(MultipleReferrals ~ NumberofTreatmentAppts + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(treatmentappts.model) #non sig
#Number of F2F appts
f2fappts.model <-  glmer(MultipleReferrals ~ NumberofF2FAppts + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(f2fappts.model) #sig
#Number of 1-1 F2F appts
f2f1.1.model <- glmer(MultipleReferrals ~ Numberof1to1F2FAppts + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(f2f1.1.model) #sig
#Single/Mixed treatment
singlemixed.model <- glmer(MultipleReferrals ~ SingleMixedTreatment + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(singlemixed.model) #non-sig
#Primary Role of Therapist model
primaryrole.model <- glmer(MultipleReferrals ~ PrimaryRoleTraining + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(primaryrole.model) #non-sig
#FT Step Intensity model
ftstepintensity.model <- glmer(MultipleReferrals ~ FT_StepIntensity + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ftstepintensity.model) #sig

#Outcome measures from earliest appointment
#PHQ9 Score from earliest appointment
eaphq9.model <- glmer(MultipleReferrals ~ EA_PHQ9Score + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(eaphq9.model) #sig
#GAD7 Score from earliest appointment
eagad7.model <- glmer(MultipleReferrals ~ EA_GAD7Score + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(eagad7.model) #sig
#WSAS Score from earliest appointment
eawsas.model <- glmer(MultipleReferrals ~ EA_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(eagad7.model) #sig
#Agora Score from earliest appointment
eaagora.model <- glmer(MultipleReferrals ~ EA_AgoraScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(eaagora.model) #sig
#Social Phobia Score from earliest appointment
easocphobia.model <- glmer(MultipleReferrals ~ EA_SocPhobiaScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(easocphobia.model) #non-sig
#Specific Phobia Score from earliest appointment
easpecphobia.model <- glmer(MultipleReferrals ~ EA_SpecPhobiaScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(easpecphobia.model) #sig

#Outcome measures from initial treatment appointment
#PHQ9 Score from initial treatment appointment
itphq9.model <- glmer(MultipleReferrals ~ IT_PHQ9Score + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(itphq9.model) #sig
#GAD7 Score from initial treatment appointment
itgad7.model <- glmer(MultipleReferrals ~ IT_GAD7Score + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(itgad7.model) #sig
#WSAS Score from initial treatment appointment
itwsas.model <- glmer(MultipleReferrals ~ IT_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(itgad7.model) #sig
#Agora Score from initial treatment appointment
itagora.model <- glmer(MultipleReferrals ~ IT_AgoraScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(itagora.model) #sig
#Social Phobia Score from initial treatment appointment
itsocphobia.model <- glmer(MultipleReferrals ~ IT_SocPhobiaScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(itsocphobia.model) #sig
#Specific Phobia Score from initial treatment appointment
itspecphobia.model <- glmer(MultipleReferrals ~ IT_SpecPhobiaScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(itspecphobia.model) #sig

#Outcome measures from final treatment appointment
#PHQ9 Score from final treatment appointment
ftphq9.model <- glmer(MultipleReferrals ~ FT_PHQ9Score + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ftphq9.model) #sig
#GAD7 Score from final treatment appointment
ftgad7.model <- glmer(MultipleReferrals ~ FT_GAD7Score + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ftgad7.model) #sig
#WSAS Score from final treatment appointment
ftwsas.model <- glmer(MultipleReferrals ~ FT_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ftgad7.model) #sig
#Agora Score from final treatment appointment
ftagora.model <- glmer(MultipleReferrals ~ FT_AgoraScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ftagora.model) #sig
#Social Phobia Score from final treatment appointment
ftsocphobia.model <- glmer(MultipleReferrals ~ FT_SocPhobiaScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ftsocphobia.model) #sig
#Specific Phobia Score from final treatment appointment
ftspecphobia.model <- glmer(MultipleReferrals ~ FT_SpecPhobiaScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(ftspecphobia.model) #sig

#WSAS Change score model
wsaschange.model <- glmer(MultipleReferrals ~ WSASChange + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(wsaschange.model) #non-sig
#PHQ9 Change score model
phq9change.model <- glmer(MultipleReferrals ~ PHQ9Change + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(phq9change.model) #non-sig
#GAD7 Change score model
gad7change.model <- glmer(MultipleReferrals ~ GAD7Change + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(gad7change.model) #non-sig

#remove all models from global environment
rm(gen.model, age.model, imd.model, income.model, employment.model, healthanddisability.model, crime.model,
   housingservices.model, livingenvironment.model, idaci.model, idaopi.model, ea.empstatus.model,
   ea.psychmed.model, thertype.model, treatmentappts.model, f2fappts.model, f2f1.1.model, singlemixed.model,
   primaryrole.model, ftstepintensity.model, eaphq9.model, eagad7.model, eawsas.model, eaagora.model,
   easocphobia.model, easpecphobia.model, itphq9.model, itgad7.model, itwsas.model, itagora.model,
   itsocphobia.model, itspecphobia.model, ftphq9.model, ftgad7.model, ftwsas.model, ftagora.model, ftsocphobia.model,
   ftspecphobia.model, wsaschange.model, phq9change.model, gad7change.model)

### Multi level logistic regression models ###
#saturated model
satmodel <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                    HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                    IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                    EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                    IT_Thertype1 + IT_PHQ9Score + IT_GAD7Score + IT_AgoraScore + IT_SocPhobiaScore +
                    IT_SpecPhobiaScore + IT_WSASScore + NumberofTreatmentAppts + NumberofF2FAppts +
                    Numberof1to1F2FAppts + SingleMixedTreatment + PrimaryRoleTraining + FT_StepIntensity +
                    FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore + FT_SocPhobiaScore + FT_SpecPhobiaScore +
                    FT_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(satmodel)

#remove most non-sig: FT_SocPhobiaScore
model1 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_GAD7Score + IT_AgoraScore + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofTreatmentAppts + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + PrimaryRoleTraining + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore + FT_SpecPhobiaScore +
                  FT_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model1)

#remove most non-sig: NumberofTreatmentAppts
model2 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_GAD7Score + IT_AgoraScore + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + PrimaryRoleTraining + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  FT_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model2)

#remove most non-sig: PrimaryRoleTraining
model3 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_GAD7Score + IT_AgoraScore + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  FT_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model3)

#remove most non-sig: IT_GAD7Score
model4 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_AgoraScore + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  FT_WSASScore + (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model4)

#remove most non-sig: FT_WSASScore
model5 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_AgoraScore + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model5)

#remove most non-sig: WSASChange
model6 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_AgoraScore + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model6)

#remove most non-sig: IT_AgoraScore
model7 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + Crime + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model7)

#remove most non-sig: Crime
model8 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_EmpStatus + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model8)

#remove most non-sig: EA_EmpStatus
model9 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                  HealthandDisability + BarrierstoHousingandServices + LivingEnvironment +
                  IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                  EA_AgoraScore + EA_SocPhobiaScore + EA_SpecPhobiaScore + EA_WSASScore + 
                  IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                  IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                  Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                  FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                  (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model9)

#remove most non-sig: EA_SocPhobiaScore
model10 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                   HealthandDisability + BarrierstoHousingandServices + LivingEnvironment +
                   IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                   EA_AgoraScore + EA_SpecPhobiaScore + EA_WSASScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model10)

#remove most non-sig: BarrierstoHousingandServices
model11 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                   HealthandDisability + LivingEnvironment +
                   IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                   EA_AgoraScore + EA_SpecPhobiaScore + EA_WSASScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model11)

#remove most non-sig: EA_SpecPhobiaScore
model12 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                   HealthandDisability + LivingEnvironment +
                   IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                   EA_AgoraScore + EA_WSASScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model12)

#remove most non-sig: EA_WSASScore
model13 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income + Employment +
                   HealthandDisability + LivingEnvironment +
                   IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                   EA_AgoraScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model13)

#remove most non-sig: Employment
model14 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score + EA_GAD7Score +
                   EA_AgoraScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model14)

#remove most non-sig: EA_GAD7Score
model15 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score +
                   EA_AgoraScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_GAD7Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model15)

#remove most non-sig: FT_GAD7Score
model16 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + IDAOPI + EA_PsychMed + EA_PHQ9Score +
                   EA_AgoraScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model16)

#remove most non-sig: IDAOPI
model17 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + EA_PsychMed + EA_PHQ9Score +
                   EA_AgoraScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_SpecPhobiaScore + IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model17)

#remove most non-sig: IT_SpecPhobiaScore
model18 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + EA_PsychMed + EA_PHQ9Score +
                   EA_AgoraScore + 
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model18)

#remove most non-sig: EA_AgoraScore
model19 <- glmer(MultipleReferrals ~ Gender + AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + EA_PsychMed + EA_PHQ9Score +
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model19)

#remove most non-sig: Gender
model20 <- glmer(MultipleReferrals ~ AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + EA_PsychMed + EA_PHQ9Score +
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_AgoraScore +  FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model20)

#remove most non-sig: FT_AgoraScore
model21 <- glmer(MultipleReferrals ~ AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + EA_PHQ9Score +
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score + FT_SpecPhobiaScore +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model21)

#remove most non-sig: FT_SpecPhobiaScore
model22 <- glmer(MultipleReferrals ~ AgeAtReferral + IMD + Income +
                   HealthandDisability + LivingEnvironment +
                   IDACI + EA_PHQ9Score +
                   IT_Thertype1 + IT_PHQ9Score + IT_SocPhobiaScore +
                   IT_WSASScore + NumberofF2FAppts +
                   Numberof1to1F2FAppts + SingleMixedTreatment + FT_StepIntensity +
                   FT_PHQ9Score +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model22)

#remove most non-sig: 
model23 <- glmer(MultipleReferrals ~ Income +
                   HealthandDisability +
                   IT_Thertype1 +
                   IT_WSASScore +
                   SingleMixedTreatment + FT_StepIntensity +
                   (1 | IT_AssignedWorker), family = binomial("logit"), data = finaldf)
summary(model23)

#remove models from global environment
rm(satmodel, model1, model2, model3, model4, model5, model6, model7, model8, model9,
   model10, model11, model12, model13, model14, model15, model16, model17, model18,
   model19, model20, model21, model22, model23)

##Final Model
finalmodel <- glmer(MultipleReferrals ~ IMD +
                      IT_Thertype1 + IT_WSASScore + NumberofF2FAppts +
                      FT_StepIntensity +
                      (1 | IT_AssignedWorker), family = binomial("logit"), na.action = na.omit,data = finaldf)
summary(finalmodel)

#calculate null model for comparison
nullmodel <- glmer(MultipleReferrals ~ (1 | IT_AssignedWorker), family = binomial("logit"), na.action = na.omit, data = finaldf)
summary(nullmodel)

##Check for multicollinearity between variables
vif(finalmodel)
#mean VIF score
1.04 + 2.47 + 1.05 + 1.50 + 2.68
8.74/5 #1.75

#calculate intraclass correlation coefficient for final model
icc(finalmodel)

##Calculate Odds Ratios of model
exp(-0.12564) #IMD 2
exp(-0.03342) #IMD 3
exp(-0.44500) #IMD 4
exp(-0.09401) #IMD 5
exp(0.03997) #IT_Thertype1CBT 
exp(-0.03683) #IT_Thertype1CounsellingforDepression
exp(-0.80465) #IT_Thertype1HIBehaviouralActivation
exp(0.30181) #IT_Thertype1IBT 
exp(-0.19975) #IT_Thertype1OtherHI
exp(0.07778) #IT_Thertype1OtherLI
exp(0.27832) #IT_Thertype1PsychoeducationalPeerSupport
exp(0.09126) #IT_Thertype1SelfHelp
exp(0.02111) #IT_WSASScore
exp(0.32604) #NumberofF2FAppts1
exp(0.36825) #NumberofF2FAppts2
exp(0.23405) #NumberofF2FAppts3 
exp(0.83596) #NumberofF2FAppts4
exp(0.09770) #NumberofF2FAppts5
exp(0.21286) #NumberofF2FAppts6
exp(-0.07226) #NumberofF2FAppts7
exp(0.57091) #NumberofF2FAppts8
exp(0.69794) #FT_StepIntensity2
exp(0.19961) #FT_StepIntensity3
exp(-0.07351) #FT_StepIntensity4

#Confidence Intervals of final model
confint(finalmodel, parm = "beta_", method = "Wald")

##Plot of model
#Create plot labels for y-axis
plotlabels <- c("FT SI 4", " FT SI 3", "FT SI 2",
                "8 F2F Appts", "7 F2F Appts", "6 F2F Appts",
                "5 F2F Appts", "4 F2F Appts", "3 F2F Appts", "2 F2F Appts", "1 F2F Appt",
                "IT WSAS Score", "Ther. Self Help", "Ther. PPS", "Ther. Other LI", "Ther. Other HI",
                "Ther. IPT", "Ther. HI BA", "Ther. CfD.", "Ther. CBT", "IMD 5",
                "IMD 4", "IMD 3", "IMD 2")



p <- plot_model(finalmodel, 
                transform = NULL, #keeps scale as log-odds
                se = TRUE, #include standard error bards
                axis.labels = plotlabels, #add in previously created plot labels
                title = "Log-Odds of Multiple Referrals by Variable", #add title of plot
                vline.color = "grey") #make zero line grey for reference 
p + theme_sjplot(base_size = 12, base_family = "") #add theme of aesthetics
p #print plot

