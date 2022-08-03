##### SCRIPT FOR TIDYING CR QUESTIONNAIRE AND EXTRACTING COMPOUND SCORE FOR MAIN DATASET #######
requiredPackages = c('dplyr','tidyr','purrr', 'rio', 'tibble', 'hablar', 'DescTools', 'multicon')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


rm(list = ls()) #Clean environment

date = 03072021
# date = format(Sys.Date(), "%d%m%Y") #Extract today's date

#Setting wd and output folder
path.out <- "./Data/Tidy/"
ToInclude <-  readLines("./Data/ToInclude.csv") #To remove excluded participants


####### CLEANING DATASET ###########
#Reading dataset
CRq <- read.csv("./Data/CR/CRquestionnaire_03072021.csv", na.strings=c(""," ","NA"))
CRq <- CRq[-c(1,2),]

# colnames(CRq)

CRq_tidier <- CRq %>%
  #Remove variables that are not of interest
  dplyr::select(StartDate, EndDate, PublicID, Progress, Duration..in.seconds., Finished, DistributionChannel,
         Q19,Q12, Q133, Q13, Q14, Q15, Q16, Q17, Q18, Q37, Q39, Q40, Q41, Q44, Q42, Q43,
         Q48_1:Q157, SC2, BDI.II.Exclusion, IQCODE.Exclusion) %>%
  #Rename variables to meaningful names
  dplyr::rename("Age.Category"=Q19, "Handedness"=Q12, "NativeLang"=Q133, "NeuroDis"=Q13, "PsychDis"=Q14,
         "LangDis"=Q15, "SubsAbuse"=Q16, "Smoking"=Q17, "Sleep"=Q18, "Age.Years"=Q37,
         "Sex"=Q39, "MarStatus"=Q40, "Edu.Years"=Q41, "Edu.Degree"=Q44, "CurrentOccu"=Q42,
         "Income"=Q43, "CR.GenAct"=SC2, "Soc1_before"=Q48_1,"Soc1_during"=Q48_2, "Soc2_before"=Q102_1,"Soc2_during"=Q102_2,"Soc3_before" =Q103_1,
         "Soc3_during"=Q103_2,"Soc4_before"=Q104_1,"Soc4_during"=Q104_2,"Soc5_before"=Q105_1,"Soc5_during"=Q105_2,
         "Soc6_before"=Q106_1,"Soc6_during"=Q106_2,"Soc7_before"=Q107_1,"Soc7_during"=Q107_2,"Soc8_before"=Q108_1,
         "Soc8_during"=Q108_2,"Cog1_before"=Q110_1,"Cog1_during"=Q110_2,"Cog2_before"=Q112_1,"Cog2_during"=Q112_2,
         "Cog3_before"=Q113_1,"Cog3_during"=Q113_2,"Cog4_before"=Q114_1,"Cog4_during"=Q114_2,"Cog5_before"=Q115_1,
         "Cog5_during"=Q115_2,"Cog6_before"=Q116_1,"Cog6_during"=Q116_2,"Cog7_before"=Q117_1,"Cog7_during"=Q117_2,
         "Cog8_before"=Q118_1,"Cog8_during"=Q118_2,"Cog9_before"=Q119_1,"Cog9_during"=Q119_2,"Prod1_before"=Q120_1,
         "Prod1_during"=Q120_2,"Prod2_before"=Q121_1,"Prod2_during"=Q121_2,"Prod3_before"=Q122_1,"Prod3_during"=Q122_2,
         "Prod4_before"=Q123_1,"Prod4_during"=Q123_2,"Prod5_before"=Q124_1,"Prod5_during"=Q124_2,"Prod6_before"=Q125_1,
         "Prod6_during"=Q125_2,"Prod7_before"=Q126_1,"Prod7_during"=Q126_2,"Prod8_before"=Q127_1,"Prod8_during"=Q127_2,
         "Prod9_before"=Q128_1,"Prod9_during"=Q128_2,"Prod10_before"=Q129_1,"Prod10_during"=Q129_2, "OtherAct"=Q76,
         "VigPA_work_before"=Q80,"VigPA_work_days_before"=Q81, "VigPA_work_min_before"=Q85,"ModPA_work_before"=Q83,
         "ModPA_work_days_before"=Q84, "ModPA_work_min_before"=Q82, "TravelPA_before"=Q87, "TravelPA_days_before"=Q88,
         "TravelPA_min_before"=Q87.1, "VigSports_before"=Q89, "VigSports_days_before"=Q91, "VigSports_min_before"=Q95,
         "ModSports_before"=Q96,"ModSports_days_before"=Q97, "ModSports_min_before"=Q98, "Sedentary_min_before"=Q100,
         "VigPA_work_during"=Q139,"VigPA_work_days_during"=Q140, "VigPA_work_min_during"=Q141,"ModPA_work_during"=Q142,
         "ModPA_work_days_during"=Q143, "ModPA_work_min_during"=Q144, "TravelPA_during"=Q146, "TravelPA_days_during"=Q147,
         "TravelPA_min_during"=Q148, "VigSports_during"=Q150, "VigSports_days_during"=Q151, "VigSports_min_during"=Q152,
         "ModSports_during"=Q153,"ModSports_days_during"=Q154, "ModSports_min_during"=Q155, "Sedentary_min_during"=Q157,
         "BDI2.excl"=BDI.II.Exclusion, "IQCODE.mean"=IQCODE.Exclusion) %>%
  #Recode values
  dplyr::mutate(across(Soc1_before:Prod10_during, ~case_when(
    . == "Never" ~ 0,
    . == "A few times a month" ~ 1,
    . == "Once a week" ~ 2,
    . == "Everyday" ~ 3)),.keep="unused") %>%
  dplyr::mutate(across(MarStatus, ~case_when(
    . == "Married or living together with partner" ~ 2,
    . == "I'm single and living with my parents, family, friends/housemates" ~ 2,
    . == "I have a partner or boy-/girlfriend but we do not live together" ~ 1,
    . == "I'm single and living on my own" ~ 0, 
    . == "Other" ~ 2))) %>%
  dplyr::mutate(across(c("VigPA_work_before", "ModPA_work_before", "TravelPA_before", "VigSports_before", "ModSports_before",
              "VigPA_work_during", "ModPA_work_during", "TravelPA_during", "VigSports_during", "ModSports_during"), ~case_when(
              . == "Yes"~ 1, 
              . == "No"~ 0))) %>%
  # #For the PA questionnaire score, replace NA with 0
  dplyr::mutate(across(c("VigPA_work_days_before", "VigPA_work_min_before", "ModPA_work_days_before",
                 "ModPA_work_min_before", "TravelPA_days_before", "TravelPA_min_before","VigSports_days_before",
                 "VigSports_min_before","ModSports_days_before", "ModSports_min_before",
                 "VigPA_work_days_during", "VigPA_work_min_during", "ModPA_work_days_during",
                 "ModPA_work_min_during", "TravelPA_days_during", "TravelPA_min_during","VigSports_days_during",
                 "VigSports_min_during","ModSports_days_during", "ModSports_min_during"), ~case_when(
                 is.na(.) ~ 0,
                 TRUE ~ as.numeric(.)))) %>%
  #Convert character to numeric
  hablar::convert(num(Age.Years, Edu.Years, VigPA_work_days_before, VigPA_work_min_before, ModPA_work_days_before,
              ModPA_work_min_before, TravelPA_days_before, TravelPA_min_before,VigSports_days_before,
              VigSports_min_before,ModSports_days_before, ModSports_min_before, 
              VigPA_work_days_during, VigPA_work_min_during, ModPA_work_days_during,
              ModPA_work_min_during, TravelPA_days_during, TravelPA_min_during,VigSports_days_during,
              VigSports_min_during,ModSports_days_during, ModSports_min_during,
              CR.GenAct, BDI2.excl, IQCODE.mean)) %>%
  #Exclude participants based on progress, BDI, IQCODE, and Exclusion List
  dplyr::filter(BDI2.excl<=19 | is.na(BDI2.excl), IQCODE.mean<3.6 | is.na(IQCODE.mean), 
                (PublicID %in% ToInclude), Handedness=="Right-handed")

#Convert values to numerical values
CRq_tidier$Sedentary_min_before <- as.numeric(CRq_tidier$Sedentary_min_before)
CRq_tidier$Sedentary_min_during <- as.numeric(CRq_tidier$Sedentary_min_during)
  
##Check class of variables
# sapply(CRq_tidier, class)

#Export tidier file
rio::export(CRq_tidier, paste0(path.out, "CRq_tidier_",date, ".csv"))

###### EXTRACTING CR SCORES #######################
CRq_tidiest <- CRq_tidier %>%
  #Group by Age Category to single impute missing values per age group
  dplyr::group_by(Age.Category) %>%
<<<<<<< HEAD
  dplyr::mutate(across(c(-OccupationCode), ~replace_na(., round(mean(., na.rm=T))))) %>%
=======
  dplyr::mutate(across(everything(), ~replace_na(., round(mean(., na.rm=T))))) %>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
  ungroup() %>%
  #To calculate scores per row/participant and not per column
  group_by(PublicID) %>% 
  #Creating CR scores per subcategory (cognitive, social, productive, and physical activity) and 
  #for the period preceding and during the COVID-19 pandemic separately
  dplyr::mutate(CR.Cog.before = sum(c(Cog1_before,Cog2_before,Cog3_before,Cog4_before,Cog5_before,
                                Cog6_before,Cog7_before,Cog8_before,Cog9_before), na.rm = T),
         CR.Cog.during = sum(c(Cog1_during,Cog2_during,Cog3_during,Cog4_during,Cog5_during,
                                Cog6_during,Cog7_during,Cog8_during,Cog9_during), na.rm = T),
         CR.Soc.before = sum(c(Soc1_before,Soc2_before,Soc3_before,Soc4_before,Soc5_before,
                                Soc6_before,Soc7_before,Soc8_before), na.rm = T),
         CR.Soc.during = sum(c(Soc1_during,Soc2_during,Soc3_during,Soc4_during,Soc5_during,
                                Soc6_during,Soc7_during,Soc8_during), na.rm = T),
         CR.Prod.before = sum(c(Prod1_before,Prod2_before,Prod3_before,Prod4_before,Prod5_before,
                                Prod6_before,Prod7_before,Prod8_before,Prod9_before,Prod10_before), na.rm = T),
         CR.Prod.during = sum(c(Prod1_during,Prod2_during,Prod3_during,Prod4_during,Prod5_during,
                                Prod6_during,Prod7_during,Prod8_during,Prod9_during,Prod10_during), na.rm = T),
         PA.compound.before = ((VigPA_work_before*VigPA_work_days_before*VigPA_work_min_before*8)+
                                 (ModPA_work_before*ModPA_work_days_before*ModPA_work_min_before*4)+
                                 (TravelPA_before*TravelPA_days_before*TravelPA_min_before*4)+
                                 (VigSports_before*VigSports_days_before*VigSports_min_before*8)+
                                 (ModSports_before*ModSports_days_before*ModSports_min_before*4)),
         PA.compound.during = ((VigPA_work_during*VigPA_work_days_during*VigPA_work_min_during*8)+
                                 (ModPA_work_during*ModPA_work_days_during*ModPA_work_min_during*4)+
                                 (TravelPA_during*TravelPA_days_during*TravelPA_min_during*4)+
                                 (VigSports_during*VigSports_days_during*VigSports_min_during*8)+
                                 (ModSports_during*ModSports_days_during*ModSports_min_during*4))) %>%
  ungroup() %>%
  #Select variables/columns of interest
<<<<<<< HEAD
  dplyr::select(PublicID, Age.Category, Sex, Edu.Years, OccupationCode, CR.Cog.before, CR.Cog.during, CR.Soc.before, CR.Soc.during,
=======
  dplyr::select(PublicID, Age.Category, Sex, Edu.Years, CR.Cog.before, CR.Cog.during, CR.Soc.before, CR.Soc.during,
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
                CR.Prod.before, CR.Prod.during, PA.compound.before,PA.compound.during,Smoking, Sleep, MarStatus, 
         Edu.Degree, CurrentOccu, Income) %>%
  #Group by Age Category to single impute missing values per age group for physical activity
  group_by(Age.Category) %>%
  dplyr::mutate(across(PA.compound.before:PA.compound.during, ~case_when(
    . == 0 ~ round(mean(.)),
    TRUE ~ .))) %>%
  ungroup() %>%
  #Group by individual to create CR general activity score (sum of cognitive, social, and productive)
  #activities for the period preceding and during the COVID-19 pandemic separately
  dplyr::group_by(PublicID) %>%
  dplyr::mutate(CR.GenAct.before = sum(c(CR.Cog.before, CR.Soc.before, CR.Prod.before), na.rm = T),
<<<<<<< HEAD
  CR.GenAct.during = sum(c(CR.Cog.during, CR.Soc.during, CR.Prod.during), na.rm = T), .after = OccupationCode)
  


=======
  CR.GenAct.during = sum(c(CR.Cog.during, CR.Soc.during, CR.Prod.during), na.rm = T), .after = Edu.Years)
  

>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
zCRq_tidiest <- CRq_tidiest %>%
  group_by(Age.Category) %>%
  dplyr::mutate(
  #Standardise CR scores (general activities, physical activities, and education) within age groups
  zCR.GenAct.before = (CR.GenAct.before - mean(CR.GenAct.before,na.rm=T))/
                      sd(CR.GenAct.before,na.rm=T),
  zCR.GenAct.during = (CR.GenAct.during - mean(CR.GenAct.during,na.rm=T))/
                      sd(CR.GenAct.during,na.rm=T),
    zCR.PA.before = (PA.compound.before - mean(PA.compound.before,na.rm=T))/
                  sd(PA.compound.before,na.rm=T),
    zCR.PA.during = (PA.compound.during - mean(PA.compound.during,na.rm=T))/
                  sd(PA.compound.during,na.rm=T),
    zCR.edu = (Edu.Years - mean(Edu.Years,na.rm=T))/
<<<<<<< HEAD
      sd(Edu.Years,na.rm=T),
 zCR.occu = ifelse(Age.Category != "18 to 30 years old", (OccupationCode - mean(OccupationCode, na.rm=T))/
    sd(OccupationCode,na.rm=T), NA)) %>%
  dplyr::mutate(across(zCR.GenAct.before:zCR.occu, ~Winsorize(., minval = -2.5, maxval = 2.5))) %>%
  rowwise() %>%
  # Create composite score
  dplyr::mutate(CR.composite.before = mean(zCR.GenAct.before, zCR.PA.before, zCR.edu, zCR.occu, na.rm=T),
                CR.composite.during = mean(zCR.GenAct.during,zCR.PA.during, zCR.edu, zCR.occu, na.rm=T), .after=Sex)
  
  #export dataset with CR composite scores
rio::export(zCRq_tidiest, paste0(path.out, "zCRq_tidiest_final.csv"))
=======
      sd(Edu.Years,na.rm=T)) %>%
  dplyr::mutate(across(zCR.GenAct.before:zCR.edu, ~Winsorize(., minval = -2.5, maxval = 2.5))) %>%
  rowwise() %>%
  # Create composite score
  dplyr::mutate(CR.composite.before = mean(zCR.GenAct.before, zCR.PA.before, zCR.edu),
                CR.composite.during = mean(zCR.GenAct.during,zCR.PA.during, zCR.edu), .after=Sex)
  
  #export dataset with CR composite scores
rio::export(zCRq_tidiest, paste0(path.out, "zCRq_tidiest_03072021.csv"))
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

# colnames(zCRq_tidiest)

#Check dataset for missing data
missingdata <- zCRq_tidiest %>%
  group_by(Age.Category) %>%
  dplyr::summarise(n_na_CRbefore = (sum(is.na(CR.composite.before))),
                   n_na_CRduring = (sum(is.na(CR.composite.during))))
