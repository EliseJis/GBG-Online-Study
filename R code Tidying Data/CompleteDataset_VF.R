############ COMPLETE DATASET VERBAL FLUENCY FOR ANALYSIS ####################

requiredPackages = c('dplyr','tidyr','purrr', 'rio', 'tibble', 'hablar', 'readxl', 'ggplot2')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


rm(list = ls()) #Clean environment

date = format(Sys.Date(), "%d%m%Y") #Extract today's data

#Setting working directory and output folder
path.out <- "./Data/Tidy/"
directory <- "./Data"

#List with all included participants with both Public and Private IDs
allIDs <- read.csv("./Data/allIDs.csv") %>%
  dplyr::mutate(ID=as.character(ID))


#Loading all necessary data files
##Verbal Fluency - Categories
VFcat <- read.csv("./Data/Tidy/VFcat_tidiest_final.csv") %>%
  dplyr::mutate(ID = as.character(ID)) %>%
  #Add private ID to dataset
  left_join(allIDs, by="ID")
##Verbal Fluency - Actions
VFact <- read.csv("./Data/Tidy/VFact_tidiest_final.csv") %>%
  dplyr::mutate(ID = as.character(ID)) %>%
  #Add private ID to dataset
  left_join(allIDs, by="ID")
##Verbal Fluency - Letters
VFlet <- read.csv("./Data/Tidy/VFlet_tidiest_final.csv") %>%
  dplyr::mutate(ID = as.character(ID)) %>%
  #Add private ID to dataset
  left_join(allIDs, by="ID")

#Read in dataset with CR scores
CRq <- read.csv("./Data/Tidy/zCRq_tidiest_03072021.csv")

######### VF Categories ####################

## Category Fluency - Selecting variables of interest
VFcat.2 <- VFcat %>%
  filter(Measures=="Ncorrect" | Measures == "AvWordFreq_subtlex") %>%
  #Replace 0 values with NA
  dplyr::mutate(across(Animals:Writing.Utensils, ~case_when(
    . == 0 ~ replace_na(.),
    TRUE ~ as.numeric(.)))) %>%
  #Add CR scores to VF dataset
  dplyr::left_join(CRq, by=c("Participant.Public.ID" = "PublicID", "Age.Category")) %>%
  #Create z-scores to create composite scores
  group_by(Measures) %>%
  dplyr::mutate(zAnimals = (Animals - mean(Animals, na.rm=T))/sd(Animals, na.rm=T),
                zVehicles = (Vehicles - mean(Vehicles, na.rm=T))/sd(Vehicles, na.rm=T),
                zFandV = (Fruits.and.Vegetables - mean(Fruits.and.Vegetables, na.rm=T))/sd(Fruits.and.Vegetables, na.rm=T),
                zFluid = (Fluid - mean(Fluid, na.rm=T))/sd(Fluid, na.rm=T),
                zWriting = (Writing.Utensils - mean(Writing.Utensils, na.rm=T))/sd(Writing.Utensils, na.rm=T),
                .after = Measures) %>%
  ungroup() %>%
  #Exclude outliers (+/- 2.5 SD)
  dplyr::mutate(across(zAnimals:zWriting, ~ifelse((.<=-2.5 | .>=2.5), NA, .))) %>%
  #Create Composite score per participant
  dplyr::group_by(ID,Measures) %>%
  dplyr::mutate(zComp.Cat = (sum(zAnimals,zVehicles,zFandV,zFluid,zWriting, na.rm=T))/5,
                .after = Measures) %>%
  #Move the following columns forward
  dplyr::relocate(c(Age.Category, Sex, Edu.Years), .after = ID) %>%
  #Exclude column with Public IDs for anonimity
  dplyr::select(-Participant.Public.ID)

#Save VF categories complete dataset for final analysis
rio::export(VFcat.2, paste0(path.out, "VFcat_complete_final.csv"))


######### VF LETTERS ####################

## Letter Fluency - Selecting variables of interest
VFlet.2 <- VFlet %>%
  filter(Measures=="Ncorrect" | Measures == "AvWordFreq_subtlex") %>%
  #Replace 0 values with NA
  dplyr::mutate(across(Total:S, ~case_when(
    . == 0 ~ replace_na(.),
    TRUE ~ as.numeric(.)))) %>%
  #Add CR scores to VF dataset
  dplyr::left_join(CRq, by=c("Participant.Public.ID" = "PublicID", "Age.Category")) %>%
  #Create z-scores for composite and outlier exclusion
  group_by(Measures) %>%
  dplyr::mutate(zM = (M - mean(M, na.rm=T))/sd(M, na.rm=T),
                zS = (S - mean(S, na.rm=T))/sd(S, na.rm=T),
                zP = (P - mean(P, na.rm=T))/sd(P, na.rm=T),
                .after = Measures) %>%
  ungroup() %>%
  #Exclude outliers (+/- 2.5 SD)
  dplyr::mutate(across(zM:zP, ~ifelse((.<=-2.5 | .>=2.5), NA, .))) %>%
  #Create Composite score per participant
  dplyr::group_by(ID,Measures) %>%
  dplyr::mutate(zComp.Let = (sum(zM, zS, zP, na.rm=T))/3,
                .after = Measures) %>%
  #Move the following columns forward
  dplyr::relocate(c(Age.Category, Sex, Edu.Years), .after = ID) %>%
  #Exclude column with Public IDs for anonimity
  dplyr::select(-Participant.Public.ID)


#Save VF letters complete dataset for final analysis
rio::export(VFlet.2, paste0(path.out, "VFlet_complete_final.csv"))


######### VF ACTIONS ##################

## Action Fluency - Selecting variables of interest
VFact.2 <- VFact %>%
  filter(Measures=="Ncorrect" | Measures == "AvWordFreq_subtlex") %>%
  #Replace 0 values with NA
  dplyr::mutate(across(Total:Things.people.do, ~case_when(
    . == 0 ~ replace_na(.),
    TRUE ~ as.numeric(.)))) %>%
  #Add CR scores to VF dataset
  dplyr::left_join(CRq, by=c("Participant.Public.ID" = "PublicID", "Age.Category")) %>%
  #Create z-scores for composite
  group_by(Measures) %>%
  dplyr::mutate(zPeople = (Things.people.do - mean(Things.people.do, na.rm=T))/sd(Things.people.do, na.rm=T),
                zEggs = (Egg - mean(Egg, na.rm=T))/sd(Egg, na.rm=T), 
                .after = Measures) %>%
  ungroup() %>%
  #Exclude outliers (+/- 2.5 SD)
  dplyr::mutate(across(zPeople:zEggs, ~ifelse((.<=-2.5 | .>=2.5), NA, .))) %>%
  dplyr::group_by(ID,Measures) %>%
  #Create Composite score per participant
  dplyr::mutate(zComp.Act = (sum(zPeople,zEggs, na.rm=T))/2,
                .after = Measures) %>%
  #Move the following columns forward
  dplyr::relocate(c(Age.Category, Sex, Edu.Years), .after = ID)

#Save VF actions complete dataset for final analysis
rio::export(VFact.2, paste0(path.out, "VFact_complete_final.csv"))



