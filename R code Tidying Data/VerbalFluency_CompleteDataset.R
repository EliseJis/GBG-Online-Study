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
<<<<<<< HEAD
CRq <- read.csv("./Data/Tidy/zCRq_tidiest_final.csv")
=======
CRq <- read.csv("./Data/Tidy/zCRq_tidiest_03072021.csv")
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

######### VF Categories ####################

## Category Fluency - Selecting variables of interest
VFcat.2 <- VFcat %>%
<<<<<<< HEAD
  filter(Measures=="Ncorrect" | Measures=="AvWordFreq_subtlex") %>%
=======
  filter(Measures=="Ncorrect" | Measures == "AvWordFreq_subtlex") %>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
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
<<<<<<< HEAD
  group_by(Measures) %>%
  #Exclude outliers (+/- 2.5 SD)
  dplyr::mutate(across(zAnimals:zWriting, ~ifelse((.<=-2.5 | .>=2.5), NA, .))) 

##Before outlier removal:
  ## Ncorrect
  # zAnimals: [-3.38 2.5]; NA's 2
  # zVehicles: [-2.10 3.30]; NA's 0
  # zFluid: [-2.10 2.11]; NA's 0
  # zFandV: [-3.34 3.42]; NA's 0
  # zWriting: [-1.95 3.77]; NA's 
  ## Frequency
  # zAnimals: [-8.34 1.43]; NA's 2
  # zVehicles: [-2.69 2.71]; NA's 0
  # zFluid: [-4.10 2.68]; NA's 0
  # zFandV: [-3.86 2.86]; NA's 0
  # zWriting: [-2.32 3.16]; NA's 0
  
##After Outlier removal:
  ## Ncorrect
  # zAnimals [-2.12 2.08]; NA's 4 (4.4% missing/removal)
  # zVehicles: [-2.10 2.47]; NA's 2 (2.2% removed)
  # zFluid: [-2.10 2.11]; NA's 0 (0% removed)
  # zFandV: [ -1.92 1.82]; NA's 3 (3.3% removed)
  # zWriting: [-1.95 2.14]; NA's 3 (3.3% removed)
  ## Frequenc
  # zAnimals: [-1.07 1.43]; NA's 3 (3.3% removed)
  # zVehicles: [-2.09 2.02]; NA's 2 (2.2% removed)
  # zFluid: [-2.18 2.16]; NA's 2 (2.2% removed)
  # zFandV: [-2.18 1.83]; NA's 2 (2.2% removed)
  # zWriting: [-2.32 2.26]; NA's 1 (1.1% removed)
  
  #Create Composite score per participant
VFcat.3 <- VFcat.2 %>%
=======
  ungroup() %>%
  #Exclude outliers (+/- 2.5 SD)
  dplyr::mutate(across(zAnimals:zWriting, ~ifelse((.<=-2.5 | .>=2.5), NA, .))) %>%
  #Create Composite score per participant
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
  dplyr::group_by(ID,Measures) %>%
  dplyr::mutate(zComp.Cat = (sum(zAnimals,zVehicles,zFandV,zFluid,zWriting, na.rm=T))/5,
                .after = Measures) %>%
  #Move the following columns forward
  dplyr::relocate(c(Age.Category, Sex, Edu.Years), .after = ID) %>%
  #Exclude column with Public IDs for anonimity
  dplyr::select(-Participant.Public.ID)

<<<<<<< HEAD
VFcat.4 <- VFcat.3 %>%
  filter(Measures=="Ncorrect") %>%
  group_by(ID) %>%
  summarise(nAnimals = mean(zAnimals, na.rm=T), #88-86 2.2%
            nVehicles = mean(zVehicles, na.rm=T), #90-88 2.2%
            nFandV = mean(zFandV, na.rm=T), #90-87 3.3%
            nFluid = mean(zFluid, na.rm=T), #90-90 0%
            nWriting = mean(zWriting, na.rm=T)) %>% #90-87 3.3%
  select(-Measures)
sapply(VFcat.4, range, na.rm=T) #There are values exceeding -2.5 or 2.5

#Save VF categories complete dataset for final analysis
rio::export(VFcat.3, paste0(path.out, "VFcat_complete_final.csv"))
=======
#Save VF categories complete dataset for final analysis
rio::export(VFcat.2, paste0(path.out, "VFcat_complete_final.csv"))
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca


######### VF LETTERS ####################

## Letter Fluency - Selecting variables of interest
VFlet.2 <- VFlet %>%
<<<<<<< HEAD
  filter(Measures == "Ncorrect" | Measures=="AvWordFreq_subtlex") %>%
=======
  filter(Measures=="Ncorrect" | Measures == "AvWordFreq_subtlex") %>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
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
<<<<<<< HEAD
  group_by(Measures) %>%
=======
  ungroup() %>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
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


<<<<<<< HEAD
  ##Before outlier removal:
  ## Ncorrect
  # zM: [-1.87 2.95]; NA's 0
  # zS: [-2.56 3.15]; NA's 0
  # zP: [-2.74 2.41]; NA's 0
  ## Frequency
  # zM: [-2.56 2.45]; NA's 0
  # zS: [-3.18 3.12]; NA's 0
  # zP: [-2.73 2.24]; NA's 0
  
  ##After Outlier removal:
  ## Ncorrect
  # zM [-1.87 1.92]; NA's 3 (3.3% removal)
  # ZS: [-1.59 2.03]; NA's 4 (4.4% removed)
  # ZP: [-2.41 2.41]; NA's 1 (1.1% removed)
  ## Frequency
  # zM: [-2.38 2.45]; NA's 1 (1.1% removed)
  # zS: [-2.17 1.97]; NA's 4 (4.4% removed)
  # zP: [-2.47 2.24]; NA's 2 (2.2% removed)

=======
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
#Save VF letters complete dataset for final analysis
rio::export(VFlet.2, paste0(path.out, "VFlet_complete_final.csv"))


######### VF ACTIONS ##################

## Action Fluency - Selecting variables of interest
VFact.2 <- VFact %>%
<<<<<<< HEAD
  filter(Measures == "Ncorrect" | Measures=="AvWordFreq_subtlex") %>%
=======
  filter(Measures=="Ncorrect" | Measures == "AvWordFreq_subtlex") %>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
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
<<<<<<< HEAD
  group_by(Measures) %>%
=======
  ungroup() %>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
  #Exclude outliers (+/- 2.5 SD)
  dplyr::mutate(across(zPeople:zEggs, ~ifelse((.<=-2.5 | .>=2.5), NA, .))) %>%
  dplyr::group_by(ID,Measures) %>%
  #Create Composite score per participant
  dplyr::mutate(zComp.Act = (sum(zPeople,zEggs, na.rm=T))/2,
                .after = Measures) %>%
  #Move the following columns forward
  dplyr::relocate(c(Age.Category, Sex, Edu.Years), .after = ID)

<<<<<<< HEAD
##Before outlier removal:
  ## Ncorrect
# zPeople: [-2.68 3.39]; NA's 0
# zEggs: [-2.86 2.45]; NA's 0
  ## Frequency
# zPeople: [-2.51 3.68]; NA's 0
# zEggs: [-2.66 2.49]; NA's 0
  
##After Outlier removal:
  ## Ncorrect
# zPeople [-1.58 2.47]; NA's 2 (2.2% removal)
# ZEggs: [-2.02 2.45]; NA's 1 (1.1% removed)
  ## Frequency
# zPeople: [-1.77 2.15]; Na's 2 (2.2% removed)
# zEggs: [-2.33 2.49]; NA's 2 (2.2% removed)
  
=======
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
#Save VF actions complete dataset for final analysis
rio::export(VFact.2, paste0(path.out, "VFact_complete_final.csv"))



