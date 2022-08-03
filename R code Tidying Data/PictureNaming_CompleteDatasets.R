################ PICTURE NAMING ###########################
requiredPackages = c('dplyr','tidyr','purrr', 'rio', 'tibble', 'hablar', 'readxl', 'ggplot2')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


rm(list = ls()) #Clean environment

<<<<<<< HEAD
date = "final"
=======
date = "27072021"
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
# date = format(Sys.Date(), "%d%m%Y") #Extract today's data

#Setting wd and output folder
path.out <- "./Data/Tidy/"
directory <- "./Data"

#Loading all necessary files
PNobjects <- read.csv("./Data/Tidy/PNobjects_tidiest_final.csv") #Clean Object Naming data file
PNactions <- read.csv("./Data/Tidy/PNactions_tidiest_final.csv")  #Clean Action Naming data file
<<<<<<< HEAD
CRq <- read.csv("./Data/Tidy/zCRq_tidiest_final.csv")  #Clean CR data file
=======
CRq <- read.csv("./Data/Tidy/zCRq_tidiest_03072021.csv")  #Clean CR data file
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

#Stimuli names to be excluded
drop.stimuli <- c("vacuum","somersault","wait","cut", "talk", "football", "skunk")

#Loading RTs and ACC for PN ACTIONS
##Each single participant has an .xlsx datafile with the RTs and Acc per trial. All these files are in one
##folder and need to be merged to one big dataset, which is what we do here. These files are different from
##the Gorilla Experimental Builder data files obtained because accuracy and reaction times were obtained 
##later using CheckVocal.
FileList_act <- list.files(path=paste0(directory,"/Picture naming/Transcribed/Actions/"), pattern= "*.xlsx", recursive=T)
read_actions <- function(x){
  PNact_df <- read_xlsx(path=paste(paste0(directory, "/Picture naming/Transcribed/Actions/"), x, sep = "/"), range=cell_cols("A:H"), col_types = "text")
  return(PNact_df)
}

#Merge all individual participant files in the directory together
PNact_ans <- lapply(FileList_act, read_actions) %>%
  bind_rows() %>%
  #Convert all values to numerical values
  convert(num(Trial, Participant, BeepOnset, VoiceOnset, RT, Acc)) %>%
  # select(-'Agree/disagree with acc given') %>%
  #Delete trials that had low name agreement (cut was in practice and real)
  dplyr::filter(!(Name %in% drop.stimuli))

#Change variable name
PNact_ans$picname <- PNact_ans$Name

###Loading RTs and ACC for PN OBJECTS
FileList_obj <- list.files(path=paste0(directory,"/Picture naming/Transcribed/Objects/"), pattern= "*.xlsx", recursive=T)
read_objects <- function(x){
  PNobj_df <- read_xlsx(path=paste(paste0(directory, "/Picture naming/Transcribed/Objects/"), x, sep = "/"), range=cell_cols("A:H"), col_types = "text")
  return(PNobj_df)
}

#Merge all individual participant files in the directory together
PNobj_ans <- lapply(FileList_obj, read_objects) %>%
  bind_rows() %>%
  convert(num(Trial, Participant, BeepOnset, VoiceOnset, RT, Acc)) %>%
  # select(-'Agree/disagree with acc given') %>%
  #Delete trials that had low name agreement (cut was in practice and real)
  dplyr::filter(!(Name %in% drop.stimuli))

#Change variable name
PNobj_ans$picname <- PNobj_ans$Name

######### COMPLETE DATASETS ########

PNactions_complete <- PNactions %>%
  #Add merged individual files to the PNactions cleaned Gorilla dataset
  dplyr::right_join(PNact_ans, by=c("picname","ID"="Participant")) %>%
  #Add CR scores to the dataset
  dplyr::left_join(CRq, by=c("Participant.Public.ID" = "PublicID", "Age.Category")) %>%
  #Exclude the column with the Public Participant IDs for anonimity
  select(-Participant.Public.ID) %>%
  #Trials that were named correctly but with an alternative name were coded as 2. Here, we convert
  #these values to 1 as to indicate a correct trial.
  dplyr::mutate(Acc =ifelse(Acc %in% c(1,2), 1, 0)) %>%
  #Type = 0 means practice trials which are excluded here.
  dplyr::filter(type!=0) %>%
  #Move columns forward
  relocate(Age.Category:Edu.Years, Smoking:MarStatus, 
           CR.composite.before:CR.composite.during, .after=ID) %>%
  relocate(Name:Acc, .after = Trial.Number)

#Check if all participants are included (should be 90)  
length(unique(PNactions_complete$ID))


PNobjects_complete <- PNobjects %>%
  #Add merged individual files to the PNobjects cleaned Gorilla dataset
  right_join(PNobj_ans, by=c("picname","ID"="Participant")) %>%
  #Add CR scores to the dataset
  left_join(CRq, by=c("Participant.Public.ID" = "PublicID", "Age.Category")) %>%
  #Exclude the column with the Public Participant IDs for anonimity
  dplyr::select(-Participant.Public.ID) %>%
  #Trials that were named correctly but with an alternative name were coded as 2. Here, we convert
  #these values to 1 as to indicate a correct trial.
  dplyr::mutate(Acc =ifelse(Acc %in% c(1,2), 1, 0)) %>%
  #Type = 0 means practice trials which are excluded here.
  dplyr::filter(type!=0) %>%
  #Move columns forward
  relocate(Age.Category:Edu.Years, Smoking:MarStatus, 
           CR.composite.before:CR.composite.during, .after=ID) %>%
  relocate(Name:Acc, .after = Trial.Number)

#Check if all participants are included (should be 90)  
length(unique(PNobjects_complete$ID))

#Save complete datasets for final analysis  
rio::export(PNactions_complete, paste0(path.out, "PNactions_complete_final.csv"))
rio::export(PNobjects_complete, paste0(path.out, "PNobjects_complete_final.csv"))

