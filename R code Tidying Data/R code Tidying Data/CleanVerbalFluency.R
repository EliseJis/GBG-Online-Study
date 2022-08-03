############ VF DATA EXTRACTION AND TIDYING SCRIPT #######################
requiredPackages = c('dplyr','tidyr','purrr', 'rio', 'tibble', 'hablar', 'fs',
                     'stringr', 'tidyverse', 'readxl')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

#  Clean Environment
rm(list=ls())

#  Obtain date + set working and output directory
date = format(Sys.Date(), "%d%m%Y")

path <- "./Data/Verbal Fluency/"
path.out <- "./Data/Tidy/"

<<<<<<< HEAD
#Read in tidied data files with scores for the composite control measures
GenCognProc <- as_tibble(read.csv("./Data/Tidy/GenCogComposite_final.csv")) #Contains composite score of general cognitive processing
=======
#Read in tidied data files with scores for the three control measures
stroop <- read.csv("./Data/Tidy/Stroop_scores_final.csv") #Stroop task/Inhibitory Control
SoP <- read.csv("./Data/Tidy/SoP_scores_final.csv") #SoP/Cognitive Processing Speed
WM <- read.csv("./Data/Tidy/WM_scores_final.csv") #WM/WorkingMemory
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
ToInclude <- readLines("./Data/ToInclude_private.csv") #Participants that are included; to filter out excluded ppts

#The answers for the verbal fluency tasks were recorded in a .xlsx file for each participant separately.
#The category fluency, action fluency, and letter fluency had each their own sheet within the spreadsheet.
#The file names contain the participant ID.
## Extract participant IDs from the filenames
FileList <- list.files(path, pattern= "*.xlsx", recursive=T)
ParticipantID <- str_extract(FileList, "\\d{7}")


## CATEGORY FLUENCY
#Function data reads the first 6 rows of the Semantic VF of each file in the path folder. The first 6 rows
#contained the important information (e.g., Number of correctly produced words, average frequency, number
#of errors, etc.)
read_xlsx_files <- function(x){
  df_cat <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Semantic",n_max = 6, col_types = "text")
  return(df_cat)
}

#Binds the extracted rows of all participants together into one data frame
VF_cat <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

#Adds participant ID to spreadsheet
VF_cat["ID"] <- as.numeric(rep(ParticipantID, each=6))

#Tidy data
VFcat_tidiest <- VF_cat %>%
  #Select columns/variables of interest
  dplyr::select(ID, Measures, Total, Animals, Vehicles, `Fruits and Vegetables`, Fluid, `Writing Utensils`) %>%
  #Round numeric values to 3 decimals
  dplyr::mutate(across(c(Total, Animals, Vehicles, "Fruits and Vegetables", Fluid, "Writing Utensils"), ~case_when(
    Measures == "AvWordFreq_subtlex" ~ round(as.numeric(.), 3),
    is.character(.) ~ round(as.numeric(.))))) %>%
  #Remove incomplete and excluded participants
  dplyr::filter(ID %in% ToInclude) %>%
  #Add the scores of each of the control tasks to the dataset.
<<<<<<< HEAD
  dplyr::left_join(GenCognProc, by=c("ID" = "Participant.Private.ID"))

=======
  dplyr::left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  dplyr::left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  dplyr::left_join(WM, by=c("ID"="Participant.Private.ID")) #%>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

#Save tidied dataset
rio::export(VFcat_tidiest, paste0(path.out, "VFcat_tidiest_final.csv"))


## ACTION FLUENCY
#Function data reads the first 6 rows of the Action VF of each file in the path folder. The first 6 rows
#contained the important information (e.g., Number of correctly produced words, average frequency, number
#of errors, etc.)
read_xlsx_files <- function(x){
  df_act <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Action", n_max = 6, col_types = "text")
  return(df_act)
}

#Binds the extracted rows of all participants together into one data frame
VF_act <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

#Adds participant ID to spreadsheet
VF_act["ID"] <- as.numeric(rep(ParticipantID, each=6))

#Tidy dataset
VFact_tidiest <- VF_act %>%
  #Select columns/variables of interest
  select(ID, Measures, Total, `Things people do`, `Egg`) %>%
  #Round numeric values to 3 decimals
  dplyr::mutate(across(c(Total, "Things people do", Egg), ~case_when(
    Measures == "AvWordFreq_subtlex" ~ round(as.numeric(.), 3),
    is.character(.) ~ round(as.numeric(.))))) %>%
  #Remove incomplete participants and excluded participants
  filter((ID %in% ToInclude)) %>%
  #Add the scores of each of the control tasks to the dataset.
<<<<<<< HEAD
  left_join(GenCognProc, by=c("ID" = "Participant.Private.ID"))

=======
  left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

#Save tidy dataset for further analysis
rio::export(VFact_tidiest, paste0(path.out, "VFact_tidiest_final.csv"))


## Letter FLUENCY
#Function data reads the first 6 rows of the Letter VF of each file in the path folder. The first 6 rows
#contained the important information (e.g., Number of correctly produced words, average frequency, number
#of errors, etc.)
read_xlsx_files <- function(x){
  df_let <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Letter", n_max = 6, col_types = "text")
  return(df_let)
}

#Binds the extracted rows of all participants together into one data frame
VF_let <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

#Adds participant ID to spreadsheet
VF_let["ID"] <- as.numeric(rep(ParticipantID, each=6))

#Tidy dataset 
VFlet_tidiest <- VF_let %>%
  #Select columns/variables of interest
  select(ID, Measures, Total, M, P, S) %>%
  #Round numeric values to 3 decimals
  dplyr::mutate(across(c(Total, M, P, S), ~case_when(
    Measures == "AvWordFreq_subtlex" ~ round(as.numeric(.), 3),
    is.character(.) ~ round(as.numeric(.))))) %>%
  #Remove incomplete participants and excluded participants
  filter((ID %in% ToInclude)) %>%
  #Add the scores of each of the control tasks to the dataset.
<<<<<<< HEAD
  left_join(GenCognProc, by=c("ID" = "Participant.Private.ID"))

=======
  left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

#Save tidy dataset for further analysis
rio::export(VFlet_tidiest, paste0(path.out, "VFlet_tidiest_final.csv"))

