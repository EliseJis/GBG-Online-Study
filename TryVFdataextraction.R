############ VF DATA EXTRACTION AND TIDYING SCRIPT #######################
#  Clean Environment
rm(list=ls())

#  Loading Packages
library(readxl)
library(tidyverse)
library(stringr)
library(purrr)
library(fs)
library(rio)

#  Obtain data + set working and output directory
date = format(Sys.Date(), "%d%m%Y")

path <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/VF/Answers/"
path.out <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Tidy Data/"

stroop <- read.csv("./Tidy Data/Stroop_scores_27042021.csv")
SoP <- read.csv("./Tidy Data/SoP_scores_27042021.csv")
WM <- read.csv("./Tidy Data/WM_scores_27042021.csv")
ToExclude <- read.csv("ToExclude2.csv") #Excluded participants

## Extract participant IDs

FileList <- list.files(path, pattern= "*.xlsx", recursive=T)
ParticipantID <- str_extract(FileList, "\\d{7}")


## CATEGORY FLUENCY
#Function data reads the first 6 rows of the Semantic VF of each file in the path folder.
read_xlsx_files <- function(x){
  df_cat <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Semantic",n_max = 6)
  return(df_cat)
}

#Binds the extracted rows together into one dataframe
VF_cat <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

#Adds participant ID
VF_cat["ID"] <- as.numeric(rep(ParticipantID, each=6))

#Tidy data
VFcat_tidiest <- VF_cat %>%
  select(ID, Total, Measures, Animals, Vehicles, `Fruits and Vegetables`, Fluid, `Writing Utensils`) %>%
  mutate_if(is.numeric, round)  %>% 
  #Remove incomplete participants and excluded participants
  filter(!(ID %in% ToExclude)) %>%
  left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))


#Save data
rio::export(VFcat_tidiest, paste0(path.out, date, "_VFcat_tidiest.csv"))


## ACTION FLUENCY
read_xlsx_files <- function(x){
  df_act <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Action", n_max = 6)
  return(df_act)
}

VF_act <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

VF_act["ID"] <- as.numeric(rep(ParticipantID, each=6))

VFact_tidiest <- VF_act %>%
  select(ID, Total, Measures, `Things people do`, `Egg`) %>%
  mutate_if(is.numeric, round) %>% 
  #Remove incomplete participants and excluded participants
  filter(!(ID %in% ToExclude)) %>%
  left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))

rio::export(VFact_tidiest, paste0(path.out, date, "_VFact_tidiest.csv"))


## Letter FLUENCY
read_xlsx_files <- function(x){
  df_let <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Letter", n_max = 6)
  return(df_let)
}

VF_let <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

VF_let["ID"] <- as.numeric(rep(ParticipantID, each=6))

VFlet_tidiest <- VF_let %>%
  select(ID, Total, Measures, M, P, S) %>%
  mutate_if(is.numeric, round) %>%
  #Remove incomplete participants and excluded participants
  filter(!(ID %in% ToExclude)) %>%
  left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))

rio::export(VFlet_tidiest, paste0(path.out, date, "_VFlet_tidiest.csv"))

