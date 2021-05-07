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

## Extract participant IDs

FileList <- list.files(path, pattern= "*.xlsx", recursive=T)
ParticipantID <- str_extract(FileList, "\\d{7}")


## CATEGORY FLUENCY
read_xlsx_files <- function(x){
  df_cat <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Semantic",n_max = 6)
  return(df_cat)
}

VF_cat <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

VF_cat["ID"] <- rep(ParticipantID, each=6)

VFcat_tidier <- VF_cat %>%
  select(ID, Total, Measures, Animals, Vehicles, `Fruits and Vegetables`, Fluid, `Writing Utensils`) %>%
  mutate_if(is.numeric, round)

## ACTION FLUENCY
read_xlsx_files <- function(x){
  df_act <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Action", n_max = 6)
  return(df_act)
}

VF_act <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

VF_act["ID"] <- rep(ParticipantID, each=6)

VFact_tidier <- VF_act %>%
  select(ID, Total, Measures, `Things people do`, `Egg`) %>%
  mutate_if(is.numeric, round)

## Letter FLUENCY
read_xlsx_files <- function(x){
  df_let <- read_xlsx(path=paste(path, x, sep = "/"),sheet="Letter", n_max = 6)
  return(df_let)
}

VF_let <- lapply(FileList, read_xlsx_files) %>%
  bind_rows()

VF_let["ID"] <- rep(ParticipantID, each=6)

VFlet_tidier <- VF_let %>%
  select(ID, Total, Measures, M, P, S) %>%
  mutate_if(is.numeric, round) %>%
  pivot_longer(!ID, names_to = "income", values_to = "count")


