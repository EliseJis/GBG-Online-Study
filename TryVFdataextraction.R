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

setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data")
path.out <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Tidy Data/"

## Extract participant IDs
FileList <- list.files(path = "./VF/Answers/")
ParticipantID <- str_extract(FileList, "\\d{7}")


# Category Fluency
dir_ls(path = "./VF/Answers/") %>%
  map(read_xlsx(path))

