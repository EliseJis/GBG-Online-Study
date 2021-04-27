###Load packages
library(tidyverse)
library(dplyr)
library(purrr)
#library(rJava)
library(rio)
library(ggplot2)
library(psych)

#setting working directory
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data")
path.out <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Tidy Data/"

#Read in data
PNobjects <- as_tibble(read.csv("./Merged/PNobj_merged_19042021.csv"))
ToExclude <- read.csv("ToExclude.csv")

PNobjects_tidier <- PNobjects %>%
  #Filter out double rows and first stimulus
  filter((display=="PracticeTrials" | display == "RealTrials") & 
           (Zone.Name=="content" & picname!="lionExclude")) %>%
  #Filter columns of interest
  select(UTC.Date, Experiment.Version, Participant.Public.ID, Participant.Private.ID, Participant.Status,
         Task.Name,randomiser.3pls, order.2ubj, order.kx72, randomiser.2p1e, randomiser.4bdx,
         Trial.Number, Reaction.Time, picname, type, picnum, frequency, freq_HAL_ELP, AoA, AoA_ELP, picfric, 
         NameAgreement, PNameAgreement, Nsyll, Nchar, Nphon, VisComplex, ShareName, WordComplexity,
         ConceptualComplexity, OrthNeighb, OrthNeighb_ELP, PhonNeighb, PhonNeighb_ELP, SemNeigD,
         Participant.Device, Participant.OS, Participant.Browser, Participant.Monitor.Size, Participant.Viewport.Size) %>%
  #Merge columns
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  unite(col="List", randomiser.2p1e:randomiser.4bdx, sep="", remove=T) %>%
  #Remove uncomplete participants and excluded participants
  filter(Participant.Status=="complete",
         !(Participant.Public.ID %in% ToExclude)) %>%
  #Change Columnnames
  rename("OrderLang"=randomiser.3pls, "durStim"=Reaction.Time, "ID"=Participant.Private.ID) %>%
  select(-Participant.Public.ID, -Participant.Status)

  #Check whether the necessary participants are exluded. Should be only FALSE (check before filtering out Public.ID)
  # unique(PNobjects_tidier$Participant.Public.ID %in% ToExclude)
  



rio::export(PNobjects_tidy, paste0(path.out, "PNobjects_tidy.csv"))

