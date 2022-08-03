###Load packages
requiredPackages = c('dplyr','tidyr','purrr', 'rio', 'tibble', 'hablar', 'ggplot2',
                     'psych', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}



#Clean Environment
rm(list=ls())

#Obtain date
date = "27072021"

#setting working directory
path.out <- "./Data/Tidy/"

#Read in data
PNobjects <- as_tibble(read.csv("./Data/Picture Naming/PNobj_merged_22072021.csv")) #Object Naming
PNactions <- as_tibble(read.csv("./Data/Picture Naming/PNact_merged_22072021.csv")) #Action Naming
<<<<<<< HEAD:R code Tidying Data/CleanPictureNaming.R
GenCognProc <- as_tibble(read.csv("./Data/Tidy/GenCogComposite_final.csv")) #Contains composite score of general cognitive processing
=======
stroop <- read.csv("./Data/Tidy/Stroop_scores_final.csv") #Stroop scores/Inhibitory Control
SoP <- read.csv("./Data/Tidy/SoP_scores_final.csv") #SoP scores/Cognitive Processing Speed
WM <- read.csv("./Data/Tidy/WM_scores_final.csv") #WM scores/Working Memory
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca:CleanObjectNaming.R
ToInclude <- readLines("./Data/ToInclude.csv") #Participants that are included; to filter out excluded participants


### OBJECT NAMING ####
PNobjects_tidier <- PNobjects %>%
  #Filter out double rows and first trial
  dplyr::filter((display=="PracticeTrials" | display == "RealTrials") & 
           (Zone.Name=="content" & picname!="lionExclude")) %>%
  #Filter columns of interest
  dplyr::select(UTC.Date, Experiment.Version, Participant.Public.ID, Participant.Private.ID, Participant.Status,
         Task.Name,randomiser.3pls, order.2ubj, order.kx72, randomiser.2p1e, randomiser.4bdx,
         Trial.Number, Reaction.Time, picname, type, picnum, frequency, freq_HAL_ELP, AoA, AoA_ELP, picfric, 
         NameAgreement, PNameAgreement, Nsyll, Nchar, Nphon, VisComplex, ShareName, WordComplexity,
         ConceptualComplexity, OrthNeighb, OrthNeighb_ELP, PhonNeighb, PhonNeighb_ELP, SemNeigD,
         Participant.Device, Participant.OS, Participant.Browser, Participant.Monitor.Size, Participant.Viewport.Size) %>%
  #Merge columns
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  unite(col="List", randomiser.2p1e:randomiser.4bdx, sep="", remove=T) %>%
  #Remove incomplete participants and excluded participants
  dplyr::filter(Participant.Status=="complete",
         (Participant.Public.ID %in% ToInclude)) %>%
  #Change Column names
  dplyr::rename("OrderLang"=randomiser.3pls, "durStim"=Reaction.Time, "ID"=Participant.Private.ID) %>%
  dplyr::select(-Participant.Status)

  #Check whether the necessary participants are exluded. Should be only FALSE (check before filtering out Public.ID)
  # unique(PNobjects_tidier$Participant.Public.ID %in% ToExclude)

#Save tidier file
rio::export(PNobjects_tidier, paste0(path.out, date, "_PNobjects_tidier.csv"))

  
#ADD CONTROL MEASURES
PNobjects_tidiest <- PNobjects_tidier %>%
<<<<<<< HEAD:R code Tidying Data/CleanPictureNaming.R
  left_join(GenCognProc, by=c("ID" = "Participant.Private.ID"))
=======
  left_join(stroop, by=c("ID" = "Participant.Private.ID"))%>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca:CleanObjectNaming.R

#Save fully cleaned dataset with control tasks added
rio::export(PNobjects_tidiest, paste0(path.out, "PNobjects_tidiest_final.csv"))

### ACTION NAMING ####
PNactions_tidier <- PNactions %>%
  #Filter out double rows and first stimulus
  dplyr::filter((display=="PracticeTrials" | display == "RealTrials") & 
           (Zone.Name=="content" & picname!="barkExclude")) %>%
  #Filter columns of interest
  dplyr::select(UTC.Date, Experiment.Version, Participant.Public.ID, Participant.Private.ID, Participant.Status,
         Task.Name,randomiser.3pls, order.2ubj, order.kx72, randomiser.csgp, randomiser.z3o5,
         Trial.Number, Reaction.Time, picname, type, picnum, frequency, freq_HAL_ELP, AoA, AoA_ELP, picfric, 
         NameAgreement, PNameAgreement, Nsyll, Nchar, Nphon, VisComplex, ShareName, WordComplexity,
         ConceptualComplexity, OrthNeighb, OrthNeighb_ELP, PhonNeighb, PhonNeighb_ELP, SemNeigD,
         Participant.Device, Participant.OS, Participant.Browser, Participant.Monitor.Size, Participant.Viewport.Size) %>%
  #Merge columns
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  unite(col="List", randomiser.csgp:randomiser.z3o5, sep="", remove=T) %>%
  #Remove incomplete participants and excluded participants
  dplyr::filter(Participant.Status=="complete",
         (Participant.Public.ID %in% ToInclude)) %>%
  #Change Column names
  dplyr::rename("OrderLang"=randomiser.3pls, "durStim"=Reaction.Time, "ID"=Participant.Private.ID) %>%
  dplyr::select(-Participant.Status)

#Check whether the necessary participants are exluded. Should be only FALSE (check before filtering out Public.ID)
# unique(PNobjects_tidier$Participant.Public.ID %in% ToExclude)

#Save tidier file
rio::export(PNactions_tidier, paste0(path.out, date, "_PNactions_tidier.csv"))

#ADD CONTROL MEASURES
PNactions_tidiest <- PNactions_tidier %>%
  left_join(GenCognProc, by=c("ID" = "Participant.Private.ID"))

#Save fully cleaned dataset with control tasks added
rio::export(PNactions_tidiest, paste0(path.out, "PNactions_tidiest_final.csv"))

