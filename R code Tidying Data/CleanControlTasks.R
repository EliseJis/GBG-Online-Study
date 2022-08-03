##### SCRIPT FOR TIDYING CONTROL TASKS AND EXTRACTING INFO FOR MAIN DATASET #######
###Load packages
requiredPackages = c('dplyr','tidyr','purrr', 'rio', 'tibble', 'hablar', 'ggplot2',
                     'psych', 'tidyverse', 'readr', 'plyr', 'multicon', 'DescTools')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


rm(list = ls()) #Clean environment

date = "final"
path.out <- "./Data/Tidy/"

### CLEANING THE DATASETS
#List with participants that will be included
ToInclude <- readLines("./Data/ToInclude.csv") #List with public participant IDs
ToInclude2 <- read.csv("./Data/ToInclude_private.csv") #only private IDs

#To obtain age categories
CRq <- read.csv("./Data/Tidy/zCRq_tidiest_03072021.csv") %>%
  dplyr::select(PublicID, Age.Category)

###### Stroop Task ##################
Stroop_merged <- read.csv("./Data/Stroop/Stroop_merged_22072021.csv") %>%
  #Add ages by joining CRq and Stroop by Public ID
  dplyr::left_join(CRq, by=c("Participant.Public.ID" = "PublicID"))
# view(Stroop_merged)

#Filter out columns and rows that won't be used (incl. excluded participants because of technical problems)
Stroop_tidier <- Stroop_merged %>%
  #Filter out irrelevant columns/variables
  dplyr::select(Age.Category, UTC.Timestamp, UTC.Date, Experiment.Version, Task.Name, Participant.Public.ID,Participant.Private.ID,
         Participant.Status,randomiser.3pls,order.2ubj, order.kx72, Trial.Number, Screen.Name,
         Reaction.Time, Response, CorrectAnswer, Correct, Timed.Out, display, Go, ISI,
         Participant.Device.Type, Participant.OS, Participant.Browser) %>%
  #Filter out irrelevant rows
  dplyr::filter(Screen.Name=="PracticeArrow" | Screen.Name=="RealArrow",
         (Participant.Public.ID %in% ToInclude)) %>%
  #Gorilla Experiment Builder uses branches. Hence, there are two columns indicating the order
  #of control tasks. Here, we merge them together.
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  dplyr::rename("compatibility"=Go) %>%
  dplyr::mutate(across(compatibility, ~case_when(
    . == "CongruentLeft.png" ~ "Stroop.Compatible",
    . == "CongruentRight.png" ~ "Stroop.Compatible",
    . == "IncongruentLeft.png" ~ "Stroop.Incompatible",
    . == "IncongruentRight.png" ~ "Stroop.Incompatible"))) %>%
  #Remove column with Public ID for anonimity
 dplyr::select(-Participant.Public.ID)

#Check whether the necessary participants are exluded. Should be only FALSE
# unique(Stroop_tidier$Participant.Public.ID %in% ToExclude)

#Save tidied Stroop dataset
rio::export(Stroop_tidier, paste0(path.out, "Stroop_tidier_",date, ".csv"))

<<<<<<< HEAD

=======
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
### SCORES STROOP - to obtain stroop scores for further analysis
Stroop_scores <- Stroop_tidier %>%
  #Only include correct trials and exclude practice trials
  dplyr::filter(Correct==1 | Screen.Name=="RealArrow") %>%
  #Group by Age Category and condition (compatible or incompatible) to single impute
  #missing data using the groups mean.
  dplyr::group_by(Age.Category, compatibility) %>%
  dplyr::mutate(across(Reaction.Time, ~case_when(
    is.na(.) ~ round(mean(.)),
    TRUE ~ .))) %>%
  ungroup() %>%
  #Average over trials per participant
  dplyr::group_by(Participant.Private.ID, compatibility) %>%
  dplyr::summarise(RT =round(mean(Reaction.Time),3)) %>%
  #Create Stimulus Response Compatibility score
  dplyr::mutate(Stroop.SRC = round(RT[compatibility=="Stroop.Incompatible"]-RT[compatibility=="Stroop.Compatible"]),3) %>%
<<<<<<< HEAD
  ungroup()%>%
=======
  ungroup() %>%
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
  #Convert to wide format
  spread(compatibility, RT) %>%
  #Remove third column
  dplyr::select(-3) %>%
  #Create z score of the Stimulus Rsponse Compatibility score for winsorising and further analysis
  dplyr::mutate(zStroop.SRC = (Stroop.SRC - mean(Stroop.SRC, na.rm=T))/sd(Stroop.SRC, na.rm=T)) %>%
  #Winsorize extreme values or outliers
<<<<<<< HEAD
  dplyr::mutate(across(zStroop.SRC, ~Winsorize(., minval = -2.5, maxval = 2.5))) %>%
  #Reverse zStroop.SRC so that higher zscores means faster performance
  dplyr::mutate(zStroop.SRC.rev = zStroop.SRC * (-1))
=======
  dplyr::mutate(across(zStroop.SRC, ~Winsorize(., minval = -2.5, maxval = 2.5)))
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

#Export file with Stroop scores
rio::export(Stroop_scores, paste0(path.out, "Stroop_scores_",date, ".csv"))



###### Deary Liewald - SoP/Cognitive Processing Speed #######################
SoP_merged <- read.csv("./Data/SoP/SoP_merged_22072021.csv") %>%
  #To add age categories to SoP dataset
  dplyr::left_join(CRq, by=c("Participant.Public.ID" = "PublicID"))
# view(SoP_merged)
colnames(SoP_merged)

#Filter out columns and rows that won't be used (incl. excluding participants)
SoP_tidier <- SoP_merged %>%
  #Remove irrelevant columns/variables
  dplyr::select(Age.Category, UTC.Timestamp, UTC.Date, Experiment.Version, Task.Name, Participant.Private.ID, 
         Participant.Public.ID,Participant.Status,
         randomiser.3pls,order.2ubj, order.kx72, Trial.Number, Screen.Name,
         Reaction.Time,  Correct, Response, Answer1, display, ISI,
         Participant.Device.Type, Participant.OS, Participant.Browser) %>%
  #Remove irrelevant rows and only include participants in the ToInclude list
  dplyr::filter((display=="SRT_practicetrials" | display=="SRT_realtrials"|
                display=="CRT_practicetrials"| display=="CRT_realtrials"),
                (Participant.Public.ID %in% ToInclude)) %>%
  #Gorilla Experiment Builder uses branches. Hence, there are two columns indicating the order
  #of control tasks. Here, we merge them together.
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  #Rename variable name and values
  dplyr::rename("SoPTask"=Screen.Name) %>%
  dplyr::mutate(across(SoPTask, ~case_when(
    . == "SRT_PracticeTrials" ~ "SRT_practice",
    . == "SRT_RealTrials" ~ "SRT_real",
    . == "CRT_PracticeTrials" ~ "CRT_practice",
    . == "CRT_RealTrials" ~ "CRT_real"))) %>%
  dplyr::mutate(Reaction.Time = as.numeric(Reaction.Time)) %>%
  #Remove column with the public IDs for anonimity
  dplyr::select(-Participant.Public.ID)

#Check whether the necessary participants are exluded. Should be only FALSE
# unique(SoP_tidier$Participant.Public.ID %in% ToExclude)
#Save tidied dataset
rio::export(SoP_tidier, paste0(path.out, "SoP_tidier_",date, ".csv"))

### SCORES SoP - to obtain stroop scores for further analysis######
SoP_scores <- SoP_tidier %>%
  #Only include correct trials
  dplyr::filter(Correct==1) %>%
  #Exclude practice trials
  dplyr::filter(SoPTask=="SRT_real" | SoPTask=="CRT_real") %>%
  # dplyr::group_by(Age.Category, SoPTask) %>% 
  # ungroup() %>%
  #Group by individuals and SoP task (SRT and CRT)
  dplyr::group_by(Participant.Private.ID, SoPTask) %>%
  #Create mean scores per participants for SRT and CRT separately and create new dataset
  dplyr::summarise(RT = mean(Reaction.Time),
                   Age.Category = Age.Category[1]) %>%
  ungroup() %>%
  #Convert to wide format
  spread(SoPTask, RT)  %>%
  dplyr::rename("SoP.CRT"=CRT_real, "SoP.SRT"=SRT_real)
  

SoP_scores <- SoP_scores %>%
  #Replace missing values wih 0
  dplyr::mutate(across(SoP.CRT:SoP.SRT, ~replace_na(.,0))) %>%
  #Group by age category to obtain means per age group for single imputation for missing data
  group_by(Age.Category) %>%
  dplyr::mutate(across(SoP.CRT:SoP.SRT, ~case_when(
    . == 0 ~ mean(.),
    TRUE ~ .))) %>%
  ungroup() %>%
  #Create z scores per condition (SRT and CRT)
  dplyr::mutate(zSoP.SRT = (SoP.SRT - mean(SoP.SRT, na.rm=T))/sd(SoP.SRT, na.rm=T),
                zSoP.CRT = (SoP.CRT - mean(SoP.CRT, na.rm=T))/sd(SoP.CRT,na.rm=T)) %>%
  #Winsorize outliers
  dplyr::mutate(across(c(zSoP.SRT, zSoP.CRT), ~Winsorize(., minval = -2.5, maxval = 2.5))) %>%
  #Create composite SoP score per participant
  group_by(Participant.Private.ID) %>%
<<<<<<< HEAD
  dplyr::mutate(zSoP.comp = (zSoP.SRT+ zSoP.CRT)/2) %>%
  #Reverse RTs so that higher z scores means better performance
  dplyr::mutate(zSoP.comp.rev = zSoP.comp * (-1))
=======
  dplyr::mutate(zSoP.comp = (zSoP.SRT+ zSoP.CRT)/2)
>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca

#Export file with scores
rio::export(SoP_scores, paste0(path.out, "SoP_scores_",date, ".csv"))


###### Digit Reorder - WM/Working Memory #######################
WM_merged <- read.csv("./Data/Digits/WM_merged_22072021.csv") %>%
  #To add Age Categories to datafile
  left_join(CRq, by=c("Participant.Public.ID" = "PublicID"))
# view(WM_merged)
colnames(WM_merged)

#Filter out columns and rows that won't be used (incl. excluding participants)
WM_tidier <- WM_merged %>%
  #Filter out irrelevant columns/variables
  dplyr::select(Age.Category, UTC.Timestamp, UTC.Date, Experiment.Version, Task.Name, Participant.Private.ID,
         Participant.Public.ID,Participant.Status,
         randomiser.3pls,order.2ubj, order.kx72, TimeFix, TimeWhite, Trial.Number,
         Response, CorrectAns,  Correct, Reaction.Time, display, Attempt,
         Participant.Device.Type, Participant.OS, Participant.Browser) %>%
  #Filter out incorrect trials and excluded participants
  dplyr::filter(is.na(Attempt)==FALSE,
                (Participant.Public.ID %in% ToInclude)) %>%
  #Gorilla Experiment Builder uses branches. Hence, there are two columns indicating the order
  #of control tasks. Here, we merge them together.
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  #Remove columns Attempt (which now only contains one single value) and Public ID for anonimity
  dplyr::select(-Attempt, -Participant.Public.ID) %>%
  #In the datafile from Gorilla, some trials were missing values. Here, we correct that.
  dplyr::mutate(across(CorrectAns, ~case_when(
     (. == 12356 & Trial.Number == 13) ~ 123567,
     . == 123467 ~ 1234678,
     . == 234578 ~ 2345789,
     . == 124557 ~ 1245578,
     TRUE ~ as.numeric(.)))) %>%
  #Recode correct answers as 1
  dplyr::mutate(across(Correct, ~case_when(
    CorrectAns == Response ~ 1,
    TRUE ~ as.numeric(.))))

#Check whether the necessary participants are exluded. Should be only FALSE
# unique(WM_tidier$Participant.Public.ID %in% ToExclude)

#Export tidied data set
rio::export(WM_tidier, paste0(path.out, "WM_tidier_",date, ".csv"))

### SCORES WM - For further analysis
WM_scores <- WM_tidier %>%
  #Only include real trials (i.e., exclude practice trials)
  dplyr::filter(display=="RealTest") %>%
  #Count the number of digits in one digit string
  dplyr::mutate(Ndigits= str_count(CorrectAns)) %>%
  #Trials with digit sequences starting with 0 were missing 0's at the start (e.g., 455 should 
  #have been 0455). Here, we correct the number of digits in that string.
  dplyr::mutate(across(Ndigits, ~case_when(
    CorrectAns == 455 ~ 4,
    CorrectAns == 1447 ~ 5,
    CorrectAns == 12567 ~ 6,
    CorrectAns == 123567 ~ 7,
    TRUE ~ as.numeric(.)
  ))) %>%
  #Group by individuals to obtain an average WM score per participant
  dplyr::group_by(Participant.Private.ID) %>%
  #Score per digit sequence is number of digits * 0(incorrect) or 1 (correct), 
  #summed up for all digit sequences per participant
  dplyr::summarise(WM.Score = as.numeric(sum(Ndigits*Correct)), 
                   Age.Category = Age.Category[1]) %>%
  ungroup() %>%
  #Group by age category and impute means where there is a score of 0 (i.e., missing data)
  dplyr::group_by(Age.Category) %>%
  dplyr::mutate(across(WM.Score, ~case_when(
    . == 0 ~ round(mean(.)),
    TRUE ~ .
  ))) %>%
  ungroup()
  
#Create a dataset with final WM scores
  WM_scores2 <- WM_scores %>%
    #Create z scores for winsorising outliers
    dplyr::mutate(zWM.Score = (WM.Score - mean(WM.Score, na.rm=T))/sd(WM.Score, na.rm=T)) %>%
    dplyr::mutate(across(zWM.Score, ~Winsorize(., minval=-2.5, maxval=2.5))) %>%
    dplyr::select(-Age.Category)
  
  #Export dataset with scores for further analysis
rio::export(WM_scores2, paste0(path.out, "WM_scores_",date, ".csv"))

<<<<<<< HEAD
WM <- read.csv("./Data/Tidy/WM_scores_final.csv")
SoP <- read.csv("./Data/Tidy/SoP_scores_final.csv")
Stroop <- read.csv("./Data/Tidy/Stroop_scores_final.csv")

# Create Composite score for General Cognitive Processing
GenCogProc <- WM %>%
  left_join(SoP, by="Participant.Private.ID") %>%
  left_join(Stroop, by="Participant.Private.ID") %>%
  mutate(GenCogProc.composite = rowMeans(select(., zWM.Score, zSoP.comp.rev, zStroop.SRC.rev)))  %>%
  select(Participant.Private.ID, Age.Category, GenCogProc.composite)
  
rio::export(GenCogProc, paste0(path.out, "GenCogComposite_", date, ".csv"))
  
=======

>>>>>>> 43b58371cd77143e3b3e24fb20029d1ee1ad06ca
