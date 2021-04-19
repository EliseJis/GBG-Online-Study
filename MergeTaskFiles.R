################ Script to merge files of the same task ##################

require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(rio)

date = format(Sys.Date(), "%d%m%Y")

## Step 1: copy all files into the same folder+ make sure they all have the same extension (.csv)

## Step 2: set path out
path.out <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Merged/"

##### Stroop - EF #######
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Stroop")

cStroop_v24 <- rbind(read_csv("14122020_v24_SpatialStroop_langfirst.csv"))
cStroop_v25 <- rbind(read_csv("All_GBGv25_SpatialStroop_controlfirst.csv"), read_csv("All_GBGv25_SpatialStroop_langfirst.csv"))
cStroop_v27 <- rbind(read_csv("14122020_v27_SpatialStroop_controlfirst.csv"), read_csv("14122020_v27_SpatialStroop_langfirst.csv"))
cStroop_v28 <- rbind(read_csv("14122020_v28_SpatialStroop_controlfirst.csv"), read_csv("14122020_v28_SpatialStroop_langfirst.csv"))
cStroop_v30 <- rbind(read_csv("03032021_v30_SpatialStroop_controlfirst.csv"), read_csv("03032021_v30_SpatialStroop_langfirst.csv"))

colnames(cStroop_v25)==colnames(cStroop_v30)

Stroop_merged <- rbind(cStroop_v24, cStroop_v25, cStroop_v27, cStroop_v28, cStroop_v30)
rio::export(Stroop_merged, paste0(path.out, "Stroop_merged_",date, ".csv"))

##### Deary-Liewald task - SoP ######
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/SoP")

cSoP_v25 <- rbind(read_csv("All_GBGv25_DearyLiewald_controlfirst.csv"), read_csv("All_GBGv25_DearyLiewald_langfirst.csv"))
cSoP_v27 <- rbind(read_csv("14122020_v27_DearyLiewald_controlfirst.csv"), read_csv("14122020_v27_DearyLiewald_langfirst.csv"))
cSoP_v30 <- rbind(read_csv("19032021_v30_DearyLiewald_controlfirst.csv"), read_csv("19032021_v30_DearyLiewald_langfirst.csv"))

SoP_merged <- rbind(cSoP_v25, cSoP_v27, cSoP_v30)
rio::export(SoP_merged, paste0(path.out, "SoP_merged_", date, ".csv"))

##### Digit Reordering - WM ######
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Digits")

cDigits_v24 <- rbind(read_csv("14122020_v24_DigitReordering_langfirst.csv"))
cDigits_v25 <- rbind(read_csv("All_GBGv25_DigitReordering_controlfirst.csv"), read_csv("All_GBGv25_DigitReordering_langfirst.csv"))
cDigits_v27 <- rbind(read_csv("14122020_v27_DigitReordering_controlfirst.csv"), read_csv("14122020_v27_DigitReordering_langfirst.csv"))
cDigits_v30 <- rbind(read_csv("19032021_v30_DigitReordering_controlfirst.csv"), read_csv("19032021_v30_DigitReordering_langfirst.csv"))

colnames(cDigits_v24)==colnames(cDigits_v30)

WM_merged <- rbind(cDigits_v24, cDigits_v25, cDigits_v27, cDigits_v30)
rio::export(WM_merged, paste0(path.out, "WM_merged_", date, ".csv"))

##### Verbal Fluency -VF ######
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/VF")

cVF_v24 <- rbind(read_csv("14122020_v24_VerbalFluency_langfirst.csv"))
cVF_v25 <- rbind(read_csv("All_GBGv25_VerbalFluency_controlfirst.csv"), read_csv("All_GBGv25_VerbalFluency_langfirst.csv"))
cVF_v27 <- rbind(read_csv("14122020_v27_VerbalFluency_controlfirst.csv"), read_csv("14122020_v27_VerbalFluency_langfirst.csv"))
cVF_v30 <- rbind(read_csv("19032021_v30_VerbalFluency_controlfirst.csv"), read_csv("19032021_v30_VerbalFluency_langfirst.csv"))

colnames(cVF_v30)==colnames(cVF_v25)

VF_merged <- rbind(cVF_v24, cVF_v25, cVF_v27, cVF_v30)
rio::export(VF_merged, paste0(path.out, "VF_merged_", date, ".csv"))

##### Picture Naming Objects - PNobj #####
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/PN_objects")

cPNobj_v24 <- rbind(read_csv("14122020_v24_PNobjectsList1_langfirst.csv"))
cPNobj_v25 <- rbind(read_csv("All_GBGv25_PNobjectsList1_controlfirst.csv"), read_csv("All_GBGv25_PNobjectsList1_langfirst.csv"),
                    read_csv("All_GBGv25_PNobjectsList2_controlfirst.csv"), read_csv("All_GBGv25_PNobjectsList2_langfirst.csv"))
cPNobj_v27 <- rbind(read_csv("14122020_v27_PNobjectsList1_controlfirst.csv"), read_csv("14122020_v27_PNobjectsList1_langfirst.csv"),
                    read_csv("14122020_v27_PNobjectsList2_controlfirst.csv"), read_csv("14122020_v27_PNobjectsList2_langfirst.csv"))
cPNobj_v30 <- rbind(read_csv("19032021_v30_PNobjectsList1_controlfirst.csv"), read_csv("19032021_v30_PNobjectsList1_langfirst.csv"),
                    read_csv("19032021_v30_PNobjectsList2_controlfirst.csv"), read_csv("19032021_v30_PNobjectsList2_langfirst.csv"))

PNobj_merged <- rbind(cPNobj_v24, cPNobj_v25, cPNobj_v27, cPNobj_v30)
rio::export(PNobj_merged, paste0(path.out, "PNobj_merged_", date, ".csv"))


##### Picture Naming Objects - PNact #####
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/PN_actions")

cPNact_v24 <- rbind(read_csv("14122020_v24_PNactionsList2_ObjectsList1_langfirst.csv"))
cPNact_v25 <- rbind(read_csv("All_GBGv25_PNactionsList1_ObjectsList1_controlfirst.csv"),
                    read_csv("All_GBGv25_PNactionsList2_ObjectsList1_controlfirst.csv"),
                    read_csv("All_GBGv25_PNactionsList1_ObjectsList2_controlfirst.csv"),
                    read_csv("All_GBGv25_PNactionsList2_ObjectsList2_controlfirst.csv"), read_csv("All_GBGv25_PNactionsList2_ObjectsList2_langfirst.csv"))
cPNact_v27 <- rbind(read_csv("14122020_v27_PNactionsList1_ObjectsList1_controlfirst.csv"), read_csv("14122020_v27_PNactionsList1_ObjectsList1_langfirst.csv"),
                    read_csv("14122020_v27_PNactionsList2_ObjectsList1_controlfirst.csv"), read_csv("14122020_v27_PNactionsList2_ObjectsList1_langfirst.csv"),
                    read_csv("14122020_v27_PNactionsList1_ObjectsList2_controlfirst.csv"), read_csv("14122020_v27_PNactionsList1_ObjectsList2_langfirst.csv"),
                    read_csv("14122020_v27_PNactionsList2_ObjectsList2_controlfirst.csv"), read_csv("14122020_v27_PNactionsList2_ObjectsList2_langfirst.csv"))
cPNact_v30 <- rbind(read_csv("19032021_v30_PNactionsList1_ObjectsList1_controlfirst.csv"), read_csv("19032021_v30_PNactionsList1_ObjectsList1_langfirst.csv"),
                    read_csv("19032021_v30_PNactionsList2_ObjectsList1_controlfirst.csv"), read_csv("19032021_v30_PNactionsList2_ObjectsList1_langfirst.csv"),
                    read_csv("19032021_v30_PNactionsList1_ObjectsList2_controlfirst.csv"), read_csv("19032021_v30_PNactionsList1_ObjectsList2_langfirst.csv"),
                    read_csv("19032021_v30_PNactionsList2_ObjectsList2_controlfirst.csv"), read_csv("19032021_v30_PNactionsList2_ObjectsList2_langfirst.csv"))

colnames(cPNact_v24)==colnames(cPNact_v25)

PNact_merged <- rbind(cPNact_v24, cPNact_v25, cPNact_v27, cPNact_v30)
rio::export(PNact_merged, paste0(path.out, "PNact_merged_", date, ".csv"))
