library(here)
setwd(here())
library(readxl)
library(dplyr)
library(lubridate)

file <- 'Download Your Data_Full Data_data-Burundi8-29-22.csv'
  #'sims_asmt_burundi_question_data_v2.csv' #Data provided by Isaiah Lee.

Bur_SIMSin <- read.csv(file = file)
Bur_SIMSin$orgUnituid  <- Bur_SIMSin$"LOWEST_OU_UID" 

Genie <-read_xlsx("Genie_SIMS_Site_IM_Burundi_4ad00932-4bce-4ccf-9030-d41d6aaaa8bf.xlsx", sheet = "org")

Bur_SIMS <- merge(x=Bur_SIMSin, y=Genie, by=c("orgUnituid"))

Bur_SIMS$Date <- substr(Bur_SIMS$ASSESSMENT_DATE, 1, 9)
Bur_SIMS$Date <- as.Date(Bur_SIMS$Date, format= "%d-%b-%y")
Bur_SIMS$ASSESSMENT_YEAR <- substr(Bur_SIMS$ASSESSMENT_QUARTER, 1, 4)

##This was the first date these sites were visited. 
Bur_SIMS_first <- setDT(Bur_SIMS)[order(Date), head(.SD, 1L), by = LOWEST_OU_UID]
Bur_SIMS_first <- select(Bur_SIMS_first, c('LOWEST_OU_UID', 'Date'))
Bur_SIMS_first$AssessmentStatus <- "Initial Assessment" 

Bur_SIMS2 <- merge(x=Bur_SIMS, y=Bur_SIMS_first, by=c("LOWEST_OU_UID", "Date"), all.x=TRUE)

##Now, make an indicator for the LAST visit. (this might be the same as the first visit)
Bur_SIMS_last <-unique(Bur_SIMS[order(Date)], by='LOWEST_OU_UID', fromLast=TRUE)
Bur_SIMS_last <- select(Bur_SIMS_last, c('LOWEST_OU_UID', 'Date'))
Bur_SIMS_last$AssessmentStatus1 <- "Last Assessment" ##This was the last date these sites were visited. 

Bur_SIMS_1 <- merge(x=Bur_SIMS2, y=Bur_SIMS_last, by=c("LOWEST_OU_UID", "Date"), all.x=TRUE)

##number of assessments 
 Bur_SIMSnumbervisits <- Bur_SIMS %>%  group_by(LOWEST_OU_UID) %>%
  summarise(n_distinct(Date))
 Bur_SIMSnumbervisits <-  Bur_SIMSnumbervisits  %>% rename(NumberofVisits =  "n_distinct(Date)")
 
Bur_SIMS1 <- merge(x=Bur_SIMS_1, y=Bur_SIMSnumbervisits, by=c("LOWEST_OU_UID"), all.x=TRUE)

Bur_SIMS1$AssessmentStatus[is.na(Bur_SIMS1$AssessmentStatus) == TRUE & Bur_SIMS1$AssessmentStatus1 == "Last Assessment"] <- "Follow-up/last assessment"
Bur_SIMS1$AssessmentStatus[is.na(Bur_SIMS1$AssessmentStatus) == TRUE & is.na(Bur_SIMS1$AssessmentStatus1) == TRUE] <- "Follow-up assessment"

Bur_SIMS1$AssessmentStatus[is.na(Bur_SIMS1$AssessmentStatus) == TRUE] <- "Follow-up assessment"
Bur_SIMS1$AssessmentStatus[Bur_SIMS1$AssessmentStatus == "Initial Assessment" & 
                             (Bur_SIMS1$SCORE == "Red" | Bur_SIMS1$SCORE == "Yellow")] <- "Initial/Need to return"
Bur_SIMS1$AssessmentStatus[Bur_SIMS1$AssessmentStatus == "Follow-up assessment" & 
                             (Bur_SIMS1$SCORE == "Red" | Bur_SIMS1$SCORE == "Yellow")] <- "Follow-up/Need to return"
Bur_SIMS1$AssessmentStatus[Bur_SIMS1$AssessmentStatus == "Follow-up/last assessment" & 
                             (Bur_SIMS1$SCORE == "Red" | Bur_SIMS1$SCORE == "Yellow")] <- "Last assessment/Need to return"


Bur_SIMSmain <- Bur_SIMS1 %>%  dplyr::filter(duplicated(Bur_SIMS1[,c("ASSESSMENT_DATE", "LOWEST_OU_UID", "CEE_LONG_ID")]) == ("FALSE")) #This is to get a single score from that date

####Number of scores per site and date
Bur_SIMSmain$n <- 1

Bur_SIMSmainnumberscores <-  aggregate(Bur_SIMSmain$n, by = list(Bur_SIMSmain$LOWEST_OU_UID, Bur_SIMSmain$Date), FUN = sum)

Bur_SIMSmainnumberscores <- Bur_SIMSmainnumberscores %>% rename(LOWEST_OU_UID = Group.1,
                                                                Date = Group.2, 
                                                                numberofscores = x)

Bur_SIMSmain1 <- merge(x=Bur_SIMSmain, y=Bur_SIMSmainnumberscores, by=c("LOWEST_OU_UID", "Date"), all.x=TRUE)

Bur_SIMSmain1$MorethanoneCEEassessment[Bur_SIMSmain1$NumberofVisits >= 2] <- 1  

## Which CEEs can be "deleted" since they should have been rechecked
Bur_SIMSmain1 <- Bur_SIMSmain1 %>% mutate(AssessmentWasRechecked = ifelse(((NumberofVisits == 2 & 
                                  AssessmentStatus == "Initial/Need to return") | (NumberofVisits >= 3 & 
                                    AssessmentStatus == "Follow-up/Need to return")), 1, 0)) #if NumberofVisits == 2 and AssessmentStatus == "Initial/Need to return" then we can technically "delete" this since these should have been checked again in the second visit  # if NumberofVisits >= 3 and AssessmentStatus == "Follow-up/Need to return" then we can technically "delete" this since these should have been checked again in the third visit

##ID required versus non-required

condition <- Bur_SIMSmain1$CEE_LONG_ID %in% c("S_02_01", "S_02_02", "S_02_03", 
                                              "S_02_05",  "S_02_07", "S_02_08", 
                                              "S_02_10", "S_02_18",  "S_02_19",
                                              "S_02_22",  "S_02_23", "S_02_27") #checking on list with team

Bur_SIMSmain1$Required[condition] <- 1

