library(here)
setwd(here())
library(readxl)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(fastDummies)
library(RColorBrewer)
library(ggplot2)


files <- list.files(pattern = "\\.csv$")

#temparary data frame to load the contents on the current file
temp_df <- data.frame(ModelName = character(), Object = character(),stringsAsFactors = F)

#reading each file within the range and append them to create one file
for (i in 1:length(files)){
  #read the file
  currentFile = read.csv(files[i])
  
  #Append the current file
  temp_df =  bind_rows(temp_df, currentFile)    
} 

OUName <- "Nigeria" ###Put your OU name here
SiteOnly <- 1 #1 if we want to focus on Site data, not above-site data, 0 if we keep both

SIMSdatain <- temp_df
colnames(SIMSdatain)[grepl("DATIM.Location.UID", colnames(SIMSdatain))] <- "orgUnituid" #"LOWEST_OU_UID"  --> question-level data uses _ instead of .
SIMSdatain$X1 <- 1
SIMSdatain$SNU1.Name[SIMSdatain$SNU1.Name == "Tajikistan (ZtoVYbNCnsj)"]<- "Tajikistan"
SIMSdatain$SNU1.Name[SIMSdatain$SNU1.Name == "Phnom Penh"]<- "Phnom Penh"
SIMSdatain$SNU1.Name[SIMSdatain$SNU1.Name == "Phnom Penh"]<- "Cambodia"
SIMSdatain$SNU1.Name[SIMSdatain$SNU1.Name == "Central Asia Region"]<- "Kazakhstan" ###I think this is correct, but there are no clues to the OU that I can see, except for the team lead being Khorlan Izmailova; when I google her, she seems to be from Kazakstan
SIMSdatain$OU.Name[SIMSdatain$OU.Name == "Asia Regional Program"] <- "Asia Region"
SIMSdatain$OU.Name[SIMSdatain$OU.Name == "Central Asia Region"] <- "Asia Region"
SIMSdatain$OU.Name[SIMSdatain$OU.Name == "Cambodia"] <- "Asia Region"

SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Abt Associates Inc."] <- "Abt Associates, Inc."
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "AFRICAN EVANGELISTIC ENTERPRI SE"] <- "African Evangelistic Enterprise"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Chemonics International"] <- "Chemonics International, Inc."
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "DELOITTE CONSULTING LIMITED"] <- "Deloitte Consulting Limited"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Education Development Center"] <- "Education Development Center, Inc."
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Elizabeth Glaser Pediatric Aids Foundation"] <- "Elizabeth Glaser Pediatric AIDS Foundation"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "HEALTH INITIATIVES FOR SAFETY AND STABILITY IN AFRICA"] <- "Health Initiatives for Safety and Stability in Africa"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "INTRAHEALTH INTERNATIONAL, INC."] <- "IntraHealth International, Inc"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "JHPIEGO"] <- "JHPIEGO CORPORATION"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "John Snow Inc (JSI)"] <- "John Snow, Inc."
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "John Snow, Incorporated"] <- "John Snow, Inc."
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Moi Teaching and Referral Hospital"] <- "MOI TEACHING AND REFERRAL HOSPITAL"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "NACOSA (Networking AIDS Community of South Africa)"] <- "NACOSA"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Partners in Hope"] <- "Partners In Hope"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Project Hope Namibia"] <- "PROJECT HOPE NAMIBIA"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Save The Children Federation Inc"] <- "Save The Children Federation, Inc."
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "TONATA PLHIV NETWORK"] <- "TONATA PLHIV Network"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "University Research Corporation, LLC"] <- "University Research Co., LLC"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Family Health International"] <- "FHI 360"
SIMSdatain$Prime.Partner[SIMSdatain$Prime.Partner == "Heartland Alliance for Human Needs and Human Rights"] <- "HEARTLAND ALLIANCE LTD-GTE"

SIMSdata <- SIMSdatain #I used to merge in Genie data here, but I don't think I can do it here
#SIMSdata$Date <- substr(SIMSdata$ASSESSMENT_DATE, 1, 9)

SIMSdata$DateAssessment <- as.Date(SIMSdata$Assessment.Date, format= "%m/%d/20%y")
#SIMSdata$ASSESSMENT_YEAR <- substr(SIMSdata$ASSESSMENT_QUARTER, 1, 4)
SIMSdata$AssessmentFY <- substr(SIMSdata$Assessment.Quarter, 1, 4)

SIMSdata_filtered <- filter(SIMSdata, OU.Name == OUName)

SIMSdata_filtered$Follow.Up.Date.Deadline2 <- as.Date(SIMSdata_filtered$Follow.Up.Date.Deadline, "%m/%d/20%y")

SIMSdata_filtered <- SIMSdata_filtered %>%  mutate(FY22followup = ifelse(SIMSdata_filtered$Follow.Up.Date.Deadline2 >=
                            as.Date("2021-10-01") &  SIMSdata_filtered$Follow.Up.Date.Deadline2 <= as.Date("2022-09-30"), 1, 0)) #I'm checking to see if there is any clean/easy way to get completed/missed/upcoming####

SIMSdata_filtered$Assessment.Date2 <- as.Date(SIMSdata_filtered$Assessment.Date, "%m/%d/20%y")

SIMSdata_filtered2 <- SIMSdata_filtered %>% filter(if(SiteOnly==1) Tool.Type == "Site" else TRUE) ##Filtering to Site Only
  #{if (SiteOnly==1) filter(., Tool.Type == "Site") else TRUE} 

paste("SIMS Summary:", OUName, "FY22")

comp_n22<-as.integer(length(unique(filter(SIMSdata_filtered2, SIMSdata_filtered2$Assessment.FY == 2022 & SIMSdata_filtered2$Assessment.Type == "Comprehensive")$Assessment.ID)) )
FU_n22<-as.integer(length(unique(filter(SIMSdata_filtered2, SIMSdata_filtered2$Assessment.FY == 2022 & SIMSdata_filtered2$Assessment.Type == "Follow Up")$Assessment.ID)) )
paste("This report summarizes key findings from the last FY of completed (Site) SIMS assessments. In FY22,",comp_n22, 
      "comprehensive, and",FU_n22, "follow-up, assessments were completed in the OU.")

#get # assessments completed in FY22, including #follow-up assessments, and # assessments missed

##This was the first date these sites were visited. 
SIMSdata_visits  <- SIMSdata_filtered2 %>% select("SNU1.Name", "orgUnituid", "Assessment.ID", "DateAssessment") %>% distinct
SIMSdata_visits <- SIMSdata_visits %>%
  group_by(SNU1.Name, orgUnituid) %>%
  mutate(Ordervisit = order(order(Assessment.ID, decreasing=FALSE))) %>%
  mutate(Ordervisitlast = order(order(Assessment.ID, decreasing=TRUE)))

SIMSdata_visits = subset(SIMSdata_visits, select = -c(DateAssessment))
SIMSdata_1 <- merge(x=SIMSdata_filtered2, y=SIMSdata_visits, by=c("SNU1.Name", "orgUnituid", "Assessment.ID"), all.x=TRUE)

##This was the first date these sites were visited. 
SIMSdata_1$AssessmentStatus[SIMSdata_1$Ordervisit == 1] <- "Initial Assessment" 
SIMSdata_1$AssessmentStatus1[SIMSdata_1$Ordervisitlast == 1] <- "Last Assessment" 

library(dplyr)
SIMSdatanumbervisits <-  SIMSdata_1 %>% filter(!is.na(Assessment.ID)) %>% 
  #dplyr::distinct(Assessment.ID) %>%
  group_by(orgUnituid) %>% 
  #summarise(n()) %>%
  summarise(Numberofvisits = n_distinct(Assessment.ID)) 


SIMSdata1 <- merge(x=SIMSdata_1, y=SIMSdatanumbervisits, by=c("orgUnituid"), all.x=TRUE)

SIMSdata1$AssessmentStatus[is.na(SIMSdata1$AssessmentStatus) == TRUE & SIMSdata1$AssessmentStatus1 == "Last Assessment"] <- "Follow-up/last assessment"
SIMSdata1$AssessmentStatus[is.na(SIMSdata1$AssessmentStatus) == TRUE & is.na(SIMSdata1$AssessmentStatus1) == TRUE] <- "Follow-up assessment"

SIMSdata1$AssessmentStatus[is.na(SIMSdata1$AssessmentStatus) == TRUE] <- "Follow-up assessment"
SIMSdata1$AssessmentStatus[SIMSdata1$AssessmentStatus == "Initial Assessment" & 
                             (SIMSdata1$SCORE == "Red" | SIMSdata1$SCORE == "Yellow")] <- "Initial/Need to return"
SIMSdata1$AssessmentStatus[SIMSdata1$AssessmentStatus == "Follow-up assessment" & 
                             (SIMSdata1$SCORE == "Red" | SIMSdata1$SCORE == "Yellow")] <- "Follow-up/Need to return"
SIMSdata1$AssessmentStatus[SIMSdata1$AssessmentStatus == "Follow-up/last assessment" & 
                             (SIMSdata1$SCORE == "Red" | SIMSdata1$SCORE == "Yellow")] <- "Last assessment/Need to return"

#SIMSdatamain1$RepeatedCEEs <- duplicated(SIMSdatamain1[,c('orgUnituid', 'CEE.Number')])
SIMSdatamain <- SIMSdata1
####Number of scores per site and date
SIMSdatamain$n <- 1

SIMSdatamainnumberscores <-  aggregate(SIMSdatamain$n, by = list(SIMSdatamain$"orgUnituid", SIMSdatamain$DateAssessment), FUN = sum)

SIMSdatamainnumberscores <- SIMSdatamainnumberscores %>% rename("orgUnituid" = Group.1,
                                                                DateAssessment = Group.2, 
                                                                numberofscores = x)

SIMSdatamain1 <- merge(x=SIMSdatamain, y=SIMSdatamainnumberscores, by=c("orgUnituid", "DateAssessment"), all.x=TRUE)

SIMSdatamain1 <- SIMSdatamain1 %>%   group_by(orgUnituid, CEE.Number) %>%   mutate(RepeatedCEEs = n())  #>1 To provide T/F. 2/3 are duplicated

SIMSdatamain1$MorethanoneCEEassessment <- as.integer("")
SIMSdatamain1$MorethanoneCEEassessment[SIMSdatamain1$Numberofvisits >= 2] <- 1
               
SIMSdatamain1 <- SIMSdatamain1 %>% mutate(FinalAssessmentStatus = case_when(AssessmentStatus == "Initial Assessment" &
                                                                              is.na(AssessmentStatus1) == TRUE ~ "Initial assessment",
                                                                            AssessmentStatus == "Initial Assessment" &  AssessmentStatus1 == "Last Assessment" ~ 
                                                                              "Initial (and only) assessment",
                                                                            AssessmentStatus == "Follow-up assessment" &  is.na(AssessmentStatus1) == TRUE ~ 
                                                                              "Follow-up assessment", 
                                                                            AssessmentStatus == "Follow-up assessment" &  AssessmentStatus1 == "Last Assessment" ~ 
                                                                              "Follow-up (and last) assessment",
                                                                            AssessmentStatus == "Follow-up/last assessment" ~ "Follow-up (and last) assessment"
)) ## this status could help us eventually determine Which CEEs can be "deleted" since they should have been rechecked in follow-ups. 
#create a duplicate for CEE.Number and orgUnituid. Essentially, we could have one group of core scores from a orgUnituid. If there is no dup then keep that score.
#If there are two dups then "delete" the one from the initial assessment. #if there are three dups then keep the one from the final
#If it is the initial and only assessment nothing will be deleted.

SIMSdatamain1 <- SIMSdatamain1 %>% mutate(NumericalScore = case_when(Score == "Green"  ~ 3,
                                                                     Score == "Yellow"  ~ 2,
                                                                     Score == "Red"  ~ 1)) 

#####put in indicators for "Improved in next assessment" and "Declined in next assessment"
SIMSdatafilteredtodups <- subset(filter(SIMSdatamain1, RepeatedCEEs >= 2), select = c(orgUnituid, CEE.Number, NumericalScore, Ordervisit))
RSSIMSdatafilteredtodups <-  
  tidyr::pivot_wider(SIMSdatafilteredtodups, names_from = Ordervisit, values_from = NumericalScore, values_fn = list) %>% 
  unnest(cols = everything() )

# this needs to be updated
RSSIMSdatafilteredtodups$ChangeinCEE <- is.numeric("")
#  RSSIMSdatafilteredtodups$ChangeinCEENumerical <- is.numeric("")
RSSIMSdatafilteredtodups$ChangeinCEENumerical <- (RSSIMSdatafilteredtodups$"4" - RSSIMSdatafilteredtodups$"1")
RSSIMSdatafilteredtodups$ChangeinCEENumerical2 <- (RSSIMSdatafilteredtodups$"3" - RSSIMSdatafilteredtodups$"1")
RSSIMSdatafilteredtodups$ChangeinCEENumerical3 <- (RSSIMSdatafilteredtodups$"2" - RSSIMSdatafilteredtodups$"1")
RSSIMSdatafilteredtodups$ChangeinCEENumerical3a <- (RSSIMSdatafilteredtodups$"3" - RSSIMSdatafilteredtodups$"2")

RSSIMSdatafilteredtodups$ChangeinCEENumerical[which(is.na(RSSIMSdatafilteredtodups$ChangeinCEENumerical) == TRUE)] <- RSSIMSdatafilteredtodups$ChangeinCEENumerical2[which(is.na(RSSIMSdatafilteredtodups$ChangeinCEENumerical) == TRUE)]
RSSIMSdatafilteredtodups$ChangeinCEENumerical[which(is.na(RSSIMSdatafilteredtodups$ChangeinCEENumerical) == TRUE)] <- RSSIMSdatafilteredtodups$ChangeinCEENumerical3[which(is.na(RSSIMSdatafilteredtodups$ChangeinCEENumerical) == TRUE)]
RSSIMSdatafilteredtodups$ChangeinCEENumerical[which(is.na(RSSIMSdatafilteredtodups$ChangeinCEENumerical) == TRUE)] <- RSSIMSdatafilteredtodups$ChangeinCEENumerical3a[which(is.na(RSSIMSdatafilteredtodups$ChangeinCEENumerical) == TRUE)]

RSSIMSdatafilteredtodups$ChangeinCEE[which(RSSIMSdatafilteredtodups$ChangeinCEENumerical > 0)] <- "Improved"
RSSIMSdatafilteredtodups$ChangeinCEE[which(RSSIMSdatafilteredtodups$ChangeinCEENumerical == 0)] <- "No.change"
RSSIMSdatafilteredtodups$ChangeinCEE[which(RSSIMSdatafilteredtodups$ChangeinCEENumerical == 0 & RSSIMSdatafilteredtodups$"1" == 3)] <- "No.change/no.need.for.change.from.green"
RSSIMSdatafilteredtodups$ChangeinCEE[which(RSSIMSdatafilteredtodups$ChangeinCEENumerical == 0 & RSSIMSdatafilteredtodups$"3" == 3 & 
                                             RSSIMSdatafilteredtodups$"2" == 3 & is.na(RSSIMSdatafilteredtodups$"1"))] <-      "No.change/no.need.for.change.from.green"
RSSIMSdatafilteredtodups$ChangeinCEE[which(RSSIMSdatafilteredtodups$ChangeinCEENumerical < 0)] <- "Declined"

SIMSdatamainwithchange <- merge(x=SIMSdatamain1, y=RSSIMSdatafilteredtodups[,c("orgUnituid", "CEE.Number", "ChangeinCEE")],
                                by=c("orgUnituid", "CEE.Number"), all.x=TRUE)

SIMSdatamainwithchange$ChangeinCEE[which(is.na(SIMSdatamainwithchange$ChangeinCEE))] <- "Not.re-assessed"

SIMSdatamainwithchange <- fastDummies::dummy_cols(SIMSdatamainwithchange, select_columns = c("ChangeinCEE")) 

SIMSdatamain1 <-SIMSdatamainwithchange

SIMSdatamain1 <- SIMSdatamain1 %>% mutate(AssessmentWasRechecked = ifelse(((Numberofvisits == 2 & 
                                                                              AssessmentStatus == "Initial/Need to return") | (Numberofvisits >= 3 & 
                                                                                                                                 AssessmentStatus == "Follow-up/Need to return")), 1, 0)) #if NumberofVisits == 2 and AssessmentStatus == "Initial/Need to return" then we can technically "delete" this since these should have been checked again in the second visit  # if NumberofVisits >= 3 and AssessmentStatus == "Follow-up/Need to return" then we can technically "delete" this since these should have been checked again in the third visit
#ID required versus non-required
conditionsite <- SIMSdatamain1$CEE.Number %in% c("1.02", "1.07", "1.08", "1.1", "1.11", "1.21", "1.22", "2.01", "2.02", "2.03", "2.05", "2.07", "2.08", "2.1", "2.18", "2.19", "2.22", 
                                                 "2.23", "2.27", "3.03", "3.05", "3.08", "3.09", "3.1", "3.12", "3.14", "3.15", "3.17", "4.01", "4.02", "4.04", "4.07", "4.08", "4.1", "4.13",
                                                 "4.14", "4.19", "4.2", "5.02", "6.02", "6.04", "6.06", "6.08", "7.01", "7.03", "8.02", "9.01", "9.03", "9.04", "10.01", "10.08",
                                                 "10.09") #required for sites 
conditionabovesite <- SIMSdatamain1$Set.Number %in% c("5", "6", "8", "9") #required for above-site

conditiontreatment <- SIMSdatamain1$Set.Number %in% c("2A", "2B") #Need to confirm, but these are Care and Treatment-General Population

SIMSdatamain1$Required <- NA
SIMSdatamain1$TreatmentCEEs <- NA

SIMSdatamain1$Required[conditionsite & SIMSdatamain1$Tool.Type == "Site"] <- 1

SIMSdatamain1$Required[conditionabovesite & SIMSdatamain1$Tool.Type == "Above Site"] <- 1

SIMSdatamain1$Required[is.na(SIMSdatamain1$Required) == TRUE] <- 0

SIMSdatamain1$TreatmentCEEs[conditiontreatment] <- 1

SIMSdatamain1$TreatmentCEEs[is.na(SIMSdatamain1$TreatmentCEEs) == TRUE] <- 0

###Some ways of looking at it: 
#1) "complete" scores from a site, then 1a. maybe adding up percent of greens for the site (and for this, try filtering for required only);
#2) Filter complete scores, and then seeing overall the egregiousness. (percent). can also look at required scores only (or treatment, as Ryan was doing for Burundi) 

SIMSdatamain1 <- SIMSdatamain1 %>%  mutate(SiteLevelFullScores = 
                                             ifelse(FinalAssessmentStatus == "Initial (and only) assessment" | RepeatedCEEs == 1 |
                                                      (RepeatedCEEs >= 2 & FinalAssessmentStatus == "Follow-up (and last) assessment"), 1, 0))

cols.num <- c("Score.for.Green", "Score.for.Red", "Score.for.Red...Yellow", "Score.for.Yellow")
SIMSdatamain1[cols.num][SIMSdatamain1[cols.num]!=""] <- "1"
SIMSdatamain1[cols.num][SIMSdatamain1[cols.num]==""] <- "0"
SIMSdatamain1[cols.num] <- sapply(SIMSdatamain1[cols.num],as.numeric)

SIMSdatamain1$Green <- 3
SiteLevelFullScores <- dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1) ####This is the "complete" scores from a site, removing CEEs that were late rescored

setwd(here("./output/"))
fwrite(SiteLevelFullScores, file=paste("sitelevelfullscores", ".csv"))

#####Looking at scores at the site-level (i.e. aggregating scores per site) #####

SIMSdataassessmentsite <- data.frame(aggregate(list(TotalScores =  SIMSdatamain1$NumericalScore,
                               Total = SIMSdatamain1$Green, Count = SIMSdatamain1$X1), by=list(SNU1.Name = SIMSdatamain1$SNU1.Name,
                                    orgUnituid = SIMSdatamain1$orgUnituid,
                                    AssessmentID = SIMSdatamain1$"Assessment.ID"), FUN = sum)) ###aggregate by assessment ID, site

SIMSdataall <- data.frame(aggregate(list(TotalScores =  SIMSdatamain1$NumericalScore,
                                         Total = SIMSdatamain1$Green, TotalDeclined = SIMSdatamain1$ChangeinCEE_Declined,
                                         TotalNotReassessed =  SIMSdatamain1$"ChangeinCEE_Not.re-assessed", 
                                         TotalGreenBefore =  SIMSdatamain1$"ChangeinCEE_No.change/no.need.for.change.from.green", Count = SIMSdatamain1$X1), 
                                    by=list(SNU1.Name = SIMSdatamain1$SNU1.Name, orgUnituid = SIMSdatamain1$orgUnituid), FUN = sum))

SIMSdatawithoutdups <- data.frame(aggregate(list(TotalScoresnodups =  SiteLevelFullScores$NumericalScore,
                                                 Totalnodups = SiteLevelFullScores$Green, 
                                                 TotalDeclinednodups = SiteLevelFullScores$ChangeinCEE_Declined,
                                                 TotalNotReassessednodups =  SiteLevelFullScores$"ChangeinCEE_Not.re-assessed", 
                                                 TotalGreenBeforenodups =  SiteLevelFullScores$"ChangeinCEE_No.change/no.need.for.change.from.green",
                                                 Countnodups = SiteLevelFullScores$X1), by=list(SNU1.Name = SiteLevelFullScores$SNU1.Name,
                                                                                                orgUnituid = SiteLevelFullScores$orgUnituid), FUN = sum))

SIMSdataRequiredandwithoutdups <- data.frame(aggregate(list(TotalScoresReqnodups =  dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$NumericalScore,
                                                            TotalReqnodups = dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$Green,
                                                            TotalDeclinedReqnodups = dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$ChangeinCEE_Declined,
                                                            TotalNotReassessedReqnodups =  dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$"ChangeinCEE_Not.re-assessed", 
                                                            TotalGreenBeforeReqnodups =  dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$"ChangeinCEE_No.change/no.need.for.change.from.green",
                                                            CountReqnodups = dplyr::filter(SIMSdatamain1, Required == 1 &  SiteLevelFullScores == 1)$X1), 
                                                       by=list(SNU1.Name = dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$SNU1.Name,
                                                               orgUnituid = dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$orgUnituid), FUN = sum))


SIMSdataTreatmentandwithoutdups <- data.frame(aggregate(list(TotalScoresTrtnodups =  dplyr::filter(SIMSdatamain1, TreatmentCEEs == 1 & SiteLevelFullScores == 1)$NumericalScore,
                                                             TotalTrtnodups = dplyr::filter(SIMSdatamain1,  TreatmentCEEs == 1 & SiteLevelFullScores == 1)$Green,
                                                             CountTrtnodups = dplyr::filter(SIMSdatamain1,  TreatmentCEEs == 1  &  SiteLevelFullScores == 1)$X1),
                                                        by=list(SNU1.Name = dplyr::filter(SIMSdatamain1,  TreatmentCEEs == 1  & SiteLevelFullScores == 1)$SNU1.Name,
                                                                orgUnituid = dplyr::filter(SIMSdatamain1,  TreatmentCEEs == 1  & SiteLevelFullScores == 1)$orgUnituid), FUN = sum))

#Also can add one filtering treatment CEEs only
SIMSdatamainalldata = SIMSdatamain1[!duplicated(SIMSdatamain1$orgUnituid),]

#put all data frames into list
df_list <- list(SIMSdataall, SIMSdatawithoutdups, SIMSdataRequiredandwithoutdups, SIMSdataTreatmentandwithoutdups, SIMSdatamainalldata)
#merge all data frames in list
allsites_ <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

allsites_$SiteLevelScorenodup <- (allsites_$TotalScoresnodups / allsites_$Totalnodups)
allsites_$SiteLevelScorenTrtnodup <- (allsites_$TotalScoresTrtnodups / allsites_$TotalTrtnodups)

exportsites <- subset(allsites_, select = c("OU.Name", "orgUnituid","SNU1.Name",  "DATIM.Location",  "Numberofvisits",  "TotalScoresnodups", "Totalnodups", "TotalDeclinednodups",  "TotalNotReassessednodups",  "TotalGreenBeforenodups",
                                            "Countnodups",  "TotalScoresReqnodups", "Assessment.Date", "SiteLevelScorenodup", "SiteLevelScorenTrtnodup"))
fwrite(exportsites, file=paste("sitedata", ".csv"))


#####Looking at scores at the SNU-level#####

SIMSdatafiltered <-  dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1) #put in your filter here  & Assessment.FY == "2022"


SIMSdataSNUall <- data.frame(aggregate(list(TotalScores =  SIMSdatamain1$NumericalScore,
                                            Total = SIMSdatamain1$Green, Count = SIMSdatamain1$X1), by=list(SNU1.Name = SIMSdatamain1$SNU1.Name), FUN = sum))

SIMSdataSNUwithoutdups <- data.frame(aggregate(list(TotalScoresnodups =  SIMSdatafiltered$NumericalScore,
                                                    Totalnodups = SIMSdatafiltered$Green, Score.for.Greennodups =  SIMSdatafiltered$Score.for.Green,
                                                    Score.for.Rednodups =  SIMSdatafiltered$Score.for.Red,
                                                    Score.for.Red...Yellownodups =  SIMSdatafiltered$Score.for.Red...Yellow,
                                                    Score.for.Yellownodups =  SIMSdatafiltered$Score.for.Yellow,
                                                    TotalDeclinednodups = SIMSdatafiltered$ChangeinCEE_Declined,
                                                    TotalImprovednodups = SIMSdatafiltered$ChangeinCEE_Improved,
                                                    TotalNotChangednodups = SIMSdatafiltered$"ChangeinCEE_No.change",
                                                    TotalNotReassessednodups =  SIMSdatafiltered$"ChangeinCEE_Not.re-assessed", 
                                                    TotalGreenBeforenodups =  SIMSdatafiltered$"ChangeinCEE_No.change/no.need.for.change.from.green", 
                                                    Countnodups = SIMSdatafiltered$X1), by=list(SNU1.Name =  SIMSdatafiltered$SNU1.Name), FUN = sum))


SIMSdataSNURequiredandwithoutdups <- data.frame(aggregate(list(TotalScoresReqnodups =  dplyr::filter(SIMSdatafiltered, Required == 1 )$NumericalScore,
                                                               TotalReqnodups = dplyr::filter(SIMSdatafiltered, Required == 1)$Green,
                                                               Score.for.GreenReqnodups =  dplyr::filter(SIMSdatafiltered, Required == 1)$Score.for.Green,
                                                               Score.for.RedReqnodups =  dplyr::filter(SIMSdatafiltered, Required == 1)$Score.for.Red,
                                                               Score.for.Red...YellowReqnodups =  dplyr::filter(SIMSdatafiltered, Required == 1)$Score.for.Red...Yellow,
                                                               Score.for.YellowReqnodups =  dplyr::filter(SIMSdatafiltered, Required == 1)$Score.for.Yellow,
                                                               TotalDeclinedReqnodups = dplyr::filter(SIMSdatafiltered, Required == 1)$ChangeinCEE_Declined,
                                                               TotalImprovedReqnodups = dplyr::filter(SIMSdatafiltered, Required == 1)$ChangeinCEE_Improved,
                                                               TotalNotChangedReqnodups = dplyr::filter(SIMSdatafiltered, Required == 1)$"ChangeinCEE_No.change",
                                                               TotalNotReassessedReqnodups =  dplyr::filter(SIMSdatafiltered, Required == 1)$"ChangeinCEE_Not.re-assessed", 
                                                               TotalGreenBeforeReqnodups =  dplyr::filter(SIMSdatafiltered, Required == 1)$"ChangeinCEE_No.change/no.need.for.change.from.green", 
                                                               CountReqnodups = dplyr::filter(SIMSdatafiltered, Required == 1)$X1), by=list(SNU1.Name = 
                                                                dplyr::filter(SIMSdatafiltered, Required == 1 )$SNU1.Name), FUN = sum))


SIMSdataSNUTreatmentandwithoutdups <- data.frame(aggregate(list(TotalScoresTrtnodups =  dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1 )$NumericalScore,
                                                                TotalTrtnodups = dplyr::filter(SIMSdatafiltered,  TreatmentCEEs == 1)$Green,
                                                                Score.for.GreenTrtnodups =  dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1)$Score.for.Green,
                                                                Score.for.RedTrtnodups =  dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1)$Score.for.Red,
                                                                Score.for.Red...YellowTrtnodups =  dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1)$Score.for.Red...Yellow,
                                                                Score.for.YellowTrtnodups =  dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1)$Score.for.Yellow,
                                                                TotalDeclinedTrtnodups = dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1)$ChangeinCEE_Declined,
                                                                TotalImprovedTrtnodups = dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1)$ChangeinCEE_Improved,
                                                                TotalNotChangedTrtnodups = dplyr::filter(SIMSdatafiltered, TreatmentCEEs == 1)$"ChangeinCEE_No.change",
                                                                TotalNotReassessedTrtnodups =  dplyr::filter(SIMSdatafiltered,  TreatmentCEEs == 1)$"ChangeinCEE_Not.re-assessed", 
                                                                TotalGreenBeforeTrtnodups =  dplyr::filter(SIMSdatafiltered,  TreatmentCEEs == 1)$"ChangeinCEE_No.change/no.need.for.change.from.green", 
                                                                CountTrtnodups = dplyr::filter(SIMSdatafiltered,  TreatmentCEEs == 1)$X1), by=list(SNU1.Name = 
                                                                dplyr::filter(SIMSdatafiltered,  TreatmentCEEs == 1)$SNU1.Name), FUN = sum))

#Also can add one filtering treatment CEEs only

SIMSdataSNUmainalldata = SIMSdatamain1[!duplicated(SIMSdatamain1$SNU1.Name),]

#put all data frames into list
df_list <- list(SIMSdataSNUall, SIMSdataSNUwithoutdups, SIMSdataSNURequiredandwithoutdups, SIMSdataSNUTreatmentandwithoutdups, SIMSdataSNUmainalldata)
#merge all data frames in list
allsitesSNU_ <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

allsitesSNU_$SiteLevelScorenodup <- (allsitesSNU_$TotalScoresnodups / allsitesSNU_$Totalnodups)
allsitesSNU_$SiteLevelScorenTrtnodup <- (allsitesSNU_$TotalScoresTrtnodups / allsitesSNU_$TotalTrtnodups)

#####Looking at the score level#####


SIMSdatascoreswithoutdups <- data.frame(aggregate(list(Score.for.Green =  SIMSdatafiltered$Score.for.Green,
                                                       Score.for.Red =  SIMSdatafiltered$Score.for.Red,
                                                       Score.for.Red...Yellow =  SIMSdatafiltered$Score.for.Red...Yellow,
                                                       Score.for.Yellow =  SIMSdatafiltered$Score.for.Yellow,
                                                       TotalDeclined = SIMSdatafiltered$ChangeinCEE_Declined,
                                                       TotalImproved = SIMSdatafiltered$ChangeinCEE_Improved,
                                                       TotalNotChanged = SIMSdatafiltered$"ChangeinCEE_No.change",
                                                       TotalNotReassessed =  SIMSdatafiltered$"ChangeinCEE_Not.re-assessed", 
                                                       TotalGreenBefore =  SIMSdatafiltered$"ChangeinCEE_No.change/no.need.for.change.from.green",     
                                                       Countnodups = SIMSdatafiltered$X1), by=list(CEE.Number.and.Name = 
                                                                                                     SIMSdatafiltered$CEE.Number.and.Name, 
                                                                                                   Set.Number = SIMSdatafiltered$Set.Number), FUN = sum))

#####Looking at the score level#####

SIMSdatafiltered <-  dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1) #put in your filter here  & Assessment.FY == "2022"

SIMSdatascoreswithoutdups <- data.frame(aggregate(list(Score.for.Green =  SIMSdatafiltered$Score.for.Green,
                                                       Score.for.Yellow =  SIMSdatafiltered$Score.for.Yellow, Score.for.Red =  SIMSdatafiltered$Score.for.Red,
                                                       Score.for.Red...Yellow =  SIMSdatafiltered$Score.for.Red...Yellow,
                                                        TotalDeclined = SIMSdatafiltered$ChangeinCEE_Declined,
                                                       TotalImproved = SIMSdatafiltered$ChangeinCEE_Improved,
                                                       TotalNotChanged = SIMSdatafiltered$"ChangeinCEE_No.change",
                                                       TotalNotReassessed =  SIMSdatafiltered$"ChangeinCEE_Not.re-assessed", 
                                                       TotalGreenBefore =  SIMSdatafiltered$"ChangeinCEE_No.change/no.need.for.change.from.green",     
                                                       Countnodups = SIMSdatafiltered$X1), by=list(CEE.Number.and.Name = 
                                                                                                     SIMSdatafiltered$CEE.Number.and.Name, 
                                                                                                   Set.Number = SIMSdatafiltered$Set.Number), FUN = sum))

SIMSdatascoreswithoutdups$PercentGreen <- (SIMSdatascoreswithoutdups$Score.for.Green / SIMSdatascoreswithoutdups$Countnodups)
SIMSdatascoreswithoutdups$PercentYellow <- (SIMSdatascoreswithoutdups$Score.for.Yellow / SIMSdatascoreswithoutdups$Countnodups)
SIMSdatascoreswithoutdups$PercentRed <- (SIMSdatascoreswithoutdups$Score.for.Red / SIMSdatascoreswithoutdups$Countnodups)
SIMSdatascoreswithoutdups$PercentRedYellow <- (SIMSdatascoreswithoutdups$Score.for.Red...Yellow / SIMSdatascoreswithoutdups$Countnodups)


SIMSdatascoreswithoutdups$CountReassessed <- SIMSdatascoreswithoutdups$Countnodups - SIMSdatascoreswithoutdups$TotalNotReassessed - 
  SIMSdatascoreswithoutdups$TotalGreenBefore
SIMSdatascoreswithoutdups$PercentDeclined <- SIMSdatascoreswithoutdups$TotalDeclined / SIMSdatascoreswithoutdups$CountReassessed
SIMSdatascoreswithoutdups$PercentImproved <- SIMSdatascoreswithoutdups$TotalImproved / SIMSdatascoreswithoutdups$CountReassessed
SIMSdatascoreswithoutdups$PercentNotChanged <- SIMSdatascoreswithoutdups$TotalNotChanged / SIMSdatascoreswithoutdups$CountReassessed
SIMSdatascoreswithoutdups$PercentNotChangedDeclined <- SIMSdatascoreswithoutdups$PercentNotChanged + SIMSdatascoreswithoutdups$PercentDeclined 

SIMSdatascoreswithoutdupstreatment <- filter(SIMSdatascoreswithoutdups, SIMSdatascoreswithoutdups$Set.Number == "2A" |
                                               SIMSdatascoreswithoutdups$Set.Number == "2B")

SIMSdatascoreswithoutdupsReassessed <- filter(SIMSdatascoreswithoutdups,
                                              SIMSdatascoreswithoutdups$CountReassessed >= 5) ##Filter those who have been reassessed >= x number of times. 

SIMSdatascoreswithoutdupsmin <- filter(SIMSdatascoreswithoutdups,
                                       SIMSdatascoreswithoutdups$Countnodups >= 5) ##Filter those who have been assessed >= x number of times. 

SIMSdatascoreswithoutdupsmin2 <- filter(SIMSdatascoreswithoutdups,
                                       SIMSdatascoreswithoutdups$Countnodups >= 10) ##Filter those who have been assessed >= x number of times. 


View(head( SIMSdatascoreswithoutdupsReassessed[order(SIMSdatascoreswithoutdupsReassessed$declinedquantile, decreasing = TRUE),], n=5))
mostdecline <- head( SIMSdatascoreswithoutdupsReassessed[order(SIMSdatascoreswithoutdupsReassessed$PercentNotChangedDeclined, decreasing = TRUE),], n=10)
fwrite(mostdecline, file=paste("mostdecline", ".csv"))


View(head( SIMSdatascoreswithoutdupstreatment[order(SIMSdatascoreswithoutdupstreatment$PercentRed, decreasing = TRUE),], n=5))

View(head( SIMSdatascoreswithoutdups[order(SIMSdatascoreswithoutdups$PercentRed, decreasing = TRUE),], n=10))
mostegregious <-   head( SIMSdatascoreswithoutdupsmin[(order(SIMSdatascoreswithoutdupsmin$PercentRed,
                 SIMSdatascoreswithoutdupsmin$Countnodups, decreasing = TRUE)),], n=10)

bestscoring <-   head(SIMSdatascoreswithoutdupsmin2[(order(SIMSdatascoreswithoutdupsmin2$PercentGreen, 
                  SIMSdatascoreswithoutdupsmin2$Countnodups, decreasing = TRUE)),], n=10)

mostdecline <-   head( SIMSdatascoreswithoutdupsmin2[(order(SIMSdatascoreswithoutdupsmin2$PercentNotChangedDeclined,
                                                             SIMSdatascoreswithoutdupsmin2$Countnodups, decreasing = TRUE)),], n=10)

fwrite(mostegregious, file=paste("mostegregious", ".csv"))
fwrite(bestscoring, file=paste("bestscoring", ".csv"))
fwrite(mostdecline, file=paste("mostdecline", ".csv"))


mostegregioustograph <- mostegregious %>% select(c("CEE.Number.and.Name", "PercentYellow", "PercentRed", "PercentGreen")) %>% 
  rename(Red = "PercentRed", Yellow = "PercentYellow", Green = "PercentGreen")

bestscoringgraph <- bestscoring %>% select(c("CEE.Number.and.Name", "PercentYellow", "PercentRed", "PercentGreen")) %>% 
  rename(Red = "PercentRed", Yellow = "PercentYellow", Green = "PercentGreen")

mostdeclinegraph <- mostdecline %>% select(c("CEE.Number.and.Name", "PercentYellow", "PercentRed", "PercentGreen")) %>% 
  rename(Red = "PercentRed", Yellow = "PercentYellow", Green = "PercentGreen")

mostegregioustographlong <- pivot_longer(mostegregioustograph, !CEE.Number.and.Name, names_to = "Scores", values_to = "Percent.Breakdown")

bestscoringtographlong <- pivot_longer(bestscoringgraph, !CEE.Number.and.Name, names_to = "Scores", values_to = "Percent.Breakdown")

mostdeclinetographlong <- pivot_longer(mostdeclinegraph, !CEE.Number.and.Name, names_to = "Scores", values_to = "Percent.Breakdown")


#Bottom CEE Scores in the OU

print("The overall bottom 5 CEEs in the OU (with scores which were later re-scored at the same facilities removed, and only keeping CEEs which were assessed by ten or more sites) include:")
paste("1.", mostegregioustograph[1,1], "(scored in", mostegregious[1,12],"sites)")
paste("2.", mostegregioustograph[2,1], "(scored in", mostegregious[2,12],"sites)")
paste("3.", mostegregioustograph[3,1], "(scored in", mostegregious[3,12],"sites)")
paste("4.", mostegregioustograph[4,1], "(scored in", mostegregious[4,12],"sites)")
paste("5.", mostegregioustograph[5,1], "(scored in", mostegregious[5,12],"sites)")

print("Flag for discussion with the OU cluster team:") 
print("What are some of the plans you have for addressing some of these poorer-performing CEEs at the site and above-site level?")

#mostegregioustographlong$Scores <- relevel(mostegregioustographlong$Scores, 'Red')
worst <- mostegregioustographlong %>% ggplot(aes(x=CEE.Number.and.Name, y = Percent.Breakdown, fill=
                                          factor(Scores, levels=c("Green", "Yellow", "Red")))) +
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Worst performing CEE scores")+xlab('CEE Number and Name')+ylab('Score breakdown') +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +  
  scale_fill_manual(values = c("green",  "yellow", "red"))

worst + theme(legend.position = "none")

print(" What are some of the plans you have for addressing some of these poorer-performing CEEs?")
#Top CEE Scores in the OU

print("The overall top 5 CEEs in the OU (with scores which were later re-scored at the same facilities removed, and only keeping CEEs which were assessed by ten or more sites) are (in decreasing order of number of sites in which the CEEs were assessed):")
paste("1.", bestscoringgraph[1,1], "(scored in", bestscoring[1,12],"sites)")
paste("2.", bestscoringgraph[2,1], "(scored in", bestscoring[2,12],"sites)")
paste("3.", bestscoringgraph[3,1], "(scored in", bestscoring[3,12],"sites)")
paste("4.", bestscoringgraph[4,1], "(scored in", bestscoring[4,12],"sites)")
paste("5.", bestscoringgraph[5,1], "(scored in", bestscoring[5,12],"sites)")

best <- bestscoringtographlong %>% ggplot(aes(x=CEE.Number.and.Name, y = Percent.Breakdown, fill=
                                                   factor(Scores, levels=c("Green", "Yellow", "Red")))) +
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Best performing CEE scores")+xlab('CEE Number and Name')+ylab('Score breakdown') +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +  
  scale_fill_manual(values = c("green",  "yellow", "red"))

best + theme(legend.position = "none")

print("Flag for discussion with the OU cluster team: Do you find that the MER indicators in your OU that are linked to these strong-performing CEEs are equally strong?")

#CEEs which have declined or remained the same

#Bottom CEE Scores in the OU
print("The CEEs which have declined or remained the same in the OU (with scores which were later re-scored at the same facilities removed, and only keeping CEEs which were assessed by ten or more sites) include:")
paste("1.", mostdeclinegraph[1,1], "(scored in", mostdecline[1,12],"sites;", mostdecline[1,18]*100,"% declined.")
paste("2.", mostdeclinegraph[2,1], "(scored in", mostdecline[2,12],"sites;", mostdecline[2,18]*100,"% declined.")
paste("3.", mostdeclinegraph[3,1], "(scored in", mostdecline[3,12],"sites;", mostdecline[3,18]*100,"% declined.")
paste("4.", mostdeclinegraph[4,1], "(scored in", mostdecline[4,12],"sites;", mostdecline[4,18]*100,"% declined.")
paste("5.", mostdeclinegraph[5,1], "(scored in", mostdecline[5,12],"sites;", mostdecline[5,18]*100,"% declined.")

decline <- mostdeclinetographlong %>% ggplot(aes(x=CEE.Number.and.Name, y = Percent.Breakdown, fill=
                                                factor(Scores, levels=c("Green", "Yellow", "Red")))) +
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("CEE scores which declined or remained the same")+xlab('CEE Number and Name')+ylab('Score breakdown') +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +  
  scale_fill_manual(values = c("green",  "yellow", "red"))

decline + theme(legend.position = "none")

print("Flag for discussion with the OU cluster team: Some of these CEEs have not been re-assessed or have not shown improvement. Are there ways to support their improvement or to monitor whether or not improvement has occurred?")

#SNU-level scores

SIMSdataSNUwithoutdups$PercentGreen <- (SIMSdataSNUwithoutdups$Score.for.Greennodups / SIMSdataSNUwithoutdups$Countnodups)
SIMSdataSNUwithoutdups$PercentYellow <- (SIMSdataSNUwithoutdups$Score.for.Yellownodups / SIMSdataSNUwithoutdups$Countnodups)
SIMSdataSNUwithoutdups$PercentRed <- (SIMSdataSNUwithoutdups$Score.for.Rednodups / SIMSdataSNUwithoutdups$Countnodups)
SIMSdataSNUwithoutdups$PercentRedYellow <- (SIMSdataSNUwithoutdups$Score.for.Red...Yellownodups / SIMSdataSNUwithoutdups$Countnodups)


SIMSdataSNUwithoutdups$CountReassessed <- SIMSdataSNUwithoutdups$Countnodups - SIMSdataSNUwithoutdups$TotalNotReassessednodups - 
  SIMSdataSNUwithoutdups$TotalGreenBeforenodups
 SIMSdataSNUwithoutdups$PercentDeclined <- SIMSdataSNUwithoutdups$TotalDeclinednodups / SIMSdataSNUwithoutdups$CountReassessed
SIMSdataSNUwithoutdups$PercentImproved <- SIMSdataSNUwithoutdups$TotalImprovednodups / SIMSdataSNUwithoutdups$CountReassessed
SIMSdataSNUwithoutdups$PercentNotChanged <- SIMSdataSNUwithoutdups$TotalNotChangednodups / SIMSdataSNUwithoutdups$CountReassessed
SIMSdataSNUwithoutdups$PercentNotChangedDeclined <- SIMSdataSNUwithoutdups$PercentNotChanged + SIMSdataSNUwithoutdups$PercentDeclined

SIMSdataSNUwithoutdupsmin <- filter(SIMSdataSNUwithoutdups,
                                    SIMSdataSNUwithoutdups$Countnodups >= 5) ##Filter those who have been assessed >= x number of times. 

SIMSdataSNUwithoutdupsmin2 <- filter(SIMSdataSNUwithoutdups,
                                     SIMSdataSNUwithoutdups$Countnodups >= 10) ##Filter those who have been assessed >= x number of times. 

bestscoringSNU <-   head(SIMSdataSNUwithoutdups[(order(SIMSdataSNUwithoutdups$PercentGreen, 
                                                           SIMSdataSNUwithoutdups$Countnodups, decreasing = TRUE)),], n=8)
worstscoringSNU <-   head(SIMSdataSNUwithoutdups[(order(SIMSdataSNUwithoutdups$PercentRed, 
                                                           SIMSdataSNUwithoutdups$Countnodups, decreasing = TRUE)),], n=8)

bestscoringSNUgraph <- bestscoringSNU %>% select(c("SNU1.Name", "PercentYellow", "PercentRed", "PercentGreen")) %>% 
  rename(Red = "PercentRed", Yellow = "PercentYellow", Green = "PercentGreen")

worstscoringSNUgraph <- worstscoringSNU %>% select(c("SNU1.Name", "PercentYellow", "PercentRed", "PercentGreen")) %>% 
  rename(Red = "PercentRed", Yellow = "PercentYellow", Green = "PercentGreen")

bestscoringSNUtographlong <- pivot_longer(bestscoringSNUgraph, !SNU1.Name, names_to = "Scores", values_to = "Percent.Breakdown")

worstscoringSNUtographlong <- pivot_longer(worstscoringSNUgraph, !SNU1.Name, names_to = "Scores", values_to = "Percent.Breakdown")


print("Regions with large shares of green CEEs indicate that those regions are doing well and perhaps could be models of best practice for other sites.  The overall top 5 SNUs in the OU (with scores which were later re-scored at the same facilities removed) are (in decreasing order of number of CEEs assessed in the SNU):")
paste("1.", bestscoringSNUgraph[1,1], "(", bestscoringSNU[1,13],"CEEs scored)")
paste("2.", bestscoringSNUgraph[2,1], "(", bestscoringSNU[2,13],"CEEs scored)")
paste("3.", bestscoringSNUgraph[3,1], "(", bestscoringSNU[3,13],"CEEs scored)")
paste("4.", bestscoringSNUgraph[4,1], "(", bestscoringSNU[4,13],"CEEs scored)")
paste("5.", bestscoringSNUgraph[5,1], "(", bestscoringSNU[5,13],"CEEs scored)")

bestSNU <- bestscoringSNUtographlong %>% ggplot(aes(x=SNU1.Name, y = Percent.Breakdown, fill=
                                                factor(Scores, levels=c("Green", "Yellow", "Red")))) +
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Best-performing SNUs")+xlab('SNUs')+ylab('Score breakdown') +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +  
  scale_fill_manual(values = c("green",  "yellow", "red"))

bestSNU + theme(legend.position = "none")

print("Regions with larger shares of red CEEs indicate that those regions may have more sites which need improvement and/or re-assessment.  The overall bottom 5 SNUs in the OU (with scores which were later re-scored at the same facilities removed) are (in decreasing order of number of CEEs assessed in the SNU):")
paste("1.", worstscoringSNUgraph[1,1], "(", worstscoringSNU[1,13],"CEEs scored)")
paste("2.", worstscoringSNUgraph[2,1], "(", worstscoringSNU[2,13],"CEEs scored)")
paste("3.", worstscoringSNUgraph[3,1], "(", worstscoringSNU[3,13],"CEEs scored)")
paste("4.", worstscoringSNUgraph[4,1], "(", worstscoringSNU[4,13],"CEEs scored)")
paste("5.", worstscoringSNUgraph[5,1], "(", worstscoringSNU[5,13],"CEEs scored)")

worstSNU <- worstscoringSNUtographlong %>% ggplot(aes(x=SNU1.Name, y = Percent.Breakdown, fill=
                                                      factor(Scores, levels=c("Green", "Yellow", "Red")))) +
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Worst-performing SNUs")+xlab('SNUs')+ylab('Score breakdown') +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +  
  scale_fill_manual(values = c("green",  "yellow", "red"))

worstSNU + theme(legend.position = "none")

paste("Flag for discussion with the country cluster team:  Are there success stories that an SNU like", bestscoringSNUgraph[1,1],
      "(which has collected a higher number of scores and which has a large proportion of green scores assessed) can share with lower-performing SNUs such as",worstscoringSNUgraph[1,1],"?")

#Partner-level scores


#####Looking at scores at the Partner-level#####

SIMSdatapartnerwithoutdups <- data.frame(aggregate(list(TotalScoresnodups =  SIMSdatafiltered$NumericalScore,
                                                    Totalnodups = SIMSdatafiltered$Green, Score.for.Greennodups =  SIMSdatafiltered$Score.for.Green,
                                                    Score.for.Rednodups =  SIMSdatafiltered$Score.for.Red,
                                                    Score.for.Red...Yellownodups =  SIMSdatafiltered$Score.for.Red...Yellow,
                                                    Score.for.Yellownodups =  SIMSdatafiltered$Score.for.Yellow,
                                                    TotalDeclinednodups = SIMSdatafiltered$ChangeinCEE_Declined,
                                                    TotalImprovednodups = SIMSdatafiltered$ChangeinCEE_Improved,
                                                    TotalNotChangednodups = SIMSdatafiltered$"ChangeinCEE_No.change",
                                                    TotalNotReassessednodups =  SIMSdatafiltered$"ChangeinCEE_Not.re-assessed", 
                                                    TotalGreenBeforenodups =  SIMSdatafiltered$"ChangeinCEE_No.change/no.need.for.change.from.green", 
                                                    Countnodups = SIMSdatafiltered$X1), by=list(PrimePartner =  SIMSdatafiltered$Prime.Partner), FUN = sum))

SIMSdatapartnerwithoutdups$PercentGreen <- (SIMSdatapartnerwithoutdups$Score.for.Greennodups / SIMSdatapartnerwithoutdups$Countnodups)
SIMSdatapartnerwithoutdups$PercentYellow <- (SIMSdatapartnerwithoutdups$Score.for.Yellownodups / SIMSdatapartnerwithoutdups$Countnodups)
SIMSdatapartnerwithoutdups$PercentRed <- (SIMSdatapartnerwithoutdups$Score.for.Rednodups / SIMSdatapartnerwithoutdups$Countnodups)
SIMSdatapartnerwithoutdups$PercentRedYellow <- (SIMSdatapartnerwithoutdups$Score.for.Red...Yellownodups / SIMSdatapartnerwithoutdups$Countnodups)

SIMSdatapartnerwithoutdups$CountReassessed <- SIMSdatapartnerwithoutdups$Countnodups - SIMSdatapartnerwithoutdups$TotalNotReassessednodups - 
  SIMSdatapartnerwithoutdups$TotalGreenBeforenodups
SIMSdatapartnerwithoutdups$PercentDeclined <- SIMSdatapartnerwithoutdups$TotalDeclinednodups / SIMSdatapartnerwithoutdups$CountReassessed
SIMSdatapartnerwithoutdups$PercentImproved <- SIMSdatapartnerwithoutdups$TotalImprovednodups / SIMSdatapartnerwithoutdups$CountReassessed
SIMSdatapartnerwithoutdups$PercentNotChanged <- SIMSdatapartnerwithoutdups$TotalNotChangednodups / SIMSdatapartnerwithoutdups$CountReassessed
SIMSdatapartnerwithoutdups$PercentNotChangedDeclined <- SIMSdatapartnerwithoutdups$PercentNotChanged + SIMSdatapartnerwithoutdups$PercentDeclined

SIMSdatapartnerwithoutdupsmin <- filter(SIMSdatapartnerwithoutdups,
                                        SIMSdatapartnerwithoutdups$Countnodups >= 5) ##Filter those who have been assessed >= x number of times. 

SIMSdatapartnerwithoutdupsmin2 <- filter(SIMSdatapartnerwithoutdups,
                                         SIMSdatapartnerwithoutdups$Countnodups >= 10) ##Filter those who have been assessed >= x number of times. 

bestscoringpartner <-   head(SIMSdatapartnerwithoutdups[(order(SIMSdatapartnerwithoutdups$PercentGreen, 
                                                                  SIMSdatapartnerwithoutdups$Countnodups, decreasing = TRUE)),], n=3)
worstscoringpartner <-   head(SIMSdatapartnerwithoutdups[(order(SIMSdatapartnerwithoutdups$PercentRed, 
                                                                   SIMSdatapartnerwithoutdups$Countnodups, decreasing = TRUE)),], n=3)

bestscoringpartnergraph <- bestscoringpartner %>% select(c("PrimePartner", "PercentYellow", "PercentRed", "PercentGreen")) %>% 
  rename(Red = "PercentRed", Yellow = "PercentYellow", Green = "PercentGreen")

worstscoringpartnergraph <- worstscoringpartner %>% select(c("PrimePartner", "PercentYellow", "PercentRed", "PercentGreen")) %>% 
  rename(Red = "PercentRed", Yellow = "PercentYellow", Green = "PercentGreen")

bestscoringpartnerographlong <- pivot_longer(bestscoringpartnergraph, !PrimePartner, names_to = "Scores", values_to = "Percent.Breakdown")

worstscoringpartnertographlong <- pivot_longer(worstscoringpartnergraph, !PrimePartner, names_to = "Scores", values_to = "Percent.Breakdown")


print("Partners with large shares of green CEEs indicate that those partners are doing well and perhaps could be models of best practice for other partners.  The overall top 3 partners in the OU (with scores which were later re-scored at the same facilities removed) are (in decreasing order of number of CEEs assessed in the partner):")
paste("1.", bestscoringpartnergraph[1,1], "(", bestscoringpartner[1,13],"CEEs scored)")
paste("2.", bestscoringpartnergraph[2,1], "(", bestscoringpartner[2,13],"CEEs scored)")
paste("3.", bestscoringpartnergraph[3,1], "(", bestscoringpartner[3,13],"CEEs scored)")
#paste("4.", bestscoringpartnergraph[4,1], "(", bestscoringpartner[4,13],"CEEs scored)")
#paste("5.", bestscoringpartnergraph[5,1], "(", bestscoringpartner[5,13],"CEEs scored)")

bestpartner <- bestscoringpartnerographlong %>% ggplot(aes(x=PrimePartner, y = Percent.Breakdown, fill=
                                                      factor(Scores, levels=c("Green", "Yellow", "Red")))) +
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Best-performing partners")+xlab('Partners')+ylab('Score breakdown') +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +  
  scale_fill_manual(values = c("green",  "yellow", "red"))

bestpartner + theme(legend.position = "none")

print("Partners with larger shares of red CEEs indicate that those regions may have more sites which need improvement and/or re-assessment.  The overall bottom 3 partners in the OU (with scores which were later re-scored at the same facilities removed) are (in decreasing order of number of CEEs assessed in the partner):")
paste("1.", worstscoringpartnergraph[1,1], "(", worstscoringpartner[1,13],"CEEs scored)")
paste("2.", worstscoringpartnergraph[2,1], "(", worstscoringpartner[2,13],"CEEs scored)")
paste("3.", worstscoringpartnergraph[3,1], "(", worstscoringpartner[3,13],"CEEs scored)")
#paste("4.", worstscoringSNUgraph[4,1], "(", worstscoringSNU[4,13],"CEEs scored)")
#paste("5.", worstscoringSNUgraph[5,1], "(", worstscoringSNU[5,13],"CEEs scored)")

worstpartner <- worstscoringpartnertographlong %>% ggplot(aes(x=PrimePartner, y = Percent.Breakdown, fill=
                                                        factor(Scores, levels=c("Green", "Yellow", "Red")))) +
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Worst-performing Partners")+xlab('SNUs')+ylab('Score breakdown') +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +  
  scale_fill_manual(values = c("green",  "yellow", "red"))

worstpartner + theme(legend.position = "none")

paste("Flag for discussion with the country cluster team:  Are there success stories that a partner like", bestscoringpartnergraph[1,1],
      "(which has a large proportion of green scores assessed) can share with lower-performing partners such as",worstscoringpartnergraph[1,1],"?")




# ggplot(mostegregioustographlong, 
#        aes(x = CEE.Number.and.Name,  fill = Scores)) + 
#   geom_bar(position = "fill") +
#   labs(y = "Proportion")
# 
# 
# mostegregioustographlong %>%  
#   ggplot(aes(CEE.Number.and.Name))+geom_bar(aes(fill=Scores), position="fill")
#     x = Scores, y = Percent.Breakdown, fill = CEE.Number.and.Name)) + 
#   geom_col(width = .25) + 
#   scale_fill_manual(values = c("black", "#039dfc", "yellow")) + 
#   coord_flip() 

#barplot(mostegregioustograph,  ylab="CEE scores")