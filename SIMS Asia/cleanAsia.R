library(here)
setwd(here())
library(readxl)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(fastDummies)


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
##for asia, may need to fix the 

SIMSdatain <- temp_df
colnames(SIMSdatain)[grepl("DATIM.Location.UID", colnames(SIMSdatain))] <- "orgUnituid" #"LOWEST_OU_UID"  --> question-level data uses _ instead of .
SIMSdatain$X1 <- 1
SIMSdatain$SNU1.Name[SIMSdatain$SNU1.Name == "Tajikistan (ZtoVYbNCnsj)"]<- "Tajikistan"

Genie <-read_xlsx("Genie_SIMS_Site_IM_Asia_Region_d195057b-7735-4141-b677-907ec66ca13e.xlsx", sheet = "org") #Datim -> Genie ->

SIMSdata <- merge(x=SIMSdatain, y=Genie, by=c("orgUnituid"), all.x = TRUE) 


#SIMSdata$Date <- substr(SIMSdata$ASSESSMENT_DATE, 1, 9)
SIMSdata$DateAssessment <- as.Date(SIMSdata$Assessment.Date, format= "%m/%d/20%y")
#SIMSdata$ASSESSMENT_YEAR <- substr(SIMSdata$ASSESSMENT_QUARTER, 1, 4)
SIMSdata$AssessmentFY <- substr(SIMSdata$Assessment.Quarter, 1, 4)

#SIMSdata <- SIMSdata[order(DateAssessment)]

##This was the first date these sites were visited. 
SIMSdata_visits  <- SIMSdata %>% select("SNU1.Name", "orgUnituid", "ï..Assessment.ID", "DateAssessment") %>% distinct
SIMSdata_visits <- SIMSdata_visits %>%
  group_by(SNU1.Name, orgUnituid) %>%
  mutate(Ordervisit = order(order(ï..Assessment.ID, decreasing=FALSE))) %>%
  mutate(Ordervisitlast = order(order(ï..Assessment.ID, decreasing=TRUE)))

SIMSdata_visits = subset(SIMSdata_visits, select = -c(DateAssessment))
SIMSdata_1 <- merge(x=SIMSdata, y=SIMSdata_visits, by=c("SNU1.Name", "orgUnituid", "ï..Assessment.ID"), all.x=TRUE)

##This was the first date these sites were visited. 
SIMSdata_1$AssessmentStatus[SIMSdata_1$Ordervisit == 1] <- "Initial Assessment" 
# SIMSdata_first <- setDT(SIMSdata)[order(DateAssessment), head(.SD, 1L), by = "orgUnituid"]
# SIMSdata_first <- select(SIMSdata_first, c('orgUnituid', 'DateAssessment'))
# SIMSdata_first$AssessmentStatus <- "Initial Assessment" 
# 
# SIMSdata2 <- merge(x=SIMSdata, y=SIMSdata_first, by=c("orgUnituid", "DateAssessment"), all.x=TRUE)
# 
# ##Now, make an indicator for the LAST visit. (this might be the same as the first visit)
SIMSdata_1$AssessmentStatus1[SIMSdata_1$Ordervisitlast == 1] <- "Last Assessment" 
# SIMSdata_last <-unique(SIMSdata[order(DateAssessment)], by= "orgUnituid", fromLast=TRUE)
# SIMSdata_last <- select(SIMSdata_last, c( "orgUnituid", 'DateAssessment'))
# SIMSdata_last$AssessmentStatus1 <- "Last Assessment" ##This was the last date these sites were visited.
# 
# SIMSdata_1 <- merge(x=SIMSdata2, y=SIMSdata_last, by=c("orgUnituid", "DateAssessment"), all.x=TRUE)

SIMSdata_1$Assessment.ID <- SIMSdata_1$"ï..Assessment.ID"

library(dplyr)
 SIMSdatanumbervisits <-  SIMSdata_1 %>% filter(!is.na(Assessment.ID)) %>% 
  #dplyr::distinct(Assessment.ID) %>%
  group_by(orgUnituid) %>% 
  #summarise(n()) %>%
  summarise(Numberofvisits = n_distinct(Assessment.ID)) ##this is not working

#SIMSdatanumbervisits <-  SIMSdatanumbervisits  %>% rename(NumberofVisits =  "n_distinct(DateAssessment)", orgUnituid = "\"orgUnituid\"")

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

# cols.num <- c( "Initial assessment", "Follow-up (and last) assessment", "Follow-up assessment")
# ####COME BACK TO THIS!
# # RSSIMSdatafilteredtodups[cols.num][is.na(RSSIMSdatafilteredtodups[cols.num]==TRUE)] <- "0"
# RSSIMSdatafilteredtodups$`Initial assessment` = unlist(RSSIMSdatafilteredtodups$`Initial assessment`,  use.names = TRUE)
# RSSIMSdatafilteredtodups[cols.num] <- sapply(unlist(RSSIMSdatafilteredtodups[cols.num]))
#  RSSIMSdatafilteredtodups2[cols.num] <- sapply(RSSIMSdatafilteredtodups[cols.num],as.numeric)
# 
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


#SIMSdatamain <- SIMSdata1 %>%  dplyr::filter(duplicated(SIMSdata1[,c("ASSESSMENT_DATE", "LOWEST_OU_UID", "CEE_LONG_ID")]) == ("FALSE")) #This is to get a single score from that date (use with the sco-re-level dataset)


#SIMSdatamain1 <- SIMSdatamain1 %>% group_by("orgUnituid", "CEE.Number") %>% mutate(duplicate=seq(n()))
#SIMSdatamain1$RepeatedCEEs <- duplicated(SIMSdatamain1[,c('orgUnituid', 'CEE.Number')])

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
## this status could help us eventually determine Which CEEs can be "deleted" since they have been re-scored in follow-ups. 



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
                                                                      AssessmentID = SIMSdatamain1$"ï..Assessment.ID"), FUN = sum)) ###aggregate by assessment ID, site


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


dodecile = quantile(allsites_$SiteLevelScorenodup, c(0:12/12))
allsites_$Sitebottododecile <- with(allsites_,
                                      cut(SiteLevelScorenodup, dodecile,
                                          include.lowest = T,
                                          labels = c("Lowest Score", "16.6%", "25%", "33.3%", "41.6%", "50%", "58.3%", "66.6%", 
                                                     "75%", "83.3%", "91.6%", "Highest score")))

dodecile = quantile(allsites_$SiteLevelScorenTrtnodup, c(0:12/12),  na.rm = TRUE)
allsites_$SiteTrtbottododecile <- with(allsites_,
                                         cut(SiteLevelScorenTrtnodup, dodecile,
                                             include.lowest = T,
                                             labels = c("Lowest Score", "16.6%", "25%", "33.3%", "41.6%", "50%", "58.3%", "66.6%", 
                                                        "75%", "83.3%", "91.6%", "Highest scores")))


bottomTrt<- filter(allsites_, SiteTrtbottododecile == "Lowest Score")  %>% select(c(1,13,16,17,19,20,89,94,133,144,150,154,166)) ##, 18,77,82,132,133,135,154,168))  ## %>% dplyr::rename("County" = County_Description) %>% 
##  dplyr::rename("State" = PART_STATE) %>% 
##  dplyr::rename("Average Estimated Salary" = EstimatedSalary)%>% 
##  dplyr::rename("Index of participant health (between 0 and 1)" = normalized)

#Service.Delivery.Type SNU1.Name Datim.Location Partner.Type Prime.Partner SNUprioritization #4,7,8,


#####Looking at scores at the SNU-level#####

SIMSdataSNUall <- data.frame(aggregate(list(TotalScores =  SIMSdatamain1$NumericalScore,
                                            Total = SIMSdatamain1$Green, Count = SIMSdatamain1$X1), by=list(SNU1.Name = SIMSdatamain1$SNU1.Name), FUN = sum))

SIMSdataSNUwithoutdups <- data.frame(aggregate(list(TotalScoresnodups =  dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1)$NumericalScore,
                                                    Totalnodups = dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1)$Green, 
                                                    Countnodups = dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1)$X1), by=list(SNU1.Name = 
                                                                                                                                        dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1)$SNU1.Name), FUN = sum))


SIMSdataSNURequiredandwithoutdups <- data.frame(aggregate(list(TotalScoresReqnodups =  dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$NumericalScore,
                                                               TotalReqnodups = dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$Green,
                                                               CountReqnodups = dplyr::filter(SIMSdatamain1, Required == 1 &  SiteLevelFullScores == 1)$X1), by=list(SNU1.Name = 
                                                                                                                                                                       dplyr::filter(SIMSdatamain1, Required == 1 & SiteLevelFullScores == 1)$SNU1.Name), FUN = sum))


SIMSdataSNUTreatmentandwithoutdups <- data.frame(aggregate(list(TotalScoresTrtnodups =  dplyr::filter(SIMSdatamain1, TreatmentCEEs == 1 & SiteLevelFullScores == 1)$NumericalScore,
                                                                TotalTrtnodups = dplyr::filter(SIMSdatamain1,  TreatmentCEEs == 1 & SiteLevelFullScores == 1)$Green,
                                                                CountTrtnodups = dplyr::filter(SIMSdatamain1,  TreatmentCEEs == 1  &  SiteLevelFullScores == 1)$X1), by=list(SNU1.Name = 
                                                                                                                                                                               dplyr::filter(SIMSdatamain1,  TreatmentCEEs == 1  & SiteLevelFullScores == 1)$SNU1.Name), FUN = sum))

#Also can add one filtering treatment CEEs only

SIMSdataSNUmainalldata = SIMSdatamain1[!duplicated(SIMSdatamain1$SNU1.Name),]

#put all data frames into list
df_list <- list(SIMSdataSNUall, SIMSdataSNUwithoutdups, SIMSdataSNURequiredandwithoutdups, SIMSdataSNUTreatmentandwithoutdups, SIMSdataSNUmainalldata)
#merge all data frames in list
allsitesSNU_ <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

allsitesSNU_$SiteLevelScorenodup <- (allsitesSNU_$TotalScoresnodups / allsitesSNU_$Totalnodups)
allsitesSNU_$SiteLevelScorenTrtnodup <- (allsitesSNU_$TotalScoresTrtnodups / allsitesSNU_$TotalTrtnodups)

dodecile = quantile(allsitesSNU_$SiteLevelScorenodup, c(0:4/4))
allsitesSNU_$Sitebottododecile <- with(allsitesSNU_,
                                         cut(SiteLevelScorenodup, dodecile,
                                             include.lowest = T,
                                             labels = c("Lowest Score", "50%", "75%", "Highest score")))

dodecile = quantile(allsitesSNU_$SiteLevelScorenTrtnodup, c(0:5/5),  na.rm = TRUE)
allsitesSNU_$SiteTrtbottododecile <- with(allsitesSNU_,
                                            cut(SiteLevelScorenTrtnodup, dodecile,
                                                include.lowest = T,
                                                labels = c("Lowest Score", "40%", "60%",
                                                           "80%", "Highest score")))

bottomSNU<- filter(allsitesSNU_, Sitebottododecile == "Lowest Score")  %>% select(c(1,13,16,17,19,20,89,94,133,144,150,154,166)) ##, 18,77,82,132,133,135,154,168))  ## %>% dplyr::rename("County" = County_Description) %>% 

bottomTrtSNU<- filter(allsitesSNU_, SiteTrtbottododecile == "Lowest Score")  %>% select(c(1,13,16,17,19,20,89,94,133,144,150,154,166)) ##, 18,77,82,132,133,135,154,168))  ## %>% dplyr::rename("County" = County_Description) %>% 


#####Looking at the score level#####

SIMSdatafiltered <-  dplyr::filter(SIMSdatamain1, SiteLevelFullScores == 1) #put in your filter here  & Assessment.FY == "2022"

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

SIMSdatascoreswithoutdups$PercentRed <- (SIMSdatascoreswithoutdups$Score.for.Red / SIMSdatascoreswithoutdups$Countnodups)
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
                                       SIMSdatascoreswithoutdups$Countnodups >= 5) ##Filter those who have been reassessed >= x number of times. 


quantile = quantile(SIMSdatascoreswithoutdupsReassessed$PercentNotChangedDeclined, c(0:5/5))
SIMSdatascoreswithoutdupsReassessed$declinedquantile <- with(SIMSdatascoreswithoutdupsReassessed,
                                                             cut(PercentNotChangedDeclined, quantile,
                                                                 include.lowest = T,
                                                                 labels = c("Lowest decline/no change", "Low decline/no change", 
                                                                            "Medium decline/no change", 
                                                                            "High decline/no changed",   "Highest decline/no changed")))

View(head( SIMSdatascoreswithoutdupsReassessed[order(SIMSdatascoreswithoutdupsReassessed$declinedquantile, decreasing = TRUE),], n=5))
mostdecline <- head( SIMSdatascoreswithoutdupsReassessed[order(SIMSdatascoreswithoutdupsReassessed$PercentNotChangedDeclined, decreasing = TRUE),], n=10)
fwrite(mostdecline, file=paste("mostdecline", ".csv"))

mostimproved <- head( SIMSdatascoreswithoutdupsReassessed[order(SIMSdatascoreswithoutdupsReassessed$PercentImproved, decreasing = TRUE),], n=10)
fwrite(mostimproved, file=paste("mostimproved", ".csv"))


quantile = quantile(SIMSdatascoreswithoutdupstreatment$PercentRed, c(0:2/2))
SIMSdatascoreswithoutdupstreatment$SIMSdatascoresquantile <- with(SIMSdatascoreswithoutdupstreatment,
                                                                  cut(PercentRed, quantile,
                                                                      include.lowest = T,
                                                                      labels = c("Most green",   "Most red")))

SIMSdatascoreswithoutdupsnoNA <- filter(SIMSdatascoreswithoutdups, is.na(PercentRed) == FALSE)
quantile2 = quantile(SIMSdatascoreswithoutdupsnoNA$PercentRed, c(0:2/2))
SIMSdatascoreswithoutdups$SIMSdatascoresquantile <- with(SIMSdatascoreswithoutdupsnoNA,
                                                         cut(PercentRed, quantile2,
                                                             include.lowest = T,
                                                             labels = c("Most green",   "Most red")))

View(head( SIMSdatascoreswithoutdupstreatment[order(SIMSdatascoreswithoutdupstreatment$PercentRed, decreasing = TRUE),], n=5))

View(head( SIMSdatascoreswithoutdups[order(SIMSdatascoreswithoutdups$PercentRed, decreasing = TRUE),], n=10))
mostegregious <-   head( SIMSdatascoreswithoutdups[order(SIMSdatascoreswithoutdups$PercentRed, decreasing = TRUE),], n=10)

fwrite(mostegregious, file=paste("mostegregious", ".csv"))

