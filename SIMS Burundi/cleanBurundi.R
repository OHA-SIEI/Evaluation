library(here)
setwd(here())
library(readxl)
library(dplyr)
library(lubridate)
library(data.table)

file <- 'Download Your Data_Full Data_data-Burundi8-31-22.csv' #downloaded from 
  #'sims_asmt_burundi_question_data_v2.csv' #Question-level data provided by Isaiah L. 

Bur_SIMSin <- read.csv(file = file)

colnames(Bur_SIMSin)[grepl("DATIM.Location.UID", colnames(Bur_SIMSin))] <- "orgUnituid" #"LOWEST_OU_UID"  --> question-level data uses _ instead of .
Bur_SIMSin$X1 <- 1

Genie <-read_xlsx("Genie_SIMS_Site_IM_Burundi_4ad00932-4bce-4ccf-9030-d41d6aaaa8bf.xlsx", sheet = "org")

Bur_SIMS <- merge(x=Bur_SIMSin, y=Genie, by=c("orgUnituid"), all.x = TRUE) 

#Bur_SIMS$Date <- substr(Bur_SIMS$ASSESSMENT_DATE, 1, 9)
Bur_SIMS$DateAssessment <- as.Date(Bur_SIMS$Assessment.Date, format= "%m/%d/20%y")
#Bur_SIMS$ASSESSMENT_YEAR <- substr(Bur_SIMS$ASSESSMENT_QUARTER, 1, 4)
Bur_SIMS$AssessmentFY <- substr(Bur_SIMS$Assessment.Quarter, 1, 4)

##This was the first date these sites were visited. 
Bur_SIMS_first <- setDT(Bur_SIMS)[order(DateAssessment), head(.SD, 1L), by = "orgUnituid"]
Bur_SIMS_first <- select(Bur_SIMS_first, c('orgUnituid', 'DateAssessment'))
Bur_SIMS_first$AssessmentStatus <- "Initial Assessment" 

Bur_SIMS2 <- merge(x=Bur_SIMS, y=Bur_SIMS_first, by=c("orgUnituid", "DateAssessment"), all.x=TRUE)

##Now, make an indicator for the LAST visit. (this might be the same as the first visit)
Bur_SIMS_last <-unique(Bur_SIMS[order(DateAssessment)], by= "orgUnituid", fromLast=TRUE)
Bur_SIMS_last <- select(Bur_SIMS_last, c( "orgUnituid", 'DateAssessment'))
Bur_SIMS_last$AssessmentStatus1 <- "Last Assessment" ##This was the last date these sites were visited. 

Bur_SIMS_1 <- merge(x=Bur_SIMS2, y=Bur_SIMS_last, by=c("orgUnituid", "DateAssessment"), all.x=TRUE)

##number of assessments 
 Bur_SIMSnumbervisits <- Bur_SIMS_1 %>%
   group_by(orgUnituid) %>%
  summarise(Numberofvisits = n_distinct(DateAssessment))
 
# Bur_SIMSnumbervisits <-  Bur_SIMSnumbervisits  %>% rename(NumberofVisits =  "n_distinct(DateAssessment)", orgUnituid = "\"orgUnituid\"")
 
Bur_SIMS1 <- merge(x=Bur_SIMS_1, y=Bur_SIMSnumbervisits, by=c("orgUnituid"), all.x=TRUE)

Bur_SIMS1$AssessmentStatus[is.na(Bur_SIMS1$AssessmentStatus) == TRUE & Bur_SIMS1$AssessmentStatus1 == "Last Assessment"] <- "Follow-up/last assessment"
Bur_SIMS1$AssessmentStatus[is.na(Bur_SIMS1$AssessmentStatus) == TRUE & is.na(Bur_SIMS1$AssessmentStatus1) == TRUE] <- "Follow-up assessment"

Bur_SIMS1$AssessmentStatus[is.na(Bur_SIMS1$AssessmentStatus) == TRUE] <- "Follow-up assessment"
Bur_SIMS1$AssessmentStatus[Bur_SIMS1$AssessmentStatus == "Initial Assessment" & 
                             (Bur_SIMS1$SCORE == "Red" | Bur_SIMS1$SCORE == "Yellow")] <- "Initial/Need to return"
Bur_SIMS1$AssessmentStatus[Bur_SIMS1$AssessmentStatus == "Follow-up assessment" & 
                             (Bur_SIMS1$SCORE == "Red" | Bur_SIMS1$SCORE == "Yellow")] <- "Follow-up/Need to return"
Bur_SIMS1$AssessmentStatus[Bur_SIMS1$AssessmentStatus == "Follow-up/last assessment" & 
                             (Bur_SIMS1$SCORE == "Red" | Bur_SIMS1$SCORE == "Yellow")] <- "Last assessment/Need to return"

Bur_SIMSmain1$RepeatedCEEs <- duplicated(Bur_SIMSmain1[,c('orgUnituid', 'CEE.Number')])

#Bur_SIMSmain <- Bur_SIMS1 %>%  dplyr::filter(duplicated(Bur_SIMS1[,c("ASSESSMENT_DATE", "LOWEST_OU_UID", "CEE_LONG_ID")]) == ("FALSE")) #This is to get a single score from that date (use with the sco-re-level dataset)

Bur_SIMSmain <- Bur_SIMS1
####Number of scores per site and date
Bur_SIMSmain$n <- 1

Bur_SIMSmainnumberscores <-  aggregate(Bur_SIMSmain$n, by = list(Bur_SIMSmain$"orgUnituid", Bur_SIMSmain$DateAssessment), FUN = sum)

Bur_SIMSmainnumberscores <- Bur_SIMSmainnumberscores %>% rename("orgUnituid" = Group.1,
                                                                DateAssessment = Group.2, 
                                                                numberofscores = x)

Bur_SIMSmain1 <- merge(x=Bur_SIMSmain, y=Bur_SIMSmainnumberscores, by=c("orgUnituid", "DateAssessment"), all.x=TRUE)

Bur_SIMSmain1$MorethanoneCEEassessment[Bur_SIMSmain1$NumberofVisits >= 2] <- 1  

Bur_SIMSmain1 <- Bur_SIMSmain1 %>% mutate(AssessmentWasRechecked = ifelse(((Numberofvisits == 2 & 
                                  AssessmentStatus == "Initial/Need to return") | (Numberofvisits >= 3 & 
                                    AssessmentStatus == "Follow-up/Need to return")), 1, 0)) #if NumberofVisits == 2 and AssessmentStatus == "Initial/Need to return" then we can technically "delete" this since these should have been checked again in the second visit  # if NumberofVisits >= 3 and AssessmentStatus == "Follow-up/Need to return" then we can technically "delete" this since these should have been checked again in the third visit

Bur_SIMSmain1 <- Bur_SIMSmain1 %>% mutate(FinalAssessmentStatus = case_when(AssessmentStatus == "Initial Assessment" &
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

#Bur_SIMSmain1 <- Bur_SIMSmain1 %>% group_by("orgUnituid", "CEE.Number") %>% mutate(duplicate=seq(n()))
#Bur_SIMSmain1$RepeatedCEEs <- duplicated(Bur_SIMSmain1[,c('orgUnituid', 'CEE.Number')])
Bur_SIMSmain1 <- Bur_SIMSmain1 %>%   group_by(orgUnituid, CEE.Number) %>%   mutate(RepeatedCEEs = n()) #>1 To provide T/F. 2/3 are duplicated

#ID required versus non-required
conditionsite <- Bur_SIMSmain1$CEE.Number %in% c("1.02", "1.07", "1.08", "1.1", "1.11", "1.21", "1.22", "2.01", "2.02", "2.03", "2.05", "2.07", "2.08", "2.1", "2.18", "2.19", "2.22", 
                                                  "2.23", "2.27", "3.03", "3.05", "3.08", "3.09", "3.1", "3.12", "3.14", "3.15", "3.17", "4.01", "4.02", "4.04", "4.07", "4.08", "4.1", "4.13",
                                                  "4.14", "4.19", "4.2", "5.02", "6.02", "6.04", "6.06", "6.08", "7.01", "7.03", "8.02", "9.01", "9.03", "9.04", "10.01", "10.08",
                                                  "10.09") #required for sites 
conditionabovesite <- Bur_SIMSmain1$Set.Number %in% c("5", "6", "8", "9") #required for above-site

conditiontreatment <- Bur_SIMSmain1$Set.Number %in% c("2A", "2B") #Need to confirm, but these are Care and Treatment-General Population

Bur_SIMSmain1$Required <- NA
Bur_SIMSmain1$TreatmentCEEs <- NA

Bur_SIMSmain1$Required[conditionsite & Bur_SIMSmain1$Tool.Type == "Site"] <- 1

Bur_SIMSmain1$Required[conditionabovesite & Bur_SIMSmain1$Tool.Type == "Above Site"] <- 1

Bur_SIMSmain1$Required[is.na(Bur_SIMSmain1$Required) == TRUE] <- 0

Bur_SIMSmain1$TreatmentCEEs[conditiontreatment] <- 1

Bur_SIMSmain1$TreatmentCEEs[is.na(Bur_SIMSmain1$TreatmentCEEs) == TRUE] <- 0


###Some ways of looking at it: 
#1) "complete" scores from a site, then 1a. maybe adding up percent of greens for the site (and for this, try filtering for required only);
#2) Filter complete scores, and then seeing overall the egregiousness. (percent). can also look at required scores only (or treatment, as Ryan was doing for Burundi) 

Bur_SIMSmain1 <- Bur_SIMSmain1 %>%  mutate(SiteLevelFullScores = 
                   ifelse(FinalAssessmentStatus == "Initial (and only) assessment" | RepeatedCEEs == 1 |
                            (RepeatedCEEs >= 2 & FinalAssessmentStatus == "Follow-up (and last) assessment"), 1, 0))
   ## this status could help us eventually determine Which CEEs can be "deleted" since they have been re-scored in follow-ups. 

Bur_SIMSmain1 <- Bur_SIMSmain1 %>% mutate(NumericalScore = case_when(Score == "Green"  ~ 3,
                                                                     Score == "Yellow"  ~ 2,
                                                                     Score == "Red"  ~ 1)) 

cols.num <- c("Score.for.Green", "Score.for.Red", "Score.for.Red...Yellow", "Score.for.Yellow")
Bur_SIMSmain1[cols.num][Bur_SIMSmain1[cols.num]!=""] <- "1"
Bur_SIMSmain1[cols.num][Bur_SIMSmain1[cols.num]==""] <- "0"
Bur_SIMSmain1[cols.num] <- sapply(Bur_SIMSmain1[cols.num],as.numeric)

SiteLevelFullScores <- dplyr::filter(Bur_SIMSmain1, SiteLevelFullScores == 1)

fwrite(SiteLevelFullScores, file=paste("sitelevelfullscores", ".csv"))

#####Looking at scores at the site-level (i.e. scores per site) #####
Bur_SIMSmain1$Green <- 3

Bur_SIMSall <- data.frame(aggregate(list(TotalScores =  Bur_SIMSmain1$NumericalScore,
                                           Total = Bur_SIMSmain1$Green, Count = Bur_SIMSmain1$X1), by=list(orgUnituid = Bur_SIMSmain1$orgUnituid), FUN = sum))

Bur_SIMSwithoutdups <- data.frame(aggregate(list(TotalScoresnodups =  SiteLevelFullScores$NumericalScore,
                                         Totalnodups = SiteLevelFullScores$Green, 
                                         Countnodups = SiteLevelFullScores$X1), by=list(orgUnituid = 
                                           SiteLevelFullScores$orgUnituid), FUN = sum))


Bur_SIMSRequiredandwithoutdups <- data.frame(aggregate(list(TotalScoresReqnodups =  dplyr::filter(Bur_SIMSmain1, Required == 1 & SiteLevelFullScores == 1)$NumericalScore,
                                                 TotalReqnodups = dplyr::filter(Bur_SIMSmain1, Required == 1 & SiteLevelFullScores == 1)$Green,
                                                 CountReqnodups = dplyr::filter(Bur_SIMSmain1, Required == 1 &  SiteLevelFullScores == 1)$X1), by=list(orgUnituid = 
                                                         dplyr::filter(Bur_SIMSmain1, Required == 1 & SiteLevelFullScores == 1)$orgUnituid), FUN = sum))


Bur_SIMSTreatmentandwithoutdups <- data.frame(aggregate(list(TotalScoresTrtnodups =  dplyr::filter(Bur_SIMSmain1, TreatmentCEEs == 1 & SiteLevelFullScores == 1)$NumericalScore,
                              TotalTrtnodups = dplyr::filter(Bur_SIMSmain1,  TreatmentCEEs == 1 & SiteLevelFullScores == 1)$Green,
                            CountTrtnodups = dplyr::filter(Bur_SIMSmain1,  TreatmentCEEs == 1  &  SiteLevelFullScores == 1)$X1), by=list(orgUnituid = 
                                   dplyr::filter(Bur_SIMSmain1,  TreatmentCEEs == 1  & SiteLevelFullScores == 1)$orgUnituid), FUN = sum))


#Also can add one filtering treatment CEEs only

Bur_SIMSmainalldata = Bur_SIMSmain1[!duplicated(Bur_SIMSmain1$orgUnituid),]

#put all data frames into list
df_list <- list(Bur_SIMSall, Bur_SIMSwithoutdups, Bur_SIMSRequiredandwithoutdups, Bur_SIMSTreatmentandwithoutdups, Bur_SIMSmainalldata)
#merge all data frames in list
allsitesBur <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

allsitesBur$SiteLevelScorenodup <- (allsitesBur$TotalScoresnodups / allsitesBur$Totalnodups)
allsitesBur$SiteLevelScorenTrtnodup <- (allsitesBur$TotalScoresTrtnodups / allsitesBur$TotalTrtnodups)


dodecile = quantile(allsitesBur$SiteLevelScorenodup, c(0:12/12))
allsitesBur$Sitebottododecile <- with(allsitesBur,
                       cut(SiteLevelScorenodup, dodecile,
                           include.lowest = T,
                           labels = c("Lowest Score", "16.6%", "25%", "33.3%", "41.6%", "50%", "58.3%", "66.6%", 
                                      "75%", "83.3%", "91.6%", "Highest score")))

dodecile = quantile(allsitesBur$SiteLevelScorenTrtnodup, c(0:12/12),  na.rm = TRUE)
allsitesBur$SiteTrtbottododecile <- with(allsitesBur,
                                      cut(SiteLevelScorenTrtnodup, dodecile,
                                          include.lowest = T,
                                          labels = c("Lowest Score", "16.6%", "25%", "33.3%", "41.6%", "50%", "58.3%", "66.6%", 
                                                     "75%", "83.3%", "91.6%", "Highest scores")))


 bottomTrt<- filter(allsitesBur, SiteTrtbottododecile == "Lowest Score")  %>% select(c(1,13,16,17,19,20,89,94,133,144,150,154,166)) ##, 18,77,82,132,133,135,154,168))  ## %>% dplyr::rename("County" = County_Description) %>% 
##  dplyr::rename("State" = PART_STATE) %>% 
##  dplyr::rename("Average Estimated Salary" = EstimatedSalary)%>% 
##  dplyr::rename("Index of participant health (between 0 and 1)" = normalized)

#Service.Delivery.Type SNU1.Name Datim.Location Partner.Type Prime.Partner SNUprioritization #4,7,8,
 
 
 #####Looking at scores at the SNU-level#####
 
 Bur_SIMSSNUall <- data.frame(aggregate(list(TotalScores =  Bur_SIMSmain1$NumericalScore,
                                          Total = Bur_SIMSmain1$Green, Count = Bur_SIMSmain1$X1), by=list(SNU1.Name = Bur_SIMSmain1$SNU1.Name), FUN = sum))
 
 Bur_SIMSSNUwithoutdups <- data.frame(aggregate(list(TotalScoresnodups =  dplyr::filter(Bur_SIMSmain1, SiteLevelFullScores == 1)$NumericalScore,
                                                  Totalnodups = dplyr::filter(Bur_SIMSmain1, SiteLevelFullScores == 1)$Green, 
                                                  Countnodups = dplyr::filter(Bur_SIMSmain1, SiteLevelFullScores == 1)$X1), by=list(SNU1.Name = 
                                                     dplyr::filter(Bur_SIMSmain1, SiteLevelFullScores == 1)$SNU1.Name), FUN = sum))
 
 
 Bur_SIMSSNURequiredandwithoutdups <- data.frame(aggregate(list(TotalScoresReqnodups =  dplyr::filter(Bur_SIMSmain1, Required == 1 & SiteLevelFullScores == 1)$NumericalScore,
                                                             TotalReqnodups = dplyr::filter(Bur_SIMSmain1, Required == 1 & SiteLevelFullScores == 1)$Green,
                                                             CountReqnodups = dplyr::filter(Bur_SIMSmain1, Required == 1 &  SiteLevelFullScores == 1)$X1), by=list(SNU1.Name = 
                                                              dplyr::filter(Bur_SIMSmain1, Required == 1 & SiteLevelFullScores == 1)$SNU1.Name), FUN = sum))
 
 
 Bur_SIMSSNUTreatmentandwithoutdups <- data.frame(aggregate(list(TotalScoresTrtnodups =  dplyr::filter(Bur_SIMSmain1, TreatmentCEEs == 1 & SiteLevelFullScores == 1)$NumericalScore,
                                                              TotalTrtnodups = dplyr::filter(Bur_SIMSmain1,  TreatmentCEEs == 1 & SiteLevelFullScores == 1)$Green,
                                                              CountTrtnodups = dplyr::filter(Bur_SIMSmain1,  TreatmentCEEs == 1  &  SiteLevelFullScores == 1)$X1), by=list(SNU1.Name = 
                                                                   dplyr::filter(Bur_SIMSmain1,  TreatmentCEEs == 1  & SiteLevelFullScores == 1)$SNU1.Name), FUN = sum))
 
 #Also can add one filtering treatment CEEs only
 
 Bur_SIMSSNUmainalldata = Bur_SIMSmain1[!duplicated(Bur_SIMSmain1$SNU1.Name),]
 
 #put all data frames into list
 df_list <- list(Bur_SIMSSNUall, Bur_SIMSSNUwithoutdups, Bur_SIMSSNURequiredandwithoutdups, Bur_SIMSSNUTreatmentandwithoutdups, Bur_SIMSSNUmainalldata)
 #merge all data frames in list
 allsitesSNUBur <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
 
 allsitesSNUBur$SiteLevelScorenodup <- (allsitesSNUBur$TotalScoresnodups / allsitesSNUBur$Totalnodups)
 allsitesSNUBur$SiteLevelScorenTrtnodup <- (allsitesSNUBur$TotalScoresTrtnodups / allsitesSNUBur$TotalTrtnodups)
 
 dodecile = quantile(allsitesSNUBur$SiteLevelScorenodup, c(0:4/4))
 allsitesSNUBur$Sitebottododecile <- with(allsitesSNUBur,
                                       cut(SiteLevelScorenodup, dodecile,
                                           include.lowest = T,
                                           labels = c("Lowest Score", "50%", "75%", "Highest score")))
 
 dodecile = quantile(allsitesSNUBur$SiteLevelScorenTrtnodup, c(0:5/5),  na.rm = TRUE)
 allsitesSNUBur$SiteTrtbottododecile <- with(allsitesSNUBur,
                                          cut(SiteLevelScorenTrtnodup, dodecile,
                                              include.lowest = T,
                                              labels = c("Lowest Score", "40%", "60%",
                                                         "80%", "Highest score")))
 
 bottomSNU<- filter(allsitesSNUBur, Sitebottododecile == "Lowest Score")  %>% select(c(1,13,16,17,19,20,89,94,133,144,150,154,166)) ##, 18,77,82,132,133,135,154,168))  ## %>% dplyr::rename("County" = County_Description) %>% 
 
 bottomTrtSNU<- filter(allsitesSNUBur, SiteTrtbottododecile == "Lowest Score")  %>% select(c(1,13,16,17,19,20,89,94,133,144,150,154,166)) ##, 18,77,82,132,133,135,154,168))  ## %>% dplyr::rename("County" = County_Description) %>% 
 
 
 #####Looking at the score level#####

 Bur_SIMSfiltered <-  dplyr::filter(Bur_SIMSmain1, SiteLevelFullScores == 1 & Assessment.FY == "2022") #put in your filter here
 
   Bur_SIMSscoreswithoutdups <- data.frame(aggregate(list(Score.for.Green =  Bur_SIMSfiltered$Score.for.Green,
                                                         Score.for.Red =  Bur_SIMSfiltered$Score.for.Red,
                                                         Score.for.Red...Yellow =  Bur_SIMSfiltered$Score.for.Red...Yellow,
                                                         Score.for.Yellow =  Bur_SIMSfiltered$Score.for.Yellow,
                                                  Countnodups = Bur_SIMSfiltered$X1), by=list(CEE.Number.and.Name = 
                                                  Bur_SIMSfiltered$CEE.Number.and.Name, 
                                               Set.Number = Bur_SIMSfiltered$Set.Number), FUN = sum))
 
 
    Bur_SIMSscoreswithoutdups$PercentRed <- (Bur_SIMSscoreswithoutdups$Score.for.Red / Bur_SIMSscoreswithoutdups$Countnodups)
   
    Bur_SIMSscoreswithoutdupstreatment <- filter(Bur_SIMSscoreswithoutdups, Bur_SIMSscoreswithoutdups$Set.Number == "2A" | Bur_SIMSscoreswithoutdups$Set.Number == "2B")
   
   quantile = quantile(Bur_SIMSscoreswithoutdupstreatment$PercentRed, c(0:2/2))
   Bur_SIMSscoreswithoutdupstreatment$Bur_SIMSscoresquantile <- with(Bur_SIMSscoreswithoutdupstreatment,
                                         cut(PercentRed, quantile,
                                             include.lowest = T,
                                             labels = c("Most green",   "Most red")))
   
   
   quantile = quantile(Bur_SIMSscoreswithoutdups$PercentRed, c(0:2/2))
   Bur_SIMSscoreswithoutdups$Bur_SIMSscoresquantile <- with(Bur_SIMSscoreswithoutdups,
                                                                     cut(PercentRed, quantile,
                                                                         include.lowest = T,
                                                                         labels = c("Most green",   "Most red")))
   
   View(head( Bur_SIMSscoreswithoutdupstreatment[order(Bur_SIMSscoreswithoutdupstreatment$PercentRed, decreasing = TRUE),], n=5))
   
   View(head( Bur_SIMSscoreswithoutdups[order(Bur_SIMSscoreswithoutdups$PercentRed, decreasing = TRUE),], n=10))
  mostegregious <-   head( Bur_SIMSscoreswithoutdups[order(Bur_SIMSscoreswithoutdups$PercentRed, decreasing = TRUE),], n=10)
   
   fwrite(mostegregious, file=paste("mostegregious22", ".csv"))
   