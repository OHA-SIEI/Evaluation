library(here)
setwd(here())
library(readxl)
library(dplyr)
library(lubridate)
library(data.table)
library(fastDummies)
library(tableone)
library(survey)
library(arsenal)
library(openxlsx)
library(tidyverse)

clientdata <-read_xlsx("RCA Combined Data 2_3_2022.xlsx", sheet = "Revised Data")

GBVdata <- read_xlsx("RCA Combined Data 2_3_2022.xlsx", sheet = "GBV Responses")

reasonsreturn <- read_xlsx("RCA Combined Data 2_3_2022_Analysed Sept 20 2022.xlsx", sheet = "Reasons for Returning to Care")
reasonsreturn$"Client Clinical ID" <- reasonsreturn$"Client Cinical ID"
cols.num <- c("Social Support- family-friends-community- work colleagues/Received Counselling/ Healthcare Provider Support", 
              "Migration (leisure, personal, work) & Travel",
              "Ready to take Treatment /Wanted to get/stay Healthy/Guilty for not Taking Treatment/Knows Importance of Treatment/Continuing Treatment/Needed Treatment (ran out)"
              )
reasonsreturn[cols.num] <-sapply(reasonsreturn[cols.num],as.numeric)
GBVdata$"Client Clinical ID" <- GBVdata$"Clinical Client ID"
clientdata$`Date of HIV+ Diagnosis` <- convertToDate(clientdata$`Date of HIV+ Diagnosis`, origin = "1900-01-01")
clientdata$`Date of HIV+ Diagnosis`[clientdata$`Date of HIV+ Diagnosis` == "2024-01-20"] <- "2014-01-20"
clientdata$`Date ART Initiation` <- convertToDate(clientdata$`Date ART Initiation`, origin = "1900-01-01")

clientdata$TimesinceDiagnosis <-  as.numeric(difftime(Sys.Date(), clientdata$`Date of HIV+ Diagnosis`, unit = "weeks")) / 52.25  
   
allclientdata_ <- merge(x=clientdata, y=GBVdata, by=c("Client Clinical ID", "SNU", "Facility"), all.x = TRUE) 
allclientdata <- merge(x=allclientdata_, y=reasonsreturn, by=c("Client Clinical ID", "SNU", "Facility"), all.x = TRUE) 

#allclientdata$Responded[is.na(allclientdata$"Main reason") == FALSE] <- 1

#allclientdata <- allclientdata %>% mutate(Responded = ifelse(is.na(allclientdata$"Long Travel") == TRUE, 0, 1)) ##need to do this later

allclientdata$`No reason for RTC`[is.na(allclientdata$"Response (List all responses client provides)") == FALSE &
                                    is.na(allclientdata$"Response") == TRUE] <- 1

allclientdata <- allclientdata %>% mutate(ExperiencedGBV = ifelse(is.na(PSNU.y) == TRUE, 0, 1))

allclientdata <- allclientdata %>% mutate(Employment.Status2 = ifelse(allclientdata$"Employment Status" == "Retired" |
             allclientdata$"Employment Status" == "0", "Retired/other", allclientdata$"Employment Status"))

allclientdata$`Main reason`[allclientdata$"Response (List all responses client provides)" == "No problem"] <- "No problem listed"
allclientdata$`Client knows viral load status?`[allclientdata$"Client knows viral load status?" == "0"] <- NA
allclientdata$`Key Population`[allclientdata$"Key Population" == "0"] <- NA


allclientdata$`Main reason` <- toupper(allclientdata$`Main reason`)
allclientdata$`MMD?` <- toupper(allclientdata$`MMD?`)

allclientdata<-  fastDummies::dummy_cols(allclientdata, select_columns = c("Main reason", "Sex", "MMD?", "Key Population", "Client knows viral load status?",
                        "Employment.Status2", "Education"), ignore_na = TRUE)

allclientdata <- allclientdata %>% dplyr::rename_all(list(~make.names(.)))

allclientdata <- allclientdata %>% mutate(Age_Band = case_when(`Age..In.Years.` < 20 ~ "<20",
              `Age..In.Years.` >= 20 & `Age..In.Years.` <= 29 ~ "20-29",
              `Age..In.Years.` >= 30 & `Age..In.Years.` <= 39 ~ "30-39",
              `Age..In.Years.` >= 40 & `Age..In.Years.` <= 49 ~ "40-49",
              `Age..In.Years.` >= 50 ~ "50+")) # & `Age..In.Years.` <= 59

allclientdata %>%
  select(MMD._YES, ExperiencedGBV, TimesinceDiagnosis, Client.knows.viral.load.status._Yes, Key.Population_FSW, Employment.Status2_Employed.for.wages,
         Employment.Status2_Employed.for.wages, , Employment.Status2_Homemaker, Employment.Status2_Out.of.work, Employment.Status2_Retired.other,
         Education_No.formal.education, Education_Primary, Age..In.Years.) %>% # select variables to summarise
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")


names<-c("Main.reason_BELIEFS", "Main.reason_GENDER.BASED.VIOLENCE", "Main.reason_HIGH.COST", "Main.reason_JAILED", "Main.reason_LONG.TRAVEL",                                                                                             
 "Main.reason_MEMORY", "Main.reason_MENTAL.HEALTH", "Main.reason_MIGRATION...TRAVEL", "Main.reason_MOTHERHOOD", "Main.reason_NO.PROBLEM.LISTED",                                                                                       
          "Main.reason_NO TIME", "Main.reason_NO..LITTLE.SOCIAL.SUPPORT", "Main.reason_NUTRITION", "Main.reason_PRIVACY...STAFF.ISSUES.AT.CENTER",                                                                       
          "Main.reason_SOCIAL.CIRCUMSTANCES", "Main.reason_STIGMA...DISCRIMINATION",   "Main.reason_SUPPLY...SERVICE.ISSUES.AT.CENTER",                                                                   
        "Main.reason_TREATMENT...HEALTH.ISSUES", "ExperiencedGBV")

allclientdata$weight <- 1
allclientdatafortable <- survey::svydesign(ids = ~ 1, weights=allclientdata$weight, data=allclientdata)
table <- svyCreateTableOne(vars = names, strata = "Age_Band", data = allclientdatafortable, test = TRUE)
summary(table, Digits = 3)


table <- tableby(Age_Band ~ Main.reason_BELIEFS + Main.reason_GENDER.BASED.VIOLENCE + Main.reason_HIGH.COST + Main.reason_JAILED + Main.reason_LONG.TRAVEL  + 
 Main.reason_MEMORY + Main.reason_MENTAL.HEALTH + Main.reason_MIGRATION...TRAVEL + Main.reason_MOTHERHOOD + Main.reason_NO.PROBLEM.LISTED  + 
Main.reason_NO.TIME + Main.reason_NO..LITTLE.SOCIAL.SUPPORT + Main.reason_NUTRITION + Main.reason_PRIVACY...STAFF.ISSUES.AT.CENTER + 
          Main.reason_SOCIAL.CIRCUMSTANCES + Main.reason_STIGMA...DISCRIMINATION + Main.reason_SUPPLY...SERVICE.ISSUES.AT.CENTER + 
        Main.reason_TREATMENT...HEALTH.ISSUES + ExperiencedGBV , data = allclientdata,
control=tableby.control(total=TRUE, cat.simplify=TRUE,    cat.stats=c("Nmiss","countpct"),digits=2))

summary(table, title="Age", text=TRUE)

table <- tableby(Sex ~ Main.reason_BELIEFS + Main.reason_GENDER.BASED.VIOLENCE + Main.reason_HIGH.COST + Main.reason_JAILED + Main.reason_LONG.TRAVEL  + 
                   Main.reason_MEMORY + Main.reason_MENTAL.HEALTH + Main.reason_MIGRATION...TRAVEL + Main.reason_MOTHERHOOD + Main.reason_NO.PROBLEM.LISTED  + 
                   Main.reason_NO.TIME + Main.reason_NO..LITTLE.SOCIAL.SUPPORT + Main.reason_NUTRITION + Main.reason_PRIVACY...STAFF.ISSUES.AT.CENTER + 
                   Main.reason_SOCIAL.CIRCUMSTANCES + Main.reason_STIGMA...DISCRIMINATION + Main.reason_SUPPLY...SERVICE.ISSUES.AT.CENTER + 
                   Main.reason_TREATMENT...HEALTH.ISSUES + ExperiencedGBV , data = allclientdata,
                 control=tableby.control(total=TRUE, cat.simplify=TRUE,    cat.stats=c("Nmiss","countpct"),digits=2))

summary(table, title="Gender", text=TRUE)

table <- tableby(ExperiencedGBV ~ Age..In.Years. + Sex_F + MMD._YES + Key.Population_FSW + Client.knows.viral.load.status._Yes +
                   Employment.Status2_Homemaker + Employment.Status2_Out.of.work + Employment.Status2_Employed.for.wages +
                   Employment.Status2_Self.Employed + Education_No.formal.education + Education_Primary, data = allclientdata,
                 control=tableby.control(total=TRUE, cat.simplify=TRUE,    cat.stats=c("Nmiss","countpct"),digits=2))
summary(table, title="GBV", text=TRUE)


# totalcount <- as.data.frame(aggregate(list(TotalSocialSupport =  "Social.Support..family.friends.community..work.colleagues.Received.Counselling..Healthcare.Provider.Support",
#                                         TotalHealthReasons = "Starting.to.Feel.Unhealthy.Sick.Unwell.Non.Adherence",
#                                         Time = "Time", MentalHealth = "Mental.Health.Issues",
#                                         Migration = "Migration..leisure..personal..work....Travel",
#                                         Transportation = "Transporation",   Cost = "Cost",
#                                         MiscellaneousReady = "Ready.to.take.Treatment..Wanted.to.get.stay.Healthy.Guilty.for.not.Taking.Treatment.Knows.Importance.of.Treatment.Continuing.Treatment.Needed.Treatment..ran.out.",
#                                         Stigma =  "Stigma.Discrimination", Incarceration = "Released.from.Incarceration",
#                                         Sociopolitical = "Socio..Political.Circumstances", NoreasonRTC = "No.reason.for.RTC", 
#                                         data = allclientdata, by=list(Age_Band = "Age..In.Years"), FUN = sum, na.rm=TRUE)))

totalcount = aggregate(allclientdata[,75:87], by=list(allclientdata$Age_Band, allclientdata$Sex), FUN=sum, na.rm=TRUE)

fwrite(totalcount, file=paste("returntocare", ".csv"))


# totalcount <- data.frame(aggregate(list(TotalSocialSupport =  allclientdata$Social.Support..family.friends.community..work.colleagues.Received.Counselling..Healthcare.Provider.Support,
#                   TotalHealthReasons = allclientdata$Starting.to.Feel.Unhealthy.Sick.Unwell.Non.Adherence,
#                   Time = allclientdata$Time, MentalHealth = allclientdata$Mental.Health.Issues,
#                   Migration = allclientdata$Migration..leisure..personal..work....Travel,
#                   Transportation = allclientdata$Transporation,   Cost = allclientdata$Cost,
#                   MiscellaneousReady = allclientdata$Ready.to.take.Treatment..Wanted.to.get.stay.Healthy.Guilty.for.not.Taking.Treatment.Knows.Importance.of.Treatment.Continuing.Treatment.Needed.Treatment..ran.out.,
#                   Stigma =  allclientdata$Stigma.Discrimination, Incarceration = allclientdata$Released.from.Incarceration,
#                   Sociopolitical = allclientdata$Socio..Political.Circumstances, NoreasonRTC = allclientdata$No.reason.for.RTC, by=list(Age_Band = 
#                    allclientdata$Age_Band), FUN = sum)))

