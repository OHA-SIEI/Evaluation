###Run this first to obtain clean datasets

## Changed program to only keep 2020 data as to large to merge with all years.  500K records for 2020 with Q1 and 2
## but 3M records for 2019 Q1-Q3!! sims_mer_combined FY2020 (Q1 and 2) is 27984, but for 2019 (Q2-4)  is 400K!

#  These lines below create a link to tableau, but Avast doesn't like it as could be a threat.
# install.packages("Rserve")
# library(Rserve)
# Rserve()  

#  Need to expand this to include all countries and maybe an additional year or two.
## but MER and SIMS data is imported on a country by country basis.
## will need to replicate this for multiple countries.

##  Need to set it up to run on countries  ##


rm(list=ls())

library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
#library(tidylog)

setwd("C:/Users/adevlin/Documents/R/SIMS_MER_ER")

## select the years want   ##
## Two years of data is a lot - need to filter down more

years<-c(2018, 2019, 2020, 2021)
indicator_list<-c('TX_CURR', 'TX_NEW', 'HTS_TST', 'TX_PVLS', 'HTS_TST_POS')  # Only keep these indicators

## select the country files to import
## countries_to_import <-c('Nigeria', 'Kenya' )  # for the future if expland to other countries



##Load & Clean MER data (from Panorama - Site_IM extract by country) 
mer_raw <- read_tsv('./Raw Data/MER_Structured_Datasets_Site_IM_FY18-20_20200626_v2_1_Nigeria.txt',
                    col_types = cols(targets = col_double(),
                                     otherdisaggregate = col_character(),
                                     qtr1 = col_double(),
                                     qtr3 = col_double(),
                                     hiv_treatment_status = col_character(),
                                     statustb = col_character(),
                                     otherdisaggregate_sub = col_character(),
                                     modality = col_character()))


# Modified to keep total denominator or total numerator for PVLS data
# Only keeping 5 MER indicators

mer_clean <- mer_raw%>%
  clean_names()%>%
  select(!c(countryname, facilityprioritization, communityprioritization, dreams, 
            award_number, pre_rgnlztn_hq_mech_code, typemilitary,
            categoryoptioncomboname, source_name, statuscx, statushiv, statustb, hiv_treatment_status,
            snuprioritization))%>%
  mutate(sitename = str_to_title(sitename),
         facility = str_to_title(facility),
         primepartner = str_to_title(primepartner),
         mech_name = str_to_title(mech_name)) %>%
  mutate(site_type_cleaned = ifelse(sitetype == 'Facility', 'Facility',
                                    ifelse(sitetype == 'Community', 'Community', 'Other'))) %>%
  filter(indicator %in% indicator_list )%>%
         
  # only keep selected indicators in list above.
  filter(standardizeddisaggregate == 'Total Numerator' | standardizeddisaggregate == 'Total Denominator' ) %>%
  pivot_longer(cols = c('qtr1':'qtr4'), names_to = 'quarter', values_to = 'quarter_results') %>%
  remove_empty(which = 'cols', quiet = FALSE) %>%
  arrange(orgunituid)

##Load & Clean SIMS sheets data
## Use csv input in case the read_excel command doesn't work 
#sims_results <- read.csv('./Raw Data/SIMS CEE Results page.csv', 
#                        , col_names = TRUE) %>%

## Below uses read_excel
sims_results <- read_excel('./Raw Data/SIMS_Structured_Datasets_Site_IM_FY19-21_20210514_v1_1_Nigeria.xlsx',
                           sheet = 'cee_results', col_names = TRUE) %>%
  clean_names() %>%
  select(!c(organisation_hst_srgt, mechanism_srgt, period_srgt_key)) %>%
#  Keep or filter out sites with assessments done.
    filter(cee_number_of_asmt >= 1) %>%
  
    mutate(cee_score_value = as.numeric(cee_score_value),
         quarter = str_c('qtr', fiscal_quarter)) %>%
  filter(!is.na(cee_score_value)) %>%
   remove_empty(which = 'cols', quiet = FALSE)

sims_cee_elements <- read_excel('./Raw Data/SIMS_Structured_Datasets_Site_IM_FY19-21_20210514_v1_1_Nigeria.xlsx',
                                sheet = 'sims_dataelements', col_names = TRUE) %>%
  clean_names() %>%
  select(c(31:80), sims_univ_data_element_srgt, sims_set_id, sims_set_name, sims_tool_type_name, cee_id,
         cee_name, cee_activity_type, cee_required, cee_question_label) %>%
  pivot_longer(cols = c(1:50), names_to = 'indicator', values_to = 'present') %>%
  mutate(indicator = str_to_upper(indicator)) %>%
  filter(indicator %in% indicator_list )%>%

## sims_cee_elements2<-sims_cee_elements %>%
##  mutate(HTS_TST_POS=case_when(indicator=="HTS_TST" & present=="Y" ~ 1, TRUE ~ "NA")) %>%

  ## HTS_TST_POS not in SIMS - only in MER
  
  #       == 'TX_CURR' | indicator == 'TX_NEW' | indicator == 'HTS_TST' | indicator == 'HTS_INDEX' |
  #        indicator == 'HTS_TST_POS' | indicator == 'TX_PVLS') %>%
  remove_empty(which = 'cols', quiet = FALSE)


sims_mech <- read_excel('./Raw Data/SIMS_Structured_Datasets_Site_IM_FY19-21_20210514_v1_1_Nigeria.xlsx',
                        sheet = 'mech', col_names = TRUE) %>%
  clean_names() %>%
  select(!c(mechanism_id, pre_rgnlztn_hq_mech_code, award_number)) %>%
  mutate(mech_code = as.numeric(mech_code),
         mech_name = str_to_title(mech_name),
         prime_partner = str_to_title(prime_partner)) %>%
  rename(primepartner = prime_partner, 
         fundingagency = funding_agency)%>%
  remove_empty(which = 'cols', quiet = FALSE)


sims_org <- read_excel('./Raw Data/SIMS_Structured_Datasets_Site_IM_FY19-21_20210514_v1_1_Nigeria.xlsx',
                       sheet = 'org', col_names = TRUE) %>%
  clean_names() %>%
  select(!c(region_uid, region, operating_unit_uid, operating_unit, country_name,
            facility_prioritization, dreams, level,
            community_prioritization, type_military, sn_uprioritization)) %>%
  mutate(site_type_cleaned = ifelse(site_type == 'Facility', 'Facility',
                           ifelse(site_type == 'Community', 'Community', 'Other'))) %>%
  mutate(facility = str_to_title(facility)) %>%
  rename(orgunituid = org_unituid,
         psnuuid = psn_uuid,
         snu1uid = snu1uid,
         communityuid = community_uid, 
         facilityuid = facility_uid) %>%
  remove_empty(which = 'cols', quiet = FALSE)


sims_asmt <- read_excel('./Raw Data/SIMS_Structured_Datasets_Site_IM_FY19-21_20210514_v1_1_Nigeria.xlsx',
                       sheet = 'asmt_coversheet', col_names = TRUE) %>%
  clean_names() %>%
  filter(asmt_tool_type == 'Site') %>%
  select(c(asmt_type, asmt_tool_type, asmt_date, sims4_cover_sheet_srgt, asmt_prtd_new_partner, asmt_prtd_new_site, 
           asmt_prtd_scaling_activity, asmt_prtd_other_evidence_or_known_gap, asmt_prtd_set, asmt_id)) %>%
  remove_empty(which = 'cols', quiet = FALSE)


##Combining SIMS sheets together
sims_joined <- sims_results %>%
  inner_join(sims_mech, by = 'mechanism_hst_srgt') %>%
  inner_join(sims_cee_elements, by = 'sims_univ_data_element_srgt') %>%
  inner_join(sims_org, by = 'organisation_srgt') %>%
  inner_join(sims_asmt, by = 'sims4_cover_sheet_srgt') %>%
  arrange(orgunituid)


#Export individual datasets
write.csv(sims_joined, './Cleaned Data/20201015_Nigeria_SIMS_FY19FY21.csv', row.names = FALSE)
write.csv(mer_clean, './Cleaned Data/20201015_Nigeria_MER_FY19FY20.csv', row.names = FALSE)

## remove missing data (asmt_prtd scaling activity, prtd_new_site) - these don't reduce the numbers of observations
## remove indicator type and disaggregate (Keep standardized disaggregate)

## inner_join will only keep results that are USAID funded as SIMS has no other funding agency data   
## right join (based on MER will keep all funding agencies and HTS_TST_POS indicator - MUCh larger dataset)

###Combining SIMS and MER Datasets - drop variables that aren't use to reduce size of merge file
sims_mer_combinedL <- sims_joined %>%
  inner_join(mer_clean, suffix = c('_sims', '_mer')) %>%
  select(!c(disaggregate, indicatortype, asmt_prtd_scaling_activity, asmt_prtd_new_site,
            sims4_cover_sheet_srgt)) %>%
  filter(fiscal_year %in% years) 

###Combining SIMS and MER - based on MER so that HTS_POS comes through and other funding agencies.  575K observations
sims_mer_combined <- sims_joined %>%
  right_join(mer_clean, suffix = c('_sims', '_mer')) %>%
  select(!c(disaggregate, indicatortype, asmt_prtd_scaling_activity, asmt_prtd_new_site,
            sims4_cover_sheet_srgt, organisation_srgt, mechanism_hst_srgt, sims_univ_data_element_srgt)) %>%
  filter(fiscal_year %in% years) 
## extra three variables in this merge doesn't reduce number of obs - only variable count.

# excluding srgt variables makes no difference in size reduction
#  organisation_srgt, mechanism_hst_srgt, sims_univ_data_element_srgt

#  HTS_TST_POS doesn't come across into SIMS_MER_COMBINED with inner join as it doesn't exist in SIMS.

sims_mer_combined <- sims_mer_combined %>%
  dplyr::mutate(mech_name = toupper(mech_name),
                primepartner = toupper(primepartner))
  ## make merge variables with ER data all upper case
  
###Export Combine Dataset
write.csv(sims_mer_combined, './Cleaned Data/20201015_Nigeria MER_SIMS Triangulation 2020.csv', row.names = FALSE)
# write.csv(sims_mer_combined, './Cleaned Data/Nigeria SIMS MER.csv', row.names = FALSE)


# Only includes those where there were SIMS assessments - see filter above.

## End of SIMS/MER Merge
## ********************************************************************************************************
##  ******************* Load ER data

##Load and Clean ER Data for Nigeria -reading in a tsv file doesn't read in the exp amounts!
er_raw <- read_tsv('./Raw Data/Financial_Structured_Dataset_COP17-20 APR 2021.txt' ,
      col_types = cols(mech_code = col_double(),
                       implementation_year = col_double(),
                       cop_budget_new_funding = col_double(),
                       cop_budget_pipeline = col_double(),
                       cop_budget_total = col_double(),
                       workplan_budget_amt = col_double(),
                       expenditure_amt = col_double()))

## er_raw <- read.csv('./Raw Data/Financial_Structured_Dataset_COP17-20 APR 2021.csv')

er_clean <- er_raw %>%
 
  dplyr::select( - c('award_number', 'subrecipient_duns', 'subrecipient_name', 
                     'interaction_type', 'sub_program')) %>% 
  dplyr::rename("Country"= country,
                "Funding Agency"= fundingagency,
                "Program Area"= program,
                "Beneficiary" = beneficiary,
                "Sub Beneficiary"= sub_beneficiary,
                "Fiscal Year" = implementation_year,
                "COP Budget New Funding"=cop_budget_new_funding,
                "COP Budget Pipeline"=cop_budget_pipeline,
                "Total Planned Funding" = cop_budget_total,
                "Workplan Budget" = workplan_budget_amt,
                "Expenditure"=expenditure_amt,
                "Prime Partner Type"=prime_partner_org_type,
                "Is Indigenous Prime Partner"=is_indigenous_prime_partner)
  
  er_clean<-er_clean%>%
  mutate_at(vars(`COP Budget New Funding`:`Expenditure`),~replace_na(.,0))%>%
    mutate(fiscal_year= as.numeric(`Fiscal Year`)) 
  
## Need to remove some duplicates....  
  ## some mech_code don't map 1:1 with mech names.  eg  Henry Jackson Foundation maps to 70255 AND 17747
  # management and operations linked to FOUR mech_codes
  ## In er_clean for 2 quarters 2020 36 unique mech_codes but 32 unique mech_names!
  #  a few partners are linked to several mech_codes
  # eg chemonics linked to mech codes 18442 18655 81857 81859
  # Family Health international linked to two mech codes -. will cause inflation.
  ## Mech_name for Nigeria is Henry Jackson Foundation!!
  
#  Mechanism names can repeat as implementing mechanisms have multiple sources for funding different activities 
# (i.e. Henry Jackson Foundation in Nigeria  has funds for a C&T program activities (mech code 1747) 
# and a larger portfolio of above site, PREV and HTS activities (mech code 70255). The different mechanisms may 
# also run on different timelines of implementation. There may also be repeats of 'Management and Operations' as 
# mech names ->  exclude from our financial analysis. There may also be issues in the larger dataset with mech names 
# being misspelled and therefore counted as distinct when they are not (see link below). 
  # https://docs.google.com/spreadsheets/d/1ojsFHffKENy2v-nJxReOizVESnMHSDuPcjzGc9yBjBE/edit#gid=0


# The reason SIMS/MER datasets don't have more than one mech code linked to a prime partner is because they are 
# not reporting an applicable indicator for that mechanism (i.e. above site partners with no targets, or non-clinical
# IPs that don't have SIMS visits) whereas the ER data is setup to measure each cost across program areas 
# that a mechanism may cover.   
  
#convert budget columns to numeric  (filter to just data in years variable - ie 2020 data)
er_cleanbyyear<-er_clean%>%
  dplyr::mutate(`Fiscal Year`= as.numeric(`Fiscal Year`))%>%
  dplyr::mutate(`COP Budget New Funding`=as.numeric(`COP Budget New Funding`))%>%
  dplyr::mutate(`COP Budget Pipeline`=as.numeric(`COP Budget Pipeline`))%>%
  dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`))%>%
  dplyr::mutate(`Workplan Budget`=as.numeric(`Workplan Budget`))%>%
  dplyr::mutate(`Expenditure`=as.numeric(`Expenditure`)) %>%
  filter(fiscal_year %in% years)  %>%
  filter(Country %in% c("Nigeria"))
  
## workplan budget is what was actually given - no data for FY20
# Can delete this variable as not needed

 ## Make merge variables all upper case 
er_cleanbyyear <- er_cleanbyyear%>%
  clean_names() %>%  # removes spaces and puts _ in the names
  dplyr::mutate(primepartner = str_to_upper(prime_partner_name)) %>%  
  dplyr::mutate(mech_name = str_to_upper(mech_name))

## need to amend    ##  COP budget new funding ==  total planned funding

# dropped the following as don't need
#    "Interaction Type"= interaction_type,
#      "Subrecipient Name"= subrecipient_name,
#      "Cost Category"= cost_category,
#      "Sub Cost Category" =sub_cost_category,
#   "Sub Program Area" = sub_program,

#  Need to aggregate categories before deleting!  eg mech_code 18675 associated with different expenditures depending on
## sub recipients!

er_clean_amended <- er_cleanbyyear %>%
  group_by(operatingunit, fiscal_year , funding_agency, primepartner, mech_code, mech_name, program_area,
           is_indigenous_prime_partner, beneficiary, record_type) %>%
  summarise(across(c(expenditure, cop_budget_new_funding,total_planned_funding, 
                     cop_budget_pipeline, workplan_budget),
                   sum,
                   na.rm = TRUE)) %>%
  ungroup()

## Remove cost_category as not needed.

# er_clean_amended <- er_clean_amended%>%
#  dplyr::select( - c('cost_category'))

# removal duplicated rows. - should be none.
er_clean_final<-er_clean_amended[!duplicated(er_clean_amended), ]

write.csv(er_clean_final, "cleaned 2019-2021 ER data for Nigeria.csv")


#  Need expenditure unit cost.
# expenditure/ tx_curr


## End of Expenditure data cleaning
##  ***************************************************************************************


#  Merge MER with ER data for the mechanism spend graph - first need to capitalise all and limit years
## Keep quarterly data as need them for targets even though ER data only annual

mer_cleaned <- mer_clean %>%
  dplyr::mutate(mech_name = toupper(mech_name),
                primepartner = toupper(primepartner)) %>%
  filter(fiscal_year %in% years)  %>%
  select(!c(disaggregate, indicatortype, numeratordenom)) %>%
  rename(funding_agency = fundingagency)
## make merge variables with ER data all upper case

## remove duplications now that deleted some variables
mer_cleaned<-mer_cleaned[!duplicated(mer_cleaned), ]


### 62421 records for 2020.  210 700 for MER_ER 2019 and 2020 - 5 quarters

MER_ER_combined <- mer_cleaned %>%
  inner_join(er_clean_final, suffix = c('_mer', '_er'))

MER_ER_combined <-MER_ER_combined %>%
  mutate( exp_person=expenditure/cumulative)  

#expenditure per person for total numbers at that point per year.

###Export Combined MER/ER Dataset
write.csv(MER_ER_combined, './Cleaned Data/2019 2020 Nigeria MER_ER.csv', row.names = FALSE)


################  Merge SIMS/MER and ER

all_data_combined <- sims_mer_combined %>%
  inner_join(er_clean_final, suffix = c('_sims_mer', '_er'))

all_data_combined<-all_data_combined %>%
  mutate( exp_person=expenditure/cumulative)    
#expenditure per person for total numbers at that point per year.

## remove duplications now that deleted some variables - not needed as no duplications
# all_data_combineda <-all_data_combined[!duplicated(all_data_combined), ]

#  large file to export to EXCEL - 1.04M is the EXCEL limit.   119608 observations - JUST NIGERIA AND one year!
# How will cope with all countries for one year?  3.23 million for Nigeria for two years (5 quarters)! 
# or 837K limiting it to just sites with SIMS assessments

write.csv(all_data_combined, './Cleaned Data/2019 2020 Nigeria SIMS_MER_ER.csv', row.names = FALSE)

###   End of merging SIMS/MER and ER - LARGE dataset.  